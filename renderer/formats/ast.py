#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to asterix specs format."""

import json
from formats.common import getNumber, renderRule, case

accumulator = []
indentLevel = 0

class Indent(object):
    """Simple indent context manager."""
    def __enter__(self):
        global indentLevel
        indentLevel += 1
    def __exit__(self, exec_type, exec_val, exec_tb):
        global indentLevel
        indentLevel -= 1

indent = Indent()

def tell(s):
    s = ' '*indentLevel*4 + s
    accumulator.append(s.rstrip())

def render(s):
    """Rendering entry point"""
    root = json.loads(s)
    renderHeader(root)
    tell('items')
    tell('')
    with indent:
        [renderItem(item) for item in root['catalogue']]
    renderUap(root['uap'])

    return ''.join([line+'\n' for line in accumulator])

def renderHeader(root):
    tell('asterix {:03d} "{}"'.format(root['number'], root['title']))
    edition = root['edition']
    tell('edition {}.{}'.format(edition['major'], edition['minor']))
    date = root['date']
    tell('date {}-{:02d}-{:02d}'.format(date['year'], date['month'], date['day']))
    tell('preamble')
    with indent:
        [tell(i) for i in root['preamble'].splitlines()]
    tell('')

def renderItem(item):
    subitem = item['subitem']
    tell('{} "{}"'.format(subitem['name'], subitem['title']))
    with indent:
        def case0(): tell('unspecified')
        def case1(enc): tell(enc['rule'])
        def case2(enc):
            tell('case {}'.format('/'.join(enc['name'])))
            with indent:
                [tell('{}: {}'.format(a,b)) for (a,b) in enc['rules']]
        renderRule(item['encoding'], case0, case1, case2)

        tell('definition')
        with indent:
            [tell(i) for i in item['definition'].strip().splitlines()]

        renderSubitem(subitem['element'])

        if subitem['remark']:
            tell('remark')
            with indent:
                [tell(i) for i in subitem['remark'].strip().splitlines()]
    tell('')

def constrainToString(constrain):
    return '{} {}'.format(constrain['type'], str(getNumber(constrain['value'])))

def renderSubitem(element):
    def renderInteger(value):
        sig = 'signed' if value['signed'] else 'unsigned'
        const = ' '.join([constrainToString(const) for const in value['constraints']])
        const = (' ' + const) if const else ''
        tell('{} integer'.format(sig) + const)

    def renderQuantity(value):
        sig = 'signed' if value['signed'] else 'unsigned'
        const = ' '.join([constrainToString(const) for const in value['constraints']])
        const = (' ' + const) if const else ''
        k = getNumber(value['scaling'])
        fract = value['fractionalBits']
        unit = value['unit']
        tell('{} quantity {} {} "{}"'.format(sig, k, fract, unit) + const)

    def renderFixed():
        n = element['size']
        tell('fixed {}'.format(n))

        def case0():
            tell ('raw')
            return n

        def case1(val):
            value = val['rule']
            t = value['type']
            if t == 'Table':
                tell('table')
                with indent:
                    [tell('{}: {}'.format(key, value)) for key,value in value['values']]
            elif t == 'String':
                var = case('string variation', value['variation'],
                    ('StringAscii', 'ascii'),
                    ('StringICAO', 'icao'),
                    )
                tell('string {}'.format(var))
            elif t == 'Integer':
                renderInteger(value)
            elif t == 'Quantity':
                renderQuantity(value)
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def case2(val):
            tell('case {}'.format('/'.join(val['name'])))
            with indent:
                for (a,b) in val['rules']:
                    tell('{}:'.format(a))
                    with indent:
                        case1({'rule': b})
            return n

        with indent:
            return renderRule(element['content'], case0, case1, case2)

    def renderMaybeSubitem(subitem):
        if subitem['spare']:
            n = subitem['length']
            tell('spare {}'.format(n))
            return n
        tit = subitem['title']
        tell('{} "{}"'.format(subitem['name'], tit))
        if subitem['description']:
            with indent:
                tell('description')
                with indent:
                    [tell(i) for i in subitem['description'].strip().splitlines()]
        with indent:
            n = renderSubitem(subitem['element'])
            if subitem['remark']:
                tell('remark')
                with indent:
                    [tell(i) for i in subitem['remark'].strip().splitlines()]
        return n

    def renderGroup():
        tell('subitems')
        n = 0
        with indent:
            for subitem in element['subitems']:
                n += renderMaybeSubitem(subitem)
        return n

    def renderExtended():
        tell('extended {} {}'.format(element['first'], element['extents']))
        n = 0
        with indent:
            for subitem in element['subitems']:
                n += renderMaybeSubitem(subitem)
        return n

    def renderRepetitive():
        tell('repetitive {}'.format(element['rep']))
        with indent:
            return renderSubitem(element['element'])

    def renderExplicit():
        tell('explicit')
        return 0

    def renderCompound():
        tell('compound')
        n = 0
        with indent:
            for subitem in element['subitems']:
                if subitem is None:
                    tell('-')
                else:
                    n += renderMaybeSubitem(subitem)
        return n

    return locals()['render'+element['type']]()

def renderUap(uap):
    def single(items):
        [tell(i if i is not None else '-') for i in items]

    t = uap['type']
    if t == 'uap':
        tell('uap')
        with indent:
            single(uap['items'])

    elif t == 'uaps':
        tell('uaps')
        with indent:
            for var in uap['variations']:
                tell(var['name'])
                with indent:
                    single(var['items'])
    else:
        raise Exception('unexpected uap type {}'.format(t))

