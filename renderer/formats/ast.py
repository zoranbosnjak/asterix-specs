#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to asterix specs format."""

from formats.common import getNumber, renderRule

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

def render(root):
    """Rendering entry point"""
    renderHeader(root)
    tell('items')
    tell('')
    with indent:
        [renderToplevel(item) for item in root['items']]
    renderUap(root['uap'])

    return ''.join([line+'\n' for line in accumulator])

def renderHeader(root):
    tell('category {} "{}"'.format(root['category'], root['title']))
    tell('edition {}'.format(root['edition']))
    tell('date {}'.format(root['date']))
    tell('preamble')
    with indent:
        [tell(i) for i in root['preamble'].splitlines()]
    tell('')

def renderToplevel(toplevel):
    item = toplevel['item']
    tell('{} "{}"'.format(item['name'], item['title']))
    with indent:
        def case1(enc): tell(enc['rule'])
        def case2(enc):
            tell('case {}'.format('/'.join(enc['item'])))
            with indent:
                [tell('{}: {}'.format(a,b)) for (a,b) in enc['rules']]
        renderRule(toplevel['encoding'], case1, case2)

        tell('definition')
        with indent:
            [tell(i) for i in toplevel['definition'].strip().splitlines()]

        renderItem(item['variation'])

        if item['remark']:
            tell('remark')
            with indent:
                [tell(i) for i in item['remark'].strip().splitlines()]
    tell('')

def renderItem(variation):
    def renderQuantity(signed, q):
        k = getNumber(q['scaling'], 1)
        fract = q['fractionalBits']
        unit = q.get('unit')

        tell('{}'.format('signed' if signed else 'unsigned'))

        if k:
            tell('scale {}'.format(k))
        if fract != 0:
            tell('fractional {}'.format(fract))
        if unit is not None:
            tell('unit "{}"'.format(unit))

        lim1 = q['lowLimit']
        lim2 = q['highLimit']

        if lim1 is not None:
            operator = 'ge' if lim1['including'] else 'gt'
            lim = getNumber(lim1['limit'])
            tell('{} {}'.format(operator, lim))

        if lim2 is not None:
            operator = 'le' if lim2['including'] else 'lt'
            lim = getNumber(lim2['limit'])
            tell('{} {}'.format(operator, lim))

    def renderFixed():
        n = variation['size']
        tell('item {}'.format(n))

        def case1(val):
            value = val['rule']
            t = value['type']
            if t == 'Raw':
                tell('raw')
            elif t == 'Unsigned':
                renderQuantity(False, value['quantity'])
            elif t == 'Signed':
                renderQuantity(True, value['quantity'])
            elif t == 'Table':
                tell('discrete')
                with indent:
                    [tell('{}: {}'.format(key, value)) for key,value in value['values']]
            elif t == 'StringAscii':
                tell('string ascii')
            elif t == 'StringICAO':
                tell('string icao')
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def case2(val):
            tell('case {}'.format('/'.join(val['item'])))
            with indent:
                for (a,b) in val['rules']:
                    tell('{}:'.format(a))
                    with indent:
                        case1({'rule': b})
            return n

        with indent:
            return renderRule(variation['value'], case1, case2)

    def renderMaybeItem(item):
        if item['spare']:
            n = item['length']
            tell('spare {}'.format(n))
            return n
        tit = item['title']
        tell('{} "{}"'.format(item['name'], tit))
        if item['description']:
            with indent:
                tell('description')
                with indent:
                    [tell(i) for i in item['description'].strip().splitlines()]
        with indent:
            n = renderItem(item['variation'])
            if item['remark']:
                tell('remark')
                with indent:
                    [tell(i) for i in item['remark'].strip().splitlines()]
        return n

    def renderGroup():
        tell('subitems')
        n = 0
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(item)
        return n

    def renderExtended():
        tell('extended {} {}'.format(variation['first'], variation['extents']))
        n = 0
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(item)
        return n

    def renderRepetitive():
        tell('repetitive')
        with indent:
            return renderItem(variation['item'])

    def renderExplicit():
        tell('explicit')
        return 0

    def renderCompound():
        tell('compound')
        n = 0
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(item)
        return n

    return locals()['render'+variation['type']]()

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

