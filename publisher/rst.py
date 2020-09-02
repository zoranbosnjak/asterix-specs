#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to rst format."""

import json
from itertools import chain, accumulate, repeat
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

def underline(c, s):
    n = len(s)
    assert len(c) == 1, 'expecting single underline character'
    return '{}\n{}\n'.format(s, c*n)

def render(s):
    """Rendering entry point"""
    root = json.loads(s)
    renderHeader(root)

    cat = root['number']
    name = 'I'+'{:03d}'.format(cat)

    rt = root['type']
    if rt == 'Basic':
        tell(underline('-', 'Description of standard data items'))
        [renderTopItem(name, item) for item in root['catalogue']]
        renderUap(root)
    elif rt == 'Expansion':
        tell(underline('-', 'Description of asterix expansion'))
        renderVariation(name, root['variation'])
    else:
        raise Exception('unexpected root type {}'.format(rt))

    return ''.join([line+'\n' for line in accumulator])

def renderHeader(root):
    cat = root['number']
    rt = root['type']
    if rt == 'Basic':
        tell(underline('=', "Asterix category " + '{:03d}'.format(cat) + ' - {}'.format(root['title'])))
    elif rt == 'Expansion':
        tell(underline('=', "Asterix expansion " + '{:03d}'.format(cat) + ' - {}'.format(root['title'])))
    else:
        raise Exception('unexpected root type {}'.format(rt))
    tell('**category**: {:03d}'.format(cat))
    tell('')
    edition = root['edition']
    tell('**edition**: {}.{}'.format(edition['major'], edition['minor']))
    tell('')
    date = root['date']
    tell('**date**: {}-{:02d}-{:02d}'.format(date['year'], date['month'], date['day']))
    tell('')
    if rt == 'Basic':
        tell(underline('-', 'Preamble'))
        tell(root['preamble'])
    tell('')

def renderTopItem(parent, item):
    name = parent + '/' + item['name']
    tell('')
    tell(underline('*', name + ' - ' + item['title']))
    tell('')
    tell('*Definition*: {}'.format(item['definition']))
    tell('')
    tell('*Structure*: ')
    tell('')
    renderVariation(name, item['variation'])
    tell('')
    if item['remark']:
        tell(item['remark'])

def constrainToString(constrain):
    return '{} {}'.format(constrain['type'], str(getNumber(constrain['value'])))

def bits(n):
    if n == 1:
        return '1 bit'
    return '{} bits'.format(n)

def renderVariation(parent, variation):

    def renderInteger(value):
        tell('- {} integer'.format('signed' if value['signed'] else 'unsigned'))
        for const in value['constraints']:
            tell('- value :math:`{}`'.format(constrainToString(const)))
        tell('')

    def renderQuantity(value):
        tell('- {} quantity'.format('signed' if value['signed'] else 'unsigned'))
        k = getNumber(value['scaling'])
        fract = value['fractionalBits']
        unit = value['unit']
        tell('- scaling factor: {}'.format(k))
        tell('- fractional bits: {}'.format(fract))
        if unit:
            tell('- unit: "{}"'.format(unit))

        if fract == 0:
            lsb = ":math:`{}` {}".format(k, unit or '')
            tell("- LSB = {}".format(lsb))
        else:
            b = '{2^{'+str(fract)+'}}'
            lsb1 = ":math:`{} / {}` {}".format(k, b, unit)
            c = '{'+str(pow(2, fract))+'}'
            d = pow(2, fract)
            lsb2 = ":math:`{} / {}` {}".format(k, c, unit)
            lsb3 = ":math:`\\approx {}` {}".format(float(k)/d, unit)
            tell("- LSB = {} = {} {}".format(lsb1, lsb2, lsb3))
        for const in value['constraints']:
            tell('- value :math:`{}` {}'.format(constrainToString(const),unit))

        tell('')

    def renderBds(value):
        tell('- BDS register')
        tell('')

    def renderElement():
        n = variation['size']
        tell('- {} [``{}``]'.format(bits(n), '.'*n))
        tell('')

        def case0():
            tell('- raw value')
            tell('')
            return n

        def case1(val):
            value = val['rule']
            t = value['type']

            if t == 'Table':
                tell('- values:')
                tell('')
                with indent:
                    for key,value in value['values']:
                        tell('| {}: {}'.format(key, value))
                tell('')
            elif t == 'String':
                if value['variation'] == 'StringAscii':
                    tell('- Ascii string (8-bits per character)')
                elif value['variation'] == 'StringICAO':
                    tell('- ICAO string (6-bits per character)')
                elif value['variation'] == 'StringOctal':
                    tell('- Octal string (3-bits per digit)')
                else:
                    raise Exception('unexpected string type {}'.format(value['variation']))
                tell('')
            elif t == 'Integer':
                renderInteger(value)
            elif t == 'Quantity':
                renderQuantity(value)
            elif t == 'Bds':
                renderBds(value)
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def case2(val):
            otherItem = '/'.join(val['name'])
            tell('* Content of this item depends on the value of item ``{}``.'.format(otherItem))
            tell('')
            with indent:
                for (a,b) in val['rules']:
                    tell('* In case of ``{} == {}``:'.format(otherItem, a))
                    with indent:
                        case1({'rule': b})
            tell('')
            return n

        return renderRule(variation['content'], case0, case1, case2)

    def renderMaybeItem(parent, item):
        if item['spare']:
            n = item['length']
            tell('**' + parent + '/(spare)**')
            tell('')
            tell('- {} [``{}``]'.format(bits(n), '.'*n))
            tell('')
            return n
        name = parent + '/' + item['name']
        tit = item['title']
        tell('**{}**{}'.format(name, ' - *{}*'.format(tit) if tit else ''))
        tell('')
        if item['description']:
            tell(' '.join(item['description'].splitlines()))
            tell('')
        n = renderVariation(name, item['variation'])
        if item['remark']:
            with indent:
                tell('remark')
                with indent:
                    for i in item['remark'].strip().splitlines():
                        tell(i)
            tell('')
        return n

    def renderGroup():
        n = 0
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(parent, item)
        return n

    def renderExtended():
        n1 = variation['first']
        n2 = variation['extents']
        fx = accumulate(chain(repeat(n1,1), repeat(n2)))
        nextFx = next(fx)
        tell('Extended item with first part ``{} bits`` long and optional ``{} bits`` extends.'.format(n1, n2))
        tell('')
        n = 0
        terminated = False
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(parent, item)
                terminated = False
                if (n+1) == nextFx:
                    tell('``(FX)``')
                    tell('')
                    tell('- extension bit')
                    tell('')
                    with indent:
                        for key,value in [(0, "End of data item"), (1, "Extension into next extent")]:
                            tell('| {}: {}'.format(key, value))
                    tell('')
                    n += 1
                    nextFx = next(fx)
                    terminated = True
        assert terminated, "wrong termination of extended item"
        return n

    def renderRepetitive():
        rep = variation['rep']
        tell('Repetitive item, repetition factor {} bits.'.format(rep))
        tell('')
        with indent:
            x = renderVariation(parent, variation['variation'])
        return x

    def renderExplicit():
        tell('Explicit item')
        tell('')
        return 0

    def renderCompound():
        fspec = variation['fspec']
        if fspec == None:
            tell('Compound item (FX)')
        else:
            tell('Compound item (fspec={} bits)'.format(fspec))
        tell('')
        n = 0
        with indent:
            for item in variation['items']:
                if item is None:
                    tell('(empty subitem)')
                    tell('')
                else:
                    n += renderMaybeItem(parent, item)
        return n

    return locals()['render'+variation['type']]()

def renderUap(root):
    cat = root['number']
    uap = root['uap']

    def findItem(name):
        for item in root['catalogue']:
            if item['name'] == name:
                return item['title']
        return '??'

    def dumpUap(items):
        frn = 0
        cnt = 0
        for name in items:
            frn += 1
            cnt += 1

            s = '- ({}) '.format(frn)
            if name is None:
                s += '``(spare)``'
            else:
                s += '``I{:03d}/{}`` - {}'.format(cat, name, findItem(name))
            tell(s)

            if cnt >= 7:
                cnt = 0
                tell('- ``(FX)`` - Field extension indicator')
        tell('')

    tell(underline('=', "User Application Profile for Category {:03d}".format(cat)))
    t = uap['type']
    if t == 'uap':
        dumpUap(uap['items'])
    elif t == 'uaps':
        tell('This category has multiple UAPs.')
        tell('')
        for var in uap['variations']:
            tell(underline('-', var['name']))
            dumpUap(var['items'])
    else:
        raise Exception('unexpected uap type {}'.format(t))

