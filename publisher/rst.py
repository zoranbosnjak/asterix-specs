#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to rst format."""

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
    accumulator.append(' '*indentLevel*4 + s)

def underline(c, s):
    n = len(s)
    assert len(c) == 1, 'expecting single underline character'
    return '{}\n{}\n'.format(s, c*n)

def render(root):
    """Rendering entry point"""
    renderHeader(root)

    cat = root['category']
    tell(underline('-', 'Description of standard data items'))
    [renderToplevel(cat, item) for item in root['items']]

    renderUap(root)

    return ''.join([line+'\n' for line in accumulator])

def renderHeader(root):
    cat = root['category']
    tell(underline('=', "Asterix category " + cat + ' - {}'.format(root['title'])))
    tell('**category**: {}'.format(cat))
    tell('')
    tell('**edition**: {}'.format(root['edition']))
    tell('')
    tell('**date**: {}'.format(root['date']))
    tell('')
    tell(underline('-', 'Preamble'))
    tell(root['preamble'])

def renderToplevel(cat, toplevel):
    item = toplevel['item']
    tell(underline('*', 'I'+cat+'/'+item['name']+' - ' + item['title']))

    def case1(enc):
        tell('*Encoding rule:* This item is ``{}``.'.format(enc['rule']))
    def case2(enc):
        tell('*Encoding rule:* Presence of this item depends on the value of item ``{}``. ::'.format('/'.join(enc['item'])))
        tell('')
        with indent:
            [tell('{} -> {}'.format(a,b)) for (a,b) in enc['rules']]
    renderRule(toplevel['encoding'], case1, case2)
    tell('')

    tell('*Definition*: {}'.format(toplevel['definition']))
    tell('*Structure*: ')
    tell('')
    renderItem(item['variation'])
    tell('')
    if item['remark']:
        tell(item['remark'])

def renderItem(variation):
    ct = variation['type']

    def bits(n):
        if n == 1: return '1 bit'
        return '{} bits'.format(n)

    def renderQuantity(signed, q):
        k = getNumber(q['scaling'])
        fract = q['fractionalBits']
        unit = q.get('unit')

        tell('- scaling factor: {}'.format(k))
        tell('- fractional bits: {}'.format(fract))
        if unit:
            tell('- unit: "{}"'.format(unit))

        if fract == 0:
            lsb = ":math:`{}` {}".format(k, unit or '')
            tell("- LSB = {}".format(lsb))
        else:
            b = '{2^{'+str(fract)+'}}'
            lsb1 = ":math:`{} / {}` {}".format(k, b, unit or '')
            c = '{'+str(pow(2, fract))+'}'
            lsb2 = ":math:`{} / {}` {}".format(k, c, unit or '')
            tell("- LSB = {} = {}".format(lsb1, lsb2))

        lim1 = q['lowLimit']
        lim2 = q['highLimit']
        if lim1 or lim2:
            rng = ''
            if lim1:
                rng += ('[' if lim1['including'] else '(')
                rng += str(getNumber(lim1['limit']))
            else:
                rng += r'(-\infty' if signed else r'[0'
            rng += ', '
            if lim2:
                rng += str(getNumber(lim2['limit']))
                rng += (']' if lim2['including'] else ')')
            else:
                rng += r'\infty)'
            tell('- range: :math:`{}` {}'.format(rng, unit or ''))
        tell('')

    def renderFixed():
        n = variation['size']
        tell('- {} [``{}``]'.format(bits(n), '.'*n))
        tell('')

        def case1(val):
            value = val['rule']
            t = value['type']

            if t == 'Raw':
                tell('- raw value')
                tell('')
            elif t == 'Unsigned':
                tell('- unsigned number')
                renderQuantity(False, value['quantity'])
            elif t == 'Signed':
                tell('- signed number')
                renderQuantity(True, value['quantity'])
            elif t == 'Table':
                tell('- values:')
                tell('')
                with indent:
                    for key,value in value['values']:
                        tell('| {}: {}'.format(key, value))
                tell('')
            elif t == 'StringAscii':
                tell('- Ascii string (8-bits per character)')
                tell('')
            elif t == 'StringICAO':
                tell('- Ascii ICAO (6-bits per character)')
                tell('')
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def case2(val):
            otherItem = '/'.join(val['item'])
            tell('* Content of this item depends on the value of item ``{}``.'.format(otherItem))
            tell('')
            with indent:
                for (a,b) in val['rules']:
                    tell('* In case of ``{} == {}``:'.format(otherItem, a))
                    with indent:
                        case1({'rule': b})
            tell('')
            return n

        return renderRule(variation['value'], case1, case2)

    def renderMaybeItem(item):
        if item['spare']:
            n = item['length']
            tell('``(spare)``')
            tell('')
            tell('- {} [``{}``]'.format(bits(n), '.'*n))
            tell('')
            return n
        tit = item['title']
        tell('``{}``{}'.format(item['name'], ' - *{}*'.format(tit) if tit else ''))
        tell('')
        if item['description']:
            tell(' '.join(item['description'].splitlines()))
            tell('')
        n = renderItem(item['variation'])
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
                n += renderMaybeItem(item)
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
                n += renderMaybeItem(item)
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
        tell('Repetitive item')
        tell('')
        with indent:
            x = renderItem(variation['item'])
        return x

    def renderExplicit():
        tell('Explicit item')
        tell('')
        return 0

    def renderCompound():
        tell('Compound item')
        tell('')
        n = 0
        m = 0
        with indent:
            for item in variation['items']:
                n += renderMaybeItem(item)
                m += 1
        return n

    return locals()['render'+variation['type']]()

def renderUap(root):
    cat = root['category']
    uap = root['uap']

    def findItem(name):
        for item in root['items']:
            item = item['item']
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
                s += '``I{}/{}`` - {}'.format(cat, name, findItem(name))
            tell(s)

            if cnt >= 7:
                cnt = 0
                tell('- ``(FX)`` - Field extension indicator')
        tell('')

    tell(underline('=', "User Application Profile for Category {}".format(cat)))
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

