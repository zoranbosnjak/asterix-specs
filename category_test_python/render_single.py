#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import argparse
import json
from itertools import chain, repeat, accumulate

def underline(c, s):
    n = len(s)
    assert len(c) == 1, 'expecting single underline character'
    return '{}\n{}\n'.format(s, c*n)

def indentLines(s, n):
    return ''.join([n*' ' + l for l in s.splitlines()])

class GenericRender(object):
    def __init__(self, *args, **kw):
        self.accumulator = ""
        self.variables = {}
        self.indentLevel = 0

    def indent(self):
        self.indentLevel += 1

    def unindent(self):
        self.indentLevel -= 1

    def save(self, attr, value):
        self.variables[attr] = value

    def fetch(self, attr):
        return self.variables.get(attr)

    def dump(self, s):
        self.accumulator += ' '*self.indentLevel*4 + s

    def dumpLn(self, s):
        return self.dump(s+'\n')

    def render(self, obj):
        rv0 = self.enterRoot(obj)

        toplevels = obj['items']
        rv1 = self.enterToplevels(obj, toplevels)
        for toplevel in toplevels:
            rv2 = self.enterToplevel(obj, toplevel)
            self.exitToplevel(rv2)
        self.exitToplevels(rv1)

        rv1 = self.enterUap(obj, obj['uap'])
        self.exitUap(rv1)

        self.exitRoot(rv0)
        return self.accumulator

    def enterRoot(self, root): pass
    def enterToplevels(self, parent, toplevels): pass
    def enterToplevel(self, parent, toplevel): pass
    def exitToplevel(self, rv): pass
    def exitToplevels(self, rv): pass
    def enterUap(self, parent, uap): pass
    def exitUap(self, rv): pass
    def exitRoot(self, rv): pass

class RenderRst(GenericRender):

    def enterRoot(self, root):
        cat = root['category']
        self.save('cat', cat)

        self.dump(underline('=', "Asterix category " + cat + ' - {}'.format(root['title'])))
        self.dumpLn('')
        self.dumpLn('**category**: {}'.format(cat))
        self.dumpLn('')
        self.dumpLn('**edition**: {}'.format(root['edition']))
        self.dumpLn('')
        self.dumpLn('**date**: {}'.format(root['date']))
        self.dumpLn('')
        self.dump(underline('-', 'Preamble'))
        self.dumpLn('')
        self.dump(root['preamble'])
        self.dumpLn('')

    def enterToplevels(self, root, toplevels):
        self.dump(underline('-', 'Description of standard data items'))
        self.dumpLn('')

    def enterToplevel(self, root, toplevel):
        item = toplevel['item']
        cat = self.fetch('cat')
        self.dump(underline('*', 'I'+cat+'/'+item['name']+' - ' + item['title']))
        self.dumpLn('')
        self.dumpLn('*Encoding rule*: This item is {}.'.format('mandatory' if toplevel['mandatory'] else 'optional'))
        self.dumpLn('')
        self.dump('*Definition*: {}'.format(toplevel['definition']))
        self.dumpLn('')
        self.dumpLn('*Structure*: ')
        self.dumpLn('')
        self.renderItem(item['content'])
        self.dumpLn('')
        if item['remark']:
            self.dump(item['remark'])
            self.dumpLn('')

    def renderItem(self, content):
        ct = content['type']

        def bits(n):
            if n == 1: return '1 bit'
            return '{} bits'.format(n)

        def renderQuantity(signed, q):
            k = q['scaling']
            fract = q['fractionalBits']
            unit = q.get('unit')

            self.dumpLn('- scaling factor: {}'.format(k))
            self.dumpLn('- fractional bits: {}'.format(fract))
            if unit:
                self.dumpLn('- unit: "{}"'.format(unit))

            if fract == 0:
                lsb = ":math:`{}` {}".format(k, unit or '')
                self.dumpLn("- LSB = {}".format(lsb))
            else:
                b = '{2^{'+str(fract)+'}}'
                lsb1 = ":math:`{} / {}` {}".format(k, b, unit or '')
                c = '{'+str(pow(2, fract))+'}'
                lsb2 = ":math:`{} / {}` {}".format(k, c, unit or '')
                self.dumpLn("- LSB = {} = {}".format(lsb1, lsb2))

            lim1 = q['lowLimit']
            lim2 = q['highLimit']
            if lim1 or lim2:
                rng = ''
                if lim1:
                    rng += ('[' if lim1['including'] else '(')
                    rng += str(lim1['limit'])
                else:
                    rng += r'(-\infty' if signed else r'[0'
                rng += ', '
                if lim2:
                    rng += str(lim2['limit'])
                    rng += (']' if lim2['including'] else ')')
                else:
                    rng += r'\infty)'
                self.dumpLn('- range: :math:`{}` {}'.format(rng, unit or ''))
            self.dumpLn('')

        def renderFixed():
            n = content['size']
            self.dumpLn('- fixed item, {}'.format(bits(n)))
            self.dumpLn('')
            value = content['value']
            t = value['type']
            if t == 'Raw':
                pass
            elif t == 'Unsigned':
                self.dumpLn('- unsigned number')
                renderQuantity(False, value['quantity'])
            elif t == 'Signed':
                self.dumpLn('- signed number')
                renderQuantity(True, value['quantity'])
            elif t == 'Table':
                self.dumpLn('- values:')
                self.dumpLn('')
                self.indent()
                for key,value in value['values'].items():
                    self.dumpLn('| {}: {}'.format(key, value))
                self.unindent()
                self.dumpLn('')
            elif t == 'StringAscii':
                self.dumpLn('- Ascii string (8-bits per character)')
                self.dumpLn('')
            elif t == 'StringICAO':
                self.dumpLn('- Ascii ICAO (6-bits per character)')
                self.dumpLn('')
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def renderMaybeItem(item):
            if item['spare']:
                n = item['length']
                self.dumpLn('``(spare)``')
                self.dumpLn('')
                self.dumpLn('- {}'.format(bits(item['length'])))
                self.dumpLn('')
                return n
            tit = item['title']
            self.dump('``{}``'.format(item['name']))
            self.dumpLn(' - *{}*'.format(tit) if tit else '')
            self.dumpLn('')
            if item['description']:
                self.dumpLn(' '.join(item['description'].splitlines()))
                self.dumpLn('')
            return self.renderItem(item['content'])

        def renderGroup():
            n = 0
            self.indent()
            for item in content['items']:
                n += renderMaybeItem(item)
            self.unindent()
            return n

        def renderExtended():
            n1 = content['first']
            n2 = content['extents']
            fx = accumulate(chain(repeat(n1,1), repeat(n2)))
            nextFx = next(fx)
            self.dumpLn('Extended item with first part ``{} bits`` long and optional ``{} bits`` extends.'.format(n1, n2))
            self.dumpLn('')
            n = 0
            self.indent()
            terminated = False
            for item in content['items']:
                n += renderMaybeItem(item)
                terminated = False
                if (n+1) == nextFx:
                    self.dumpLn('``(FX)``')
                    self.dumpLn('')
                    self.dumpLn('- extension bit')
                    self.dumpLn('')
                    self.indent()
                    for key,value in [(0, "End of data item"), (1, "Extension into next extent")]:
                        self.dumpLn('| {}: {}'.format(key, value))
                    self.unindent()
                    self.dumpLn('')
                    n += 1
                    nextFx = next(fx)
                    terminated = True
            self.unindent()
            assert terminated, "wrong termination of extended item"
            return n

        def renderRepetitive():
            self.dumpLn('Repetitive item')
            self.dumpLn('')
            self.indent()
            x = self.renderItem(content['item'])
            self.unindent()
            return x

        def renderCompound():
            self.dumpLn('Compound item')
            self.dumpLn('')
            self.indent()
            n = 0
            m = 0
            for item in content['items']:
                n += renderMaybeItem(item)
                m += 1
            self.unindent()
            return n

        return locals()['render'+content['type']]()

    def enterUap(self, root, uap):
        cat = self.fetch('cat')
        self.dump(underline('=', "User Application Profile for Category {}".format(cat)))
        items = root['items']
        def findItem(name):
            for item in root['items']:
                item = item['item']
                if item['name'] == name:
                    return item['title']
            return '??'
        for name in uap:
            if name is None:
                self.dumpLn('- ``(spare)``')
            else:
                self.dumpLn('- ``I{}/{}`` - {}'.format(cat, name, findItem(name)))
        self.dumpLn('')

parser = argparse.ArgumentParser()
parser.add_argument('infile', nargs='?', type=argparse.FileType('r'), default=sys.stdin)
parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'), default=sys.stdout)

args = parser.parse_args()

prog = RenderRst()
x = json.loads(args.infile.read())
y = prog.render(x)
args.outfile.write(y)

