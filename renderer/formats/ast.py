
from itertools import chain, repeat, accumulate

from formats.common import RenderTextGeneric

def getNumber(s, default=None):
    class Natural(object):
        def __init__(self, val): self.val = val
        def __str__(self): return '{}'.format(self.val)
        def __bool__(self): return self.val != default

    class Real(object):
        def __init__(self, val): self.val = val
        def __str__(self): return '{0:f}'.format(self.val).rstrip('0')
        def __bool__(self): return True

    class Ratio(object):
        def __init__(self, a, b):
            self.a = a
            self.b = b
        def __str__(self): return '{}/{}'.format(self.a, self.b)
        def __bool__(self): return True

    (a, _space, b) = s.partition(' ')
    if a == 'Natural':
        return Natural(int(b))
    elif a == 'Real':
        return Real(float(b))
    elif a == 'Ratio':
        (x,y) = map(int, b.split(' '))
        return Ratio(x, y)
    else:
        raise Exception('unexpected value type {}'.format(a))

class RenderAst(RenderTextGeneric):
    """Render back to the original .ast format"""

    def enterRoot(self, root):
        dl = self.dumpLn
        dl('category {} "{}"'.format(root['category'], root['title']))
        dl('edition {}'.format(root['edition']))
        dl('date {}'.format(root['date']))
        dl('preamble')
        self.indent()
        for i in root['preamble'].strip().splitlines():
            dl(i)
        self.unindent()
        dl('')

    def enterToplevels(self, parent, toplevels):
        dl = self.dumpLn
        dl('items')
        self.indent()

    def exitToplevels(self, rv):
        self.unindent()

    def enterToplevel(self, parent, toplevel):
        dl = self.dumpLn
        item = toplevel['item']
        dl('')
        dl('{} "{}"'.format(item['name'], item['title']))
        self.indent()
        dl('{}'.format('mandatory' if toplevel['mandatory'] else 'optional'))
        dl('definition')
        self.indent()
        for i in toplevel['definition'].strip().splitlines():
            dl(i)
        self.unindent()
        self.renderItem(item['variation'])
        if item['remark']:
            dl('remark')
            self.indent()
            for i in item['remark'].strip().splitlines():
                dl(i)
            self.unindent()

        self.unindent()

    def renderItem(self, variation):
        dl = self.dumpLn

        def renderQuantity(signed, q):
            k = getNumber(q['scaling'], 1)
            fract = q['fractionalBits']
            unit = q.get('unit')

            dl('{}'.format('signed' if signed else 'unsigned'))

            if k:
                dl('scale {}'.format(k))
            if fract != 0:
                dl('fractional {}'.format(fract))
            if unit is not None:
                dl('unit "{}"'.format(unit))

            lim1 = q['lowLimit']
            lim2 = q['highLimit']

            if lim1 is not None:
                operator = 'ge' if lim1['including'] else 'gt'
                lim = getNumber(lim1['limit'])
                dl('{} {}'.format(operator, lim))

            if lim2 is not None:
                operator = 'le' if lim2['including'] else 'lt'
                lim = getNumber(lim2['limit'])
                dl('{} {}'.format(operator, lim))

        def renderFixed():
            n = variation['size']
            dl('item {}'.format(n))

            value = variation['value']
            t = value['type']
            if t == 'Raw':
                dl('raw')
            elif t == 'Unsigned':
                renderQuantity(False, value['quantity'])
            elif t == 'Signed':
                renderQuantity(True, value['quantity'])
            elif t == 'Table':
                dl('discrete')
                self.indent()
                for key,value in value['values']:
                    dl('{}: {}'.format(key, value))
                self.unindent()
            elif t == 'StringAscii':
                dl('string ascii')
            elif t == 'StringICAO':
                dl('string icao')
            else:
                raise Exception('unexpected value type {}'.format(t))
            return n

        def renderMaybeItem(item):
            if item['spare']:
                n = item['length']
                dl('spare {}'.format(n))
                return n
            tit = item['title']
            dl('{} "{}"'.format(item['name'], tit))
            if item['description']:
                self.indent()
                dl('description')
                self.indent()
                for i in item['description'].strip().splitlines():
                    dl(i)
                self.unindent()
                self.unindent()
            self.indent()
            n = self.renderItem(item['variation'])
            if item['remark']:
                dl('remark')
                self.indent()
                for i in item['remark'].strip().splitlines():
                    dl(i)
                self.unindent()
            self.unindent()
            return n

        def renderGroup():
            dl('subitems')
            n = 0
            self.indent()
            for item in variation['items']:
                n += renderMaybeItem(item)
            self.unindent()
            return n

        def renderExtended():
            dl('extended {} {}'.format(variation['first'], variation['extents']))
            n = 0
            self.indent()
            for item in variation['items']:
                n += renderMaybeItem(item)
            self.unindent()
            return n

        def renderRepetitive():
            dl('repetitive')
            n = 0
            self.indent()
            n = self.renderItem(variation['item'])
            self.unindent()
            return n

        def renderExplicit():
            dl('explicit')
            return 0

        def renderCompound():
            dl('compound')
            self.indent()
            n = 0
            for item in variation['items']:
                n += renderMaybeItem(item)
            self.unindent()
            return n

        return locals()['render'+variation['type']]()

    def enterUap(self, parent, uap):
        dl = self.dumpLn
        dl('')
        dl('uap')
        self.indent()
        for i in uap:
            dl(i if i is not None else '-')
        self.unindent()

    def exitRoot(self, rv):
        dl = self.dumpLn
        dl('')

