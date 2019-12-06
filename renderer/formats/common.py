
def underline(c, s):
    n = len(s)
    assert len(c) == 1, 'expecting single underline character'
    return '{}\n{}\n'.format(s, c*n)

def indentLines(s, n):
    return ''.join([n*' ' + l for l in s.splitlines()])

def getNumber(s):
    (a, _space, b) = s.partition(' ')
    if a == 'Natural':
        return int(b)
    elif a == 'Real':
        return float(b)
    elif a == 'Ratio':
        (x,y) = map(int, b.split(' '))
        return float(x) / float(y)
    else:
        raise Exception('unexpected value type {}'.format(a))

class RenderTextGeneric(object):
    def __init__(self):
        self.accumulator = ""
        self.variables = {}
        self.indentLevel = 0
        self.onInit()

    def onInit(self):
        pass

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

    def __call__(self, obj):
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

