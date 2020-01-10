#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def getNumber(s, default=None):
    """Get Natural/Real/Rational number as an object."""
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

def renderRule(rule, caseContextFree, caseDependent):
    rule_type = rule['type']
    if rule_type == 'ContextFree':
        return caseContextFree(rule)
    elif rule_type == 'ItemDependent':
        return caseDependent(rule)
    else:
        raise Exception('unexpected rule type {}'.format(rule_type))

