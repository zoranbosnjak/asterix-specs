#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to minimal json.
Simplify catalogue to bare minimum,
just to be able to decode/encode, using raw bits."""

import json

from formats.common import getNumber, renderRule, case

def stripElement(element):
    f = case('element type', element['type'],
        ('Fixed', lambda: {'size': element['size']}),
        ('Group', lambda: {
            'subitems': [stripSubitem(subitem) for subitem in element['subitems']]}),
        ('Extended', lambda: {
            'first': element['first'], 'extents': element['extents'],
            'subitems': [stripSubitem(subitem) for subitem in element['subitems']]}),
        ('Repetitive', lambda: stripElement(element['element'])),
        ('Explicit', lambda: {}),
        ('Compound', lambda: {
            'subitems': [stripSubitem(subitem) for subitem in element['subitems']]}),
        )
    d = f()
    d.update({'type': element['type']})
    return d

def stripSubitem(subitem):
    if subitem['spare']: return subitem
    return {
        'spare': False,
        'name': subitem['name'],
        'element': stripElement(subitem['element']),
    }

def render(root):
    catalogue = [stripSubitem(item['subitem']) for item in root['catalogue']]
    dst = root.copy()
    dst['catalogue'] = catalogue
    return json.dumps(dst, indent=4)

