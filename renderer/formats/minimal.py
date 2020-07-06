#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Render json decoded object to minimal json.
Simplify catalogue to bare minimum,
just to be able to decode/encode, using raw bits."""

import json

from formats.common import case

def stripVariation(variation):
    f = case('variation type', variation['type'],
        ('Element', lambda: {'size': variation['size']}),
        ('Group', lambda: {
            'items': [stripItem(item) for item in variation['items']]}),
        ('Extended', lambda: {
            'first': variation['first'], 'extents': variation['extents'],
            'items': [stripItem(item) for item in variation['items']]}),
        ('Repetitive', lambda: {
            'rep': variation['rep'],
            'element': stripVariation(variation['variation'])}),
        ('Explicit', lambda: {}),
        ('Compound', lambda: {
            'items': [stripItem(item) for item in variation['items']]}),
        )
    d = f()
    d.update({'type': variation['type']})
    return d

def stripItem(item):
    if item is None:
        return None
    if item['spare']:
        return item
    return {
        'spare': False,
        'name': item['name'],
        'variation': stripVariation(item['variation']),
    }

def render(s):
    root = json.loads(s)
    catalogue = [stripItem(item) for item in root['catalogue']]
    dst = root.copy()
    dst['catalogue'] = catalogue
    return json.dumps(dst, indent=4)

