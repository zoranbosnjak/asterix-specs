#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json
from jinja2 import Template

data = json.loads(sys.stdin.read())
with open(sys.argv[1]) as f:
    template = Template(f.read())

print (template.render(data))

