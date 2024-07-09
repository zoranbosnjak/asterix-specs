---
title: Home
---

# Overview

**Asterix** is a binary data format, developed and
maintained by [eurocontrol](https://www.eurocontrol.int/asterix).

The major problem with the original specifications is that
they are provided in a form of free text (PDF files). As a consequence,
the very first step in every asterix project is to retype the
specifications to a parsable form. And this is what this project
is all about.

# Project content

This project contains:

* [asterix definitions](/specs.html) in *zero overhead* custom text based [format](/syntax.html);
* [aspecs](/aspecs.html) to validate and convert specifications between formats;
* auto-generated specifications in *JSON* format;
* auto-generated specifications in *HTML* and *PDF* format;
* formal description of asterix [structure](/struct.html);

## Asterix definitions

In this project, the asterix definitions are stored and maintained in a
compact text based parsable form. The intention of this [format](/syntax.html)
is to be:

* exact and complete (including definition text and remarks);
* easy to read and write with any text editor;
* easy to parse and reuse in other projects;
* clutter free.

Example definition snippet:
```
asterix 008 "Monoradar Derived Weather Information"
edition 1.2
date 2014-08-24
preamble
    Surveillance data exchange.

items

    000 "Message Type"
        definition
            This Data Item allows for a more convenient handling of the messages
            at the receiver side by further defining the type of transaction.
        element 8
            table
                1: Polar vector
                2: Cartesian vector of start point/length
                3: Contour record
                4: Cartesian start point and end point vector
                254: SOP message
                255: EOP message

    010 "Data Source Identifier"
        definition
            Identification of the radar station from which the data are received.
        group
            SAC "System Area Code"
                element 8
                    raw
            SIC "System Identification Code"
                element 8
                    raw
        remark
            Note:
                The defined SACs are on the EUROCONTROL ASTERIX website
                (www.eurocontrol.int/asterix)
    ...
```
See the [structure description](/struct.html) for more details.

# How to use it?

The [specifications](/specs.html) are provided in various formats.

* For new projects it is recommended to use the definitions directly
in the original *ast* or *json* format.

* For the existing projects, the odds are that some subset of the
specifications is already defined in some other structured format
(like *json* or *xml*). In this case, the reasonable approach would
be to write *additional converter* from the specifications in this
project to the format required by the target project.\
\
This is a one-time effort and pays off quickly by:
    * reusing all the [existing](/specs.html) definitions;
    * writing new definitions in a clutter free format.

If you are creating new categories, consider [contributing](/source.html)
definitions to the upstream repository.

## Example: traversing specification with `python` script

This example is using asterix category description in `json` format.

```python
import sys
import json

def split(obj):
    return (obj['tag'], obj['contents'])

def dump_rule(f, obj):
    t, cont = split(obj)
    if t == 'ContextFree':
        f(cont)
    elif t == 'Dependent':
        f(cont['default'])
    else:
        raise Exception('unexpected', t)

def dump_uap(obj):
    t, cont = split(obj)
    def f(i):
        t, name = split(i)
        if t == 'UapItem':
            return name
        elif t == 'UapItemRFS':
            return 'RFS'
        elif t == 'UapItemSpare':
            return '-'
        else:
            raise Exception('unexpected', t)
    if t == 'Uap':
        print('Single uap... {}'.format([f(i) for i in cont]))
    elif t == 'Uaps':
        raise NotImplementedError
    else:
        raise Exception('unexpected', t)

def handle_signedness(obj):
    t, cont = split(obj)
    if t == 'Signed':
        return True
    elif t == 'Unsigned':
        return False
    else:
        raise Exception('unexpected', t)

def handle_number(obj):
    t, cont = split(obj)
    if t == 'NumInt':
        return cont
    elif t == 'NumDiv':
        a = handle_number(cont['numerator'])
        b = handle_number(cont['denominator'])
        return float(a) / float(b)
    elif t == 'NumPow':
        return pow(cont['base'], cont['exponent'])
    else:
        raise Exception('unexpected', t)

def handle_constrain(obj):
    t, cont = split(obj)
    if t == 'EqualTo': s = '=='
    elif t == 'NotEqualTo': s = '/='
    elif t == 'GreaterThan': s = '>'
    elif t == 'GreaterThanOrEqualTo': s = '>='
    elif t == 'LessThan': s = '<'
    elif t == 'LessThanOrEqualTo': s = '<='
    else:
        raise Exception('unexpected', t)
    return {
        'type': s,
        'value': handle_number(cont),
    }

def dump_content(obj):
    t, cont = split(obj)
    if t == 'ContentRaw':
        print('Raw content')
    elif t == 'ContentTable':
        print('Table content {}'.format(cont))
    elif t == 'ContentString':
        print('String content {}'.format(cont['tag']))
    elif t == 'ContentInteger':
        print({
            'type': 'Integer',
            'signed': handle_signedness(cont['signedness']),
            'constraints': [handle_constrain(i) for i in cont['constraints']],
        })
    elif t == 'ContentQuantity':
        print({
            'type': 'Quantity',
            'constraints': [handle_constrain(i) for i in cont['constraints']],
            'lsb': handle_number(cont['lsb']),
            'signed': handle_signedness(cont['signedness']),
            'unit': cont['unit'],
        })
    elif t == 'ContentBds':
        print('BDS register...')
    else:
        raise Exception('unexpected', t)

def dump_variation(obj):
    t, cont = split(obj)
    if t == 'Element':
        print('Element {} bit(s)'.format(cont['bitSize']))
        dump_rule(dump_content, cont['rule'])
    elif t == 'Group':
        print('Group of items')
        [dump_item(i) for i in cont]
    elif t == 'Extended':
        print('Extended')
        for i in cont:
            if i is None:
                print('FX')
            else:
                dump_item(i)
    elif t == 'Repetitive':
        print('Repetitive item {}'.format(cont['type']))
        dump_variation(cont['variation'])
    elif t == 'Explicit':
        print('Explicit item')
    elif t == 'Compound':
        print('Compound item')
        [dump_nonspare(i) for i in cont if i is not None]
    else:
        raise Exception('unexpected', t)

def dump_item(obj):
    t, cont = split(obj)
    if t == 'Spare':
        print('spare item {} bit(s)'.format(cont))
    elif t == 'Item':
        return dump_nonspare(cont)
    else:
        raise Exception('unexpected', t)

def dump_nonspare(obj):
    doc = obj['documentation'] # definition, description...
    print('item {} - {}'.format(obj['name'], obj['title']))
    dump_rule(dump_variation, obj['rule'])

def dump_asterix(obj):
    t, cont = split(obj)
    if t == 'AsterixBasic':
        print('Basic asterix category {}, {}, {}'.format(
            cont['category'], cont['edition'], cont['date']))
        dump_uap(cont['uap'])
        [dump_nonspare(i) for i in cont['catalogue']]
    elif t == 'AsterixExpansion':
        print('This is asterix expansion...')
    else:
        raise Exception('unexpected', t)

# main
infile=sys.argv[1]
with open(infile) as f:
    obj = json.loads(f.read())
dump_asterix(obj)
```

# Related projects

If you’re using asterix-specs definition files in your project (directly or
indirectly) and the source code is available, your are welcome to
[notify project maintainer](/source.html), to be added to the list.

* [asterix-data](https://github.com/zoranbosnjak/asterix-data) is a set of
  machine-readable asterix definition files in *xml* format. The *xml* files
  are generated automatically out of asterix specifications from this
  project (*json* to *xml* conversion). In turn, various other projects
  are using generated *xml* definition files for encoding and decoding
  asterix data.

* [asterix](https://github.com/CroatiaControlLtd/asterix) is a *python* module
  and application for reading and parsing asterix data. It includes custom *xml*
  configuration files for asterix category definitions, similar but not the
  same as the `asterix-data` project mentioned above.
  [This directory](https://github.com/CroatiaControlLtd/asterix/tree/master/asterix-specs-converter)
  contains an automatically generated version of the *xml* configuration files,
  together with the conversion script.

* [wireshark](https://www.wireshark.org/) is a well known
  network protocol analyzer, also capable of decoding asterix data format.
  Asterix related `C/C++` structures and decoding functions are
  automatically generated from *json* `asterix-specs` definitions. See
  [asterix parser generator](https://gitlab.com/wireshark/wireshark/-/tree/master/tools/asterix)
  (part of wireshark source code) for details.

* [asterix-lib-generator](https://zoranbosnjak.github.io/asterix-lib-generator/)
  is an asterix data processing library source code generator. It currently
  supports
  [python](https://zoranbosnjak.github.io/asterix-lib-generator/python.html).
  The library source code is automatically generated out of the `*.ast`
  specification files from this project.

* [asterix-tool](https://github.com/zoranbosnjak/asterix-tool#readme)
  is a versatile asterix data related command line tool. Features include:
  random asterix data generation, asterix decoding, UDP datagram sending and
  receiving (including multicast), recording/replaying to/from a file, custom
  asterix filtering...
  Supported asterix categories and editions are automatically updated from
  this project.
