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

* This project tries to address most aspects of asterix specifications,
including rare corner cases. For some projects this might not be nessary and
it would make sense to simplify definitions before actual use. See example
below.

If you are creating new categories, consider [contributing](/source.html)
definitions to the upstream repository.

## Example: simplify specification with `python` script

This example converts asterix specification to a simpler form, with
some nonessential information removed from the structure. Where the
specification is context dependent, it is converted to a *default*
value (this is not exactly according to the original specification,
but it allows much simpler struture and the problem can be handled
manually in the application code if required - that is: hardcoding
the exceptional cases).

Input and output are both in `json` format.

```python
# -- convert.py script

import sys
import json

def split(obj):
    return (obj['tag'], obj['contents'])

def unsplit(tag, contents):
    return {'tag': tag, 'contents': contents}

def handle_number(obj):
    """Convert precise number structure to int or float"""
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

def handle_rule(handler, obj):
    """Handle ContextFree/Dependent rule.
    In this example, ignore dependency and use 'default' rule.
    This handler requires another handler for the internal structure."""
    t, cont = split(obj)
    if t == 'ContextFree':
        return handler(cont)
    elif t == 'Dependent':
        return handler(cont['default'])
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

def handle_content(obj):
    t, cont = split(obj)
    if t == 'ContentQuantity':
        return unsplit('quantity', {
            'lsb': handle_number(cont['lsb']),
            'signed': handle_signedness(cont['signedness']),
        })
    return unsplit('other', None)

def handle_variation(obj):
    t, cont = split(obj)
    if t == 'Element':
        rv = {
            'bits': cont['bitSize'],
            'content': handle_rule(handle_content, cont['rule']),
        }
    elif t == 'Group':
        rv = [handle_item(i) for i in cont]
    elif t == 'Extended':
        rv = [handle_item(i) if i else None for i in cont]
    elif t == 'Repetitive':
        rv = {
            'type': cont['type'],
            'variation': handle_variation(cont['variation']),
        }
    elif t == 'Explicit':
        rv = None
    elif t == 'Compound':
        rv = [handle_nonspare(i) if i else None for i in cont]
    else:
        raise Exception('unexpected', t)
    return unsplit(t, rv)

def handle_item(obj):
    t, cont = split(obj)
    if t == 'Spare':
        rv = cont
    elif t == 'Item':
        rv = handle_nonspare(cont)
    else:
        raise Exception('unexpected', t)
    return unsplit(t, rv)

def handle_nonspare(obj):
    return {
        'name': obj['name'],
        'variation': handle_rule(handle_variation, obj['rule']),
    }

def handle_uap_item(obj):
    t, cont = split(obj)
    if t == 'UapItem':
        return cont
    return None

def handle_uap(obj):
    t, cont = split(obj)
    if t == 'Uap':
        return unsplit(t, [handle_uap_item(i) for i in cont])
    elif t == 'Uaps':
        return unsplit(t, [(name,
            [handle_uap_item(i) for i in lst]) for name, lst in cont['cases']])
    else:
        raise Exception('unexpected', t)

def handle_asterix(obj):
    t, cont = split(obj)
    if t == 'AsterixBasic':
        rv = {
            'category': cont['category'],
            'edition': cont['edition'],
            'catalogue': [handle_nonspare(i) for i in cont['catalogue']],
            'uap': handle_uap(cont['uap']),
        }
    elif t == 'AsterixExpansion':
        rv = {
            'category': cont['category'],
            'edition': cont['edition'],
            'fspecByteSize': cont['fspecByteSize'],
            'items': [handle_nonspare(i) for i in cont['items']],
        }
    else:
        raise Exception('unexpected', t)
    return unsplit(t, rv)

# main
infile=sys.argv[1]
with open(infile) as f:
    obj1 = json.loads(f.read())
obj2 = handle_asterix(obj1)
print(json.dumps(obj2, indent=4))
```

Download and convert one specification:

```bash
# download
src=https://zoranbosnjak.github.io/asterix-specs/specs/
curl $src/cat001/cats/cat1.4/definition.json > original.json

# convert
python3 convert.py original.json > simple.json
ls -l original.json simple.json
```

# Related projects

If you’re using asterix-specs definition files in your project (directly or
indirectly) and the source code is available, your are welcome to
[notify project maintainer](/source.html), to be added to the list.

* [asterix-data](https://github.com/zoranbosnjak/asterix-data) is a set of
  machine-readable asterix definition files in *xml* format. The *xml* files
  are generated automatically out of asterix specifications from this
  project (*json* to *xml* conversion). In turn, various other legacy projects
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

* [asterix-libs](https://github.com/zoranbosnjak/asterix-libs#readme)
  is a library collection project for processing (encoding and decoding)
  asterix data format. It currently supports
  [python](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/python#readme)
  and
  [haskell](https://github.com/zoranbosnjak/asterix-libs/tree/main/libs/haskell#readme).
  The library source code is automatically updated from `*.ast` specification
  files from this project.

* [asterix-tool](https://github.com/zoranbosnjak/asterix-tool#readme)
  is a versatile asterix data related command line tool. Features include:
  random asterix data generation, asterix decoding, UDP datagram sending and
  receiving (including multicast), recording/replaying to/from a file, custom
  asterix filtering...
  Supported asterix categories and editions are automatically updated from
  this project.
