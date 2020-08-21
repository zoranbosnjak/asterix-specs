---
title: Home
---

# Overview

**Asterix** is a binary data format. It was originally developed
and is still maintained by [eurocontrol](https://www.eurocontrol.int/asterix).

The major problem with the original specifications is that
they are provided in a form of free text (PDF files). As a consequence,
the very first step in every asterix project is to retype the
specifications to a parsable form. And this is what this projects
is all about.

# Project content

This project contains:

* [asterix definitions](/specs.html) in *zero overhead* custom text based [format](/syntax.html);
* [converter](/converter.html) tool to validate and convert specifications between formats;
* auto-generated specifications in *JSON* and *XML* format;
* auto-generated specifications in *HTML* and *PDF* format;
* formal description of asterix [structure](/struct.html);

## Asterix definitions

In this project, the asterix definitions are stored and maintained in a
compact text based parsable form. The intention of this [format](/syntax.html)
is to be:

* exact and complete (including remarks);
* easy to read and write with any text editor;
* easy to parse and reuse in other projects;
* clutter free;

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
                2: Cartesian vector of start point/ length
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
            SIC "System Identification code"
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
be to write a *converter* from the specifications in this
project to the format required by the target project.\
\
This is one-time effort and pays off quickly by:
    * reusing all [existing](/specs.html) definitions;
    * writing new definitions in clutter free format;

## Example usage in python script

```python

import json

# load definition

# extract some information

# show top level items

# show user application profile

# show some more details about this category
```

