---
title: Converter
---

# Converter

Asterix conversion and validation tool is an integral part of this project.

Features:

* validate sources file
* transparently convert from one format to another
* convert from one format to itself (pretify definition file)
* dump file as a list of items
* show asterix definition signature (like a fingerprint, but not
  depending on a particular file format).

Conversion process works in following steps:

#. parse input file to internal (abstract) structure
#. validate internal structure
#. generate required output format from internal structure

Supported formats:

* custom *.ast* format
* standard *.json* format
* standard *.xml* format (experimental)

It is possible to extend the converter to other formats
by implementing appropriate `encodeAsterix` and `decodeAsterix` functions.

Abstract [internal structure](/struct.html) is the same for all formats.

## Installation

Pre-build 64-bit linux binary is available to download from
[this link](/bin/converter-static).

To install, download and copy the file to some location and set file mode,
for example under UbuntuOS:

```bash
sudo cp converter-static /usr/local/bin/converter
sudo chmod 755 /usr/local/bin/converter
converter --help
```

For [nix](https://nixos.org/) users,
a [converter nix expression](https://github.com/zoranbosnjak/asterix-specs/blob/master/converter/default.nix)
is provided in the source code.

## Usage

```bash
# show help
converter --help

# validate a file (ast/json format)
converter -f input.ast --ast --validate
converter -f input.json --json --validate

# show definition fingerprint, for the same definitions
# the fingerprint shall be the same, regardless of the format
converter -f input.ast --ast --sha1
converter -f input.json --json --sha1

# convert ast -> json
converter -f input.ast --ast --json > out.json

# convert json -> ast
converter -f input.json --json --ast > out.ast

# pretify ast file after editing, the file will be overwritten
converter --pretify input.ast --remove-comments --ast
```

## Development

```bash
# install nix package manager
curl -L https://nixos.org/nix/install | sh

# rebuild converter
git clone https://github.com/zoranbosnjak/asterix-specs.git
cd asterix-specs/converter/
nix-build

# run it
./result/bin/converter --help
```

