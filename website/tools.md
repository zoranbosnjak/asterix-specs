---
title: Tools
---

# Tools

Asterix conversion and validation tool is an integral part of this project.

Features:

* validate sources file
* transparently convert from one format to another
* convert from one format to itself (prettify definition file)
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
[this link](/bin/aspecs-static).

To install, download and copy the file to some location and set file mode,
for example under UbuntuOS:

```bash
sudo cp aspecs-static /usr/local/bin/aspecs
sudo chmod 755 /usr/local/bin/aspecs
aspecs --help
```

Install or upgrade procedure for [nix](https://nixos.org/) users:

```bash
cd tools
nix-build
nix-env -i $(readlink result)
aspecs --help
```

## Usage

```bash
# show help
aspecs --help

# validate a file (ast/json format)
aspecs validate -f input.ast --ast --warnings
aspecs validate -f input.json --json --warnings

# show definition fingerprint, for the same definitions
# the fingerprint shall be the same, regardless of the format
aspecs checksum -f input.ast --ast
aspecs checksum -f input.json --json

# convert ast -> json
aspecs convert -f input.ast --ast --json > out.json

# convert json -> ast
aspecs convert -f input.json --json --ast > out.ast

# prettify ast file after editing, the file will be overwritten
aspecs prettify --remove-comments input.ast --ast
```

## Development

```bash
# install nix package manager
curl -L https://nixos.org/nix/install | sh

# clone repository
git clone https://github.com/zoranbosnjak/asterix-specs.git

# (re)build tools
cd asterix-specs/tools/
nix-build

# run it
./result/bin/aspecs --help
```

