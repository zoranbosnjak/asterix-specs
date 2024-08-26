---
title: Aspecs
---

# Aspecs

Asterix conversion and validation tool is an integral part of this project.

Features:

* validate sources file
* transparently convert from one format to another
* convert from one format to itself (prettify definition file)
* dump file as a list of items
* show asterix definition signature (like a fingerprint, but not
  depending on a particular file format)
* generate documentation format

Conversion process works in the following steps:

#. parse input file to internal (abstract) structure
#. validate internal structure
#. generate required output format from internal structure

Supported formats:

* custom *.ast* format
* standard *.json* format

It is possible to extend the converter to other formats
by implementing appropriate `encodeAsterix` and `decodeAsterix` functions.

Abstract [internal structure](/struct.html) is the same for all formats.

## Installation

To install or upgrade asterix specifications tools, use one of the
following methods.

### Static binary installation

Pre-build 64-bit linux binary is available to download from
[this link](/bin/aspecs-static).

- current version: `$aspecsVersion$`
- SHA256SUM: `$aspecsSha256Sum$`

Download and copy the file to some location and set
file mode, for example under UbuntuOS:

```bash
sudo cp aspecs-static /usr/local/bin/aspecs
sudo chmod 755 /usr/local/bin/aspecs

# verify installation
aspecs --version
```

### Installation via `nix` package manager

With active [nix](https://nixos.org/) environment:

```bash
cd aspecs
nix-build
nix-env -i $$(readlink result)

# verify installation
aspecs --version
```

## Usage

```bash
# show help
aspecs --help

# validate a file (ast/json format)
aspecs validate --input-ast input.ast
aspecs validate --input-json input.json

# prettify ast file after editing, the file will be overwritten
aspecs prettify input.ast --ast

# show definition fingerprint, for the same definitions
# the fingerprint shall be the same, regardless of the format
aspecs checksum --input-ast input.ast
aspecs checksum --input-json input.json

# convert ast -> json
aspecs convert --input-ast --output-json input.ast > out.json

# convert json -> ast
aspecs convert --input-json --output-ast input.json > out.ast
```

## Development

Install nix package manager, clone repository

```bash
curl -L https://nixos.org/nix/install | sh
git clone https://github.com/zoranbosnjak/asterix-specs.git
cd asterix-specs/aspecs/
```

(Re)build aspecs with `nix-build` and run it

```bash
nix-build
./result/bin/aspecs --help
```

Use `nix-shell` environment

```bash
nix-shell

# monitor changes, auto rebuild on any source change
ghcid "--command=ghci -Wall $$EXTENSIONS -iother -ilib -iapp app/Main.hs"

# run program without rebuild
runhaskell $$EXTENSIONS -iother -ilib -iapp app/Main --help

# (re)build with 'cabal' and run program
cabal build
find . -type f -executable | grep -v "\.so"

exit
```
