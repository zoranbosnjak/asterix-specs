#!/usr/bin/env bash

# run inside nix-shell

set -e

if [[ $# -ne 2 ]]; then
    echo 'Required arguments: [filename] [fileformat]'
    exit 1
fi

cat $1 > specs.input
aspecs -f specs.input $2 --json > specs.json
render --script rst.py render specs.json > specs.rst
make html
make latexpdf

