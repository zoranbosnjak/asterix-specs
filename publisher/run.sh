#!/usr/bin/env bash

# run inside nix-shell

set -e

if [[ $# -eq 0 ]]; then
    echo 'Input spec file is required.'
    exit 1
fi

cat $1 > specs.ast
converter -f specs.ast --json > specs.json
render --script rst.py render specs.json > specs.rst
make html
make latexpdf

