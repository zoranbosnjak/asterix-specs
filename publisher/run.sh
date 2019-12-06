#!/usr/bin/env bash

# run inside nix-shell

set -e

if [[ $# -eq 0 ]]; then
    echo 'Input spec file is required.'
    exit 1
fi

converter -f $1 --json > specs.json
render --script rst.py RenderRst specs.json > specs.rst
make html
make latexpdf

