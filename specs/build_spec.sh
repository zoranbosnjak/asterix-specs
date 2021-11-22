#! /usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Expecting .ast file as argument."
    exit 1
fi

set -e

src=$1
dst="output"

aspecs validate -f $src --ast --warnings > /dev/null
cp $src $dst/definition.ast
aspecs convert -f $src --ast --ast > $dst/definition.txt
aspecs convert -f $src --ast --json > $dst/definition.json
json-to-rst $dst/definition.json > $dst/definition.rst
pandoc --metadata title="asterix specification" -f rst -t html $dst/definition.rst -o $dst/definition.html --self-contained --css=../website/css/default.css --css=../website/css/syntax.css
rst-to-pdf $dst/definition.rst $dst/definition.pdf

echo "done... result is in $dst" directory

