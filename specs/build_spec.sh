#! /usr/bin/env bash

while getopts ":hd" opt; do
  case ${opt} in
    h)
        echo "Usage:"
        echo "    $0 -h                Display this help message."
        echo "    $0 file.ast          Process file.ast."
        exit 0
        ;;
    \? )
        echo "Invalid Option: -$OPTARG" 1>&2
        exit 1
  esac
done
shift $((OPTIND -1))

if [ $# -ne 1 ]; then
    echo "Expecting .ast file as argument."
    exit 1
fi

set -e

src=$1
dst="output"

aspecs validate --input-ast $src > /dev/null
cp $src $dst/definition.ast
aspecs convert --input-ast --output-ast $src > $dst/definition.txt
aspecs convert --input-ast --output-json $src > $dst/definition.json
aspecs pandoc --input-ast $src > $dst/definition.pandoc.native
pandoc --metadata title="asterix specification" \
    -f native -t html $dst/definition.pandoc.native \
    -o $dst/definition.html --embed-resources --standalone \
    --css=../style/default.css --css=../style/syntax.css
to-pdf $dst/definition.pandoc.native $dst/definition.pdf

echo "done... result is in $dst" directory
