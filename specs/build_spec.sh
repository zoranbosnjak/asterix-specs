#! /usr/bin/env bash

warnings="--warnings"

while getopts ":hd" opt; do
  case ${opt} in
    h)
        echo "Usage:"
        echo "    $0 -h                Display this help message."
        echo "    $0 file.ast          Process file.ast."
        echo "    $0 -d file.ast       Process file.ast, disable warnings check."
        exit 0
        ;;
    d)
        warnings=""
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

aspecs validate -f $src --ast $warnings > /dev/null
cp $src $dst/definition.ast
aspecs convert -f $src --ast --ast > $dst/definition.txt
aspecs convert -f $src --ast --json > $dst/definition.json
ast-to-rst $dst/definition.ast > $dst/definition.rst
pandoc --metadata title="asterix specification" -f rst -t html $dst/definition.rst -o $dst/definition.html --embed-resources --standalone --css=../website/css/default.css --css=../website/css/syntax.css
rst-to-pdf $dst/definition.rst $dst/definition.pdf

echo "done... result is in $dst" directory

