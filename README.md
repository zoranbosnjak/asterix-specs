# Asterix specifications

## converter

    - validate `.ast` file
    - convert `.ast` file to `.json`
    - dump `.ast` file as a list of items

## renderer

Render `.json` file to various output formats.
To render `.json` back to `.ast`, use builtin `renderer/formats/ast.py` file.

Example:

```bash
# convert from .ast to .json
converter -f specs/imaginary.ast --json > test.json

# convert from .json to .ast
render --script ast.py RenderAst test.json > test.ast

diff specs/imaginary.ast test.ast
```

## publisher

Render `.json` file to `.rst`, then generate documentation out of it.

Example:

```bash
cat some_specs_file > specs.ast
converter -f specs.ast --json > specs.json
render --script rst.py render specs.json > specs.rst
make html
make latexpdf
```

