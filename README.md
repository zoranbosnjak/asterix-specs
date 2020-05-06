# Asterix specifications

The all-purpose structured EUROCONTROL surveillance information exchange (ASTERIX)
in a parsable format.

## Source format (.ast)

Each asterix category is described in specific `.ast` text file.
See `syntax/*ps` files for detailed syntax.

## JSON format (.json)

This is an intermediate format which is directly derived from the `.ast` files,
using a reference parser (`convert --ast --json`). JSON is well supported format,
so it is easy to include asterix specifications in another project.

See `renderer` as an example `python` application.

`.ast` and `.json` files are identical in respect to the information they contain.

## Restructured text format (.rst)

This is an intermediate format to generate human readable documentation (pdf, html).
`.rst` file is automatically generated out of coresponding `.json` file, using a renderer.

## Custom formats

It is possible to generate (render) any other file format as required by the application,
for example `.xml` or a `.json` with some other naming schema.

In this case, a specific renderer function is required.

# converter

This is a reference parser for various file formats (`.ast`, `.json`,...).

    - validate file
    - convert from one format to another
    - dump file as a list of items

# renderer

Render `.json` file to various output formats.
To render `.json` back to `.ast`, use builtin `renderer/formats/ast.py` file.

Example:

```bash
# convert from .ast to .json
converter -f specs/imaginary.ast --ast --json > test.json

# convert from .json to .ast
render --script ast.py render test.json > test.ast

diff specs/imaginary.ast test.ast
```

# publisher

Render `.json` file to `.rst`, then generate documentation out of it.

Example:

```bash
cat some_specs_file > specs.ast
converter -f specs.ast --ast --json > specs.json
render --script rst.py render specs.json > specs.rst
make html
make latexpdf
```

