# Asterix specifications

The all-purpose structured EUROCONTROL surveillance information exchange (ASTERIX)
in a parsable format.

## Abstract structure

This project assumes speciffic abstract structure for asterix definition.
Basic elements are **Item** and **Variation**, which are defined
in terms of mutual recursion.

```
RegisterSize = Int
PrimarySize = Int
ExtensionSize = Int
RepetitionSize = Int

Name = Text
Title = Text

Content is one of: ...

Item is one of:
    * (SpareItem, RegisterSize)
    * (RegularItem, Name, Title, Variation)

Variation is one of:
    * (Element, RegisterSize, Content)
    * (Group, list_of[Item])
    * (Extended, PrimarySize, ExtensionSize, list_of[Item])
    * (Repetitive, RepetitionSize, Variation)
    * (Explicit)
    * (Compound, list_of[Optional Item])

Catalogue = list_of [Item]
Uap = list_of [Name]
```

### Example

![asterix example](/doc/asterix-example.svg.png)

See [structure description][structure] for details.

# Primary supported formats

All primary supported formats are identical in respect to the information they contain.
It is always possible to transparently convert from one primary format to another.

## Slim custom format (.ast)

Each asterix category is described in specific `.ast` text file.
See `syntax/*ps` files for detailed syntax.

## JSON format (.json)

JSON is well supported format, so it is easy to include asterix specifications
in another project.

See `renderer` as an example `python` application, extracting information out
of the `.json` format.


# converter

This is a reference parser and converter for all supported primary file formats.

Features:

- validate sources file
- convert from one format to another
- convert from one format to itself (pretify definition)
- dump file as a list of items

Conversion process works in following steps:

- parse input file to internal (abstract) structure
- validate a structure
- generate required output format from internal structure

Converter is modular. To extend the converter to support other formats,
functions `encodeAsterix` and `decodeAsterix` must be implemented.

Abstract internal structure remains the same for all formats.
See [structure description][structure] for details.

# Format isomorphism

2 formats are *isomorphic* if there is a way to translate from one format to another
in both directions. This property can be verified by the converter:

- run a converter once, to get a *normal* version of the file (pretify the file)
- compare original with normal version (difference is possible, spaces, indents, comments...)
- convert normal version again (new file should be exactly the same)
- convert from normal version to other format
- convert from other format back (the result should be exactly the same)

All primary formats are *isomorphic*.

# Other non-primary formats

Conversion to other formats is possible, but in this case, there is no need for
conversion in the oposite direction.

## Restructured text format (.rst)

This is an intermediate format to generate human readable documentation (pdf, html).

`.rst` file is automatically generated out of coresponding `.json` file, using a renderer.
A documentation tool (like `sphinx-doc`) is in turn used, to generate PDF and HTML.

## Minimal json format (.json)

This is an example format where all remarks and notes are removed from the
original json.

## Custom formats

It is possible to generate (render) any other file format as required by the application,
for example `.xml` or `.json` with some other naming schema, `c/c++` header files...


# renderer

Render `.json` primary file to various output formats.

Example:

```bash
# convert from .ast to .json
converter -f specs/imaginary.ast --ast --json > test.json

# render from .json to .ast
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

[structure]: ./STRUCTURE.md

