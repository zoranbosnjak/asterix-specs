---
title: Structure
---

# Asterix structure

Basic principles of asterix are described in the
[part 1 document](https://www.eurocontrol.int/publication/eurocontrol-specification-surveillance-data-exchange-part-i).
This project reuses the definitions and vocabulary to the maximum possible extent.

## Example

![asterix example](/images/asterix-example.svg.png)

## Definition of terms

* **asterix category** is a top-level structure containing complete specification.

* **category number** is a number between 0 and 255.

* **edition** is a pair of numbers `major`.`minor`. *Major* and *minor* numbers have no particular meaning
  except to determine the order of versions of a category.

* **preamble** represents an arbitrary text to describe a category.

* **item catalogue** is a list of asterix items which represents the main part of the definition.

* **UAP** - user application profile - is ordered selection of items from the **item catalogue**.

* **asterix item** - is a basic building block to define asterix structure.

* **subitem** - means the same as **item**, but lower in the hierarchical item structure.

* **item type** or **variation** - possible variations are: **element**, **compound**, **extended**...

* **content** is the interpretation of bits: **raw**, **integer**, **table**...

## Abstract structure

**Asterix** is a structure containing:

- category number (Integer)
- title (Text)
- edition
- date
- preamble (Text)
- catalogue (list of Items)
- uap (one or more UAP definitions)

**Item** is a structure containing:

- name or item identifier (String, normally short)
- title (Text)
- definition (Optional Text)
- description (Optional Text)
- Variation
- remark (Optional Text)

**Item** can also be empty (occupying defined number of spare bits).

**Variation** (or **ItemType**) could be:

- Element (containing number of bits and content definition)
- Group (containing list of Items)
- Extended (containing list of Items and position of FX bits)
- Repetitive (containing number of bits for the first part and recursive ItemType definition)
- Explicit (no further information attached)
- Compound (containing list of Items where some items may be missing)

Remark:

> **Item** and **Variation** are defined in terms of mutual recursion.

**Content** could be:

- *Raw* value
- *Table* of possible values
- *Ascii*, *ICAO* or *Octal String*
- *Signed* or *Unsigned Integer*
- *Signed* or *Unsigned Quantity* (including units)
- *BDS* register (with or without address)

Remark:

> This structure makes distinction between *Raw* value and *Unsigned Integer*.
> A *raw* value is a content in which bits do not have any particular meaning,
> other than just representing some *identifier* (this is true in most
> cases, for example the *SAC/SIC* code).
>
> *Unsigned Integer* is similar to *Raw* value, however semantically there is a
> small difference.
> In strongly typed environments, the compiler can prevent some irregular
> operations over *Raw* values. For example: it makes no sense to
> *add/multiply* two identifiers, whereas the same arithmetic operations over
> *unsigned integers* are well defined. One example on this is a content that
> represents *seconds*.

## Validation rules

For each defined item or subitem, the following validation rules are
enforced by the [validator](/aspecs.html):

- Tables shall not contain duplicated entries.
- Each table entry shall contain non-empty description.
- Number of element bits must be sufficient for a specified table size,
  the table however does not need to be completely defined.
- First word in each table row description shall be capitalized ("Like this").
- No dot '.' is expected at the end of row description.
- "ascii", "icao", "octal" strings shall have (8, 6, 3) bits per
  character respectively.
- "scaling factor" and number of "fractional bits" are checked if
  optimally defined, for example, the scaling factor of "0.5" is
  suggested to be redefined as "1" fractional bit, to avoid potential rounding error.
- Values with "negative constraints" shall be of "signed" type.
- BDS values are expected to be "64" bits long if the address is included or
  "56" bits long otherwise.
- Defined subitems within each (sub)items shall have non-duplicated names.
- Element size must be 'positive'.
- All elements together within a 'Group' shall be 8-bit aligned.
- A 'Group' shall contain more than 1 element
  (single element group shall be simplified to just 'Element').
- Elements sizes within 'Extended' item shall be properly bit-aligned
  and "a-priory" known.
- Extended items shall contain more than one subitem. In the case of
  a single subitem (with FX bit), use 'repetitive fx'.
- Repetitive item shall specify a "REP" factor size, which must
  be positive and 8-bit aligned.
- Repetitive item content shall be 8-bit aligned.
- Compound item as a whole shall be 8-bit aligned.
- Spare item shall have a positive bit size.
- Short item names shall be `<= 15` character long.
- Short item names shall only contain characters "A-Z" and digits "0-9"
  (for example "SAC", "010", ...).
- Item title shall be all capitalized ("Like This Example"), see the source
  code for some short words exceptions, like "in", "and", "of"...
- A dot at the end of item title is not expected.
- Category number shall be within range `[0,255]`.
- All defined items in a category shall be referenced by the 'UAP'.
- All items referenced by the 'UAP' shall be defined.
- Top level items (like subitems) shall not be duplicated.
- Items listed in UAP shall not be duplicated.
- In case of multiple UAPs, each UAP requires a unique name
  (example: "plot", "track" for cat001).
- In case of non-context free item definitions, the depending item
  shall be defined, for example: the meaning of item
  `I062/380/IAS/IAS` depends on the current value of `I062/380/IAS/IM`.
  The validation requires that `I062/380/IAS/IM` is also defined and
  should be of a fixed size. It is also checked that the number of cases does
  not exceed the value that size can represent.
- Subitem names shall be non-repetitive, in respect to the parent item name,
  for example: "POSITION/POSITIONX" is suggested to change to "POSITION/X".

## Asterix definition in `haskell` syntax

The essence of asterix definition is recursively encoded in `Item` and it's `Variation`.

```haskell
$typesSimple$
```

See [source code](https://github.com/zoranbosnjak/asterix-specs)
for exact definition.

## Non-backward compatible changes

### 2024-07-09 - cleanup

Parent git release: `#d3cccd1`

- project cleanup
- better RFS handling
- additional `NonSpare` type
- `byte size` instead of `bit size`, where applicable
- Ast syntax contains only minimal changes
- JSON structure follows internal structure and is incompatible
  with previous version. A converter script is available for
  legacy application (convert from new to old json format).
  See `migrations/2024-07-cleanup/convertspec.py`.

### 2024-02-26 - generalized 'Rule'

Parent git release: `#941f5ce`

The `Rule` type is extended, to support `I004/120/CC` content cases.

`.ast` syntax is extended to include 'default' value, `.json` syntax requires
minor update (change in field types and names). See changes in:

- `aspecs/lib/Asterix/Specs/Types.hs`
- `aspecs/lib/Asterix/Specs/Syntax/Ast.hs`
- `aspecs/lib/Asterix/Specs/Syntax/Json.hs`

### 2023-12-18 - precise numeric expression

Parent git release: `#a7c135f`

The `Number` type is simplified and generalized, to allow precise
numeric expression, without rounding error. Ast syntax becomes more
readable, for example:

- `0.1 0` -> `1/10`
- `180 31` -> `180/2^31`

See [FAQ](/faq.html) for expression evaluation details.

### 2023-09-18 - extended item modification, fx bit is explicit

Parent git release: `#53a30f4`

Extended item structure is simplified to list of items with possible
spare slots ('fx' bits).

### 2023-07-24 - explicit item modification, rfs item added

Parent git release: `#5c263cb`

Explicit item has optional (RE/SP) type added,
such that RE items can be automatically processed.

Rfs item type is introduced.

### 2023-06-20 - extended/repetitve item modification

Parent git release: `#3d75e32`

Extended items with a single subitem are not allowed.
In those cases, the items are more similar to 'repetitive',
but with the 'fx' extension mechanism.

New 'RepetitiveType' data type is introduced to handle the case.
