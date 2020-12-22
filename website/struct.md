---
title: Structure
---

# Asterix structure

Basic principles of asterix are described in the
[part 1 document](https://www.eurocontrol.int/publication/eurocontrol-specification-surveillance-data-exchange-part-i).
This project reuses the definitions and vocabulary as much as possible.

## Example

![asterix example](/images/asterix-example.svg.png)

## Definition of terms

* **asterix category** is a toplevel structure containing complete specification.

* **category number** is a number between 0 and 255.

* **edition** is a pair of numbers `major`.`minor`. *Major* and *minor* numbers have no particular meaning
  except to determine the order of versions of a category.

* **preamble** represents an arbitrary text to describe a category.

* **item catalogue** is a list of asterix items which represents the main part of the definition.

* **UAP** - user application profile - ordered selection of items from the **item catalogue**.

* **asterix item** - a basic building block to define asterix structure.

* **subitem** - same as **item**, but lower in the hierarchical item structure.

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

- name or item identifier (String, normaly short)
- title (Text)
- definition (Optional Text)
- description (Optional Text)
- Variation
- remark (Optional Text)

**Item** can also be empty (occupying defined number of spare bits).

**Variation** (or **ItemType**) is one of:

- Element (containing number of bits and content definition)
- Group (containing list of Items)
- Extended (containing list of Items and position of FX bits)
- Repetitive (containing number of bits for the first part and recursive ItemType definition)
- Explicit (no further information attached)
- Compound (containing list of Items where some items may be missing)

Remark:

> **Item** and **Variation** are defined in terms of mutual recursion.

**Content** is one of:

- *Raw* value
- *Table* of possible values
- *Ascii*, *ICAO* or *Octal String*
- *Signed* or *Unsigned Integer*
- *Signed* or *Unsigned Quantity* (including units)
- *BDS*

Remark:

> This structure makes distinction between *Raw* value and *Unsigned Integer*.
> A *raw* value is a content where the bits don't have any particular meaning,
> other then just representing some *identifier* (this is true in most of the
> cases, for example the *SAC/SIC* code).
>
> *Unsigned Integer* is similar, however semantically there is a small difference.
> In strongly typed environments, the compiler can prevent some irregular
> operations over *raw* values. For example: it makes no sense to
> *add/multiply* 2 identifiers, whereas the same arithmetic operations over
> *unsigned integers* are well defined. One such example is a content that
> represents *seconds*.

## Asterix definition in `haskell` syntax

The essence of asterix definition is recursively encoded in `Item` and it's `Variation`.

```haskell
type RegisterSize = Int
type PrimarySize = Int
type ExtensionSize = Int
type RepetitionSize = Int
type FractBits = Int

type Name = Text
type Title = Text
type UapName = Text
type Unit = Text

data Documentation = Documentation
    { docDefinition     :: Maybe Text
    , docDescription    :: Maybe Text
    , docRemark         :: Maybe Text
    }

data Edition = Edition
    { editionMajor :: Int
    , editionMinor :: Int
    }

data Date = Date
    { dateYear  :: Integer
    , dateMonth :: Int
    , dateDay   :: Int
    }

data StringType
    = StringAscii
    | StringICAO
    | StringOctal

data Content
    = ContentRaw
    | ContentTable
        [(Int, Text)]
    | ContentString
        StringType
    | ContentInteger
        Signed
        [Constrain]
    | ContentQuantity
        Signed      -- unsigned/signed
        Number      -- scaling factor
        FractBits   -- number for fractional bits
        Unit        -- unit
        [Constrain]
    | ContentBds

data Variation
    -- leaf of the structure
    = Element RegisterSize (Rule Content)

    -- concatinated subitems, example:
    -- item 010 is concatinated list of subitems SAC and SIC
    | Group [Item]

    -- extended item with FX extension mechanism
    | Extended PrimarySize ExtensionSize [Item]

    -- N bits reserved for REP lengt field, followed by recursive variation
    | Repetitive RepetitionSize Variation

    -- item with explicit size
    | Explicit

    -- list of subitems with FSPEC mechanism
    -- Some subitems may not be defined in which case the respective
    -- presence bit in the first part is always zero
    | Compound [Maybe Item]

data Item
    = Spare RegisterSize
    | Item Name Title Variation Documentation

-- User applicaton profile type
data Uap
    = Uap [Maybe Name]                  -- single UAP
    | Uaps [(UapName, [Maybe Name])]    -- multiple UAPs

data Asterix = Asterix
    { astCategory   :: Int              -- category number
    , astTitle      :: Text             -- title
    , astEdition    :: Edition          -- edition
    , astDate       :: Date             -- release date
    , astPreamble   :: Maybe Text       -- optional preamble text
    , astCatalogue  :: [Item]           -- the cataloguq
    , astUap        :: Uap              -- one or more UAPs
    }
```

See [tools source code](https://github.com/zoranbosnjak/asterix-specs/blob/master/tools/src/Data/Asterix/Types.hs)
for exact definition.

