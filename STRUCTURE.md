# Basic asterix structure

Asterix specifications contain recursive definition of (sub)items.

## Definition of terms

* **asterix category** is a toplevel structure containing complete specification.

* **category number** is a number between 0 and 255.

* **edition** is a pair of numbers major.minor. Major and minor numbers have no particular meaning
  except to determine the order of versions of a category.

* **preamble** represents an arbitrary text

* **item catalogue** is a list of asterix items

* **UAP** - user application profile - ordered selection of items from the **item catalogue**.

* **asterix item** - a basic building block to define asterix structure

* **subitem** - same as **item**, but lower in the hierarchical item structure.

* **item type** or **variation** - possible variations are: **element**, **compound**, **extended**...

## The structure

**Asterix** is a structure containing:

- category number (Integer)
- title (Text)
- edition
- date
- preamble (Text)
- catalogue (list of Items)
- uap (one or more UAP definitions)

**Item** is a structure containing:

- encoding rule (mandatory or optional)
- definition text
- (sub)item details

**Subitem** is a structure containing:

- name or identifier (String, normaly short)
- title (Text)
- description (Optional Text)
- ItemType
- remark (Optional Text)

**Subitem** can also be empty (occupying defined number of spare bits).

**ItemType** is one of:

- Element (containing number of bits and content definition)
- Group (containing list of Subitems)
- Extended (containing list of Subitems and position of FX bits)
- Repetitive (containing number of bits for the first part and recursive ItemType definition)
- Explicit (no further information attached)
- Compound (containing list of Subitems where some subitems may be missing)

Remark:

> **Subitem** and **ItemType** are defined in terms of mutual recursion.

## Formal asterix model definition in `haskell` syntax

`Text`, `Int`, `Integer` are assumed to be basic types.
The rest of the structures definition is either:

- a type synonym
- product type (record)
- sum type (multiple options)

See [Wikipedia article][adt] for more information about algebraic data types.

The essence of asterix definition is recursively encoded in `Subitem` and it's `Variation`.

```haskell
-- Asterix structure definition

type Name = Text
type Title = Text
type Description = Text
type Remark = Text
type UapName = Text

data Edition = Edition
    { editionMajor :: Int
    , editionMinor :: Int
    }

data Date = Date
    { dateYear  :: Integer
    , dateMonth :: Int
    , dateDay   :: Int
    }

type RegisterSize = Int

data Content
    = Number
    | TableOfValues
    -- some more

data Variation

    -- leaf of the structure
    = Element RegisterSize Content

    -- concatinated subitems, example:
    -- subitem 010 is concatinated list of subitems SAC and SIC
    | Group [Subitem]

    -- number of always present bits (ending with FX)
    -- followed by number of extension bits (ending with FX)
    -- followed by list of subitems
    -- FX extension mechanism is in use
    | Extended Int Int [Subitem]

    -- N bytes reserved for size, followed by recursive variation
    | Repetitive Int Variation

    -- subitem with explicit size (leaf of the structure)
    | Explicit

    -- list of subitems with FSPEC mechanism
    -- Some subitems may not be defined in which case the respective
    -- presence bit in the first part is always zero
    | Compound [Maybe Subitem]

data Subitem
    = Spare RegisterSize
    | Subitem Name Title (Maybe Description) Variation (Maybe Remark)

-- Encoding rule
data Encoding
    = Mandatory
    | Optional

-- Item is a non-spare subitem,
-- with additional definition text and encoding rule.
data Item = Item
    { itemEncoding  :: Encoding
    , itemDefinition :: Text
    , itemSubitem   :: Subitem
    }

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

[adt]: https://en.wikipedia.org/wiki/Algebraic_data_type

