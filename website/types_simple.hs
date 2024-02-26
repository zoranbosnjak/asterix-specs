type RegisterSize = Int
type RepetitionSize = Int
type FractBits = Int

type Name = Text
type Title = Text
type UapName = Text
type Unit = Text
type ItemPath = [Name]

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

data Number
    = NumInt Integer
    | NumDiv Number Number
    | NumPow Integer Integer

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number

data Signedness
    = Signed
    | Unsigned

data StringType
    = StringAscii
    | StringICAO
    | StringOctal

newtype BdsAddr = BdsAddr Int

data BdsType
    = BdsWithAddress        -- 64 bit value (address is encoded with data)
    | BdsAt (Maybe BdsAddr) -- 56 bit value (address is maybe a priory known)

data Content
    = ContentRaw
    | ContentTable
        [(Int, Text)]
    | ContentString
        StringType
    | ContentInteger
        Signedness
        [Constrain]
    | ContentQuantity
        Signedness  -- unsigned/signed
        Number      -- lsb
        Unit        -- unit
        [Constrain]
    | ContentBds
        BdsType

data Rule a
    = ContextFree a
    | Dependent
        [ItemPath]   -- items that this rule depends on
        a            -- default value
        [([Int], a)] -- cases

data RepetitiveType
    -- N bits reserved for REP lengt field
    = RepetitiveRegular RepetitionSize

    -- Number of repetitions are defined by FX bit value
    | RepetitiveFx

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose

data Variation
    -- leaf of the structure
    = Element RegisterSize (Rule Content)

    -- concatinated subitems, example:
    -- item 010 is concatinated list of subitems SAC and SIC
    | Group [Item]

    -- extended item with FX extension mechanism
    | Extended [Maybe Item]

    -- repetitive item
    | Repetitive RepetitiveType Variation

    -- item with explicit size
    | Explicit (Maybe ExplicitType)

    -- random field sequencing
    | RandomFieldSequencing

    -- list of subitems with FSPEC mechanism
    -- Some subitems may not be defined in which case the respective
    -- presence bit in the first part is always zero
    | Compound (Maybe RegisterSize) [Maybe Item]

data Item
    = Spare RegisterSize
    | Item Name Title (Rule Variation) Documentation

data UapSelector = UapSelector
    { selItem :: ItemPath           -- UAP depends on this item
    , selTable :: [(Int, UapName)]  -- value lookup table
    }

-- User applicaton profile type
data Uap
    -- single UAP
    = Uap [Maybe Name]

    -- multiple UAPs
    | Uaps [(UapName, [Maybe Name])] (Maybe UapSelector)


-- Basic category definition
data Basic = Basic
    { basCategory   :: Int
    , basTitle      :: Text
    , basEdition    :: Edition
    , basDate       :: Date
    , basPreamble   :: Maybe Text
    , basCatalogue  :: [Item]
    , basUap        :: Uap
    }

-- Expansion category definition
data Expansion = Expansion
    { expCategory   :: Int
    , expTitle      :: Text
    , expEdition    :: Edition
    , expDate       :: Date
    , expVariation  :: Variation
    }

data Asterix
    = AsterixBasic Basic
    | AsterixExpansion Expansion

