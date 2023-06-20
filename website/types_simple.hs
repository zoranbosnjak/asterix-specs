type RegisterSize = Int
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

data Number
    = NumberZ Integer
    | NumberQ Rational
    | NumberR Rational

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number

data Signed
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
        Signed
        [Constrain]
    | ContentQuantity
        Signed      -- unsigned/signed
        Number      -- scaling factor
        FractBits   -- number for fractional bits
        Unit        -- unit
        [Constrain]
    | ContentBds
        BdsType

data Rule
    = ContextFree Content
    | Dependent [Name] [(Int, Content)]

data ExtendedType
    = ExtendedRegular
    | ExtendedNoTrailingFx
    deriving (Generic, Eq, Ord, Show)

data RepetitiveType
    -- N bits reserved for REP lengt field
    = RepetitiveRegular RepetitionSize

    -- Number of repetitions are defined by FX bit value
    | RepetitiveFx

data Variation
    -- leaf of the structure
    = Element RegisterSize Rule

    -- concatinated subitems, example:
    -- item 010 is concatinated list of subitems SAC and SIC
    | Group [Item]

    -- extended item with FX extension mechanism
    | Extended ExtendedType RegisterSize RegisterSize [Item]

    -- repetitive item
    | Repetitive RepetitiveType Variation

    -- item with explicit size
    | Explicit

    -- list of subitems with FSPEC mechanism
    -- Some subitems may not be defined in which case the respective
    -- presence bit in the first part is always zero
    | Compound (Maybe RegisterSize) [Maybe Item]

data Item
    = Spare RegisterSize
    | Item Name Title Variation Documentation


data UapSelector = UapSelector
    { selItem :: [Name]             -- UAP depends on this item
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

