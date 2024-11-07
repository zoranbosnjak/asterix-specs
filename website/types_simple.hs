newtype CatNum = CatNum Int
newtype BitSize = BitSize Int
newtype ByteSize = ByteSize Int
newtype ItemName = ItemName Text
newtype Title = Title Text
newtype UapName = UapName Text
newtype Unit = Unit Text
newtype ItemPath = ItemPath [ItemName]

data Documentation = Documentation
    { docDefinition  :: Maybe Text
    , docDescription :: Maybe Text
    , docRemark      :: Maybe Text
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
    -- N bytes reserved for REP lengt field
    = RepetitiveRegular ByteSize

    -- Number of repetitions are defined by FX bit value
    | RepetitiveFx

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose

data Variation offset
    -- leaf of the structure
    = Element offset BitSize (Rule Content)

    -- concatinated subitems, example:
    -- item 010 is concatinated list of subitems SAC and SIC
    | Group offset [Item offset]

    -- extended item with FX extension mechanism
    | Extended [Maybe (Item offset)]

    -- repetitive item
    | Repetitive RepetitiveType (Variation offset)

    -- item with explicit size
    | Explicit (Maybe ExplicitType)

    -- list of subitems with FSPEC mechanism
    -- Some subitems may not be defined in which case the respective
    -- presence bit in the first part is always zero
    | Compound [Maybe (NonSpare offset)]

data NonSpare offset = NonSpare ItemName Title (Rule (Variation offset)) Documentation

data Item offset
    = Spare offset BitSize
    | Item (NonSpare offset)

data UapItem a
    = UapItem a
    | UapItemSpare
    | UapItemRFS

data UapSelector = UapSelector
    { selItem  :: ItemPath           -- UAP depends on this item
    , selTable :: [(Int, UapName)]  -- value lookup table
    }

-- User applicaton profile type
data Uap r
    -- single UAP
    = Uap r

    -- multiple UAPs
    | Uaps [(UapName, r)] (Maybe UapSelector)

-- Basic category definition
data Basic = Basic
    { basCategory  :: CatNum
    , basTitle     :: Title
    , basEdition   :: Edition
    , basDate      :: Date
    , basPreamble  :: Maybe Text
    , basCatalogue :: [NonSpare ()]
    , basUap       :: Uap [UapItem ItemName]
    }

-- Expansion category definition
data Expansion = Expansion
    { expCategory  :: CatNum
    , expTitle     :: Title
    , expEdition   :: Edition
    , expDate      :: Date
    , expFspecSize :: Maybe ByteSize
    , expItems     :: [Maybe (NonSpare ())]
    }

data Asterix
    = AsterixBasic Basic
    | AsterixExpansion Expansion
