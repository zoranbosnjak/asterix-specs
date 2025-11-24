-- Asterix data types.
--
-- Remark: 'offset' type parameter in some types below is required, to be able
-- to keep the specs stored on disk without offset, for example 'NonSpare ()'.
-- The same structure could be later used with the actual offset value
-- calculated, for example 'NonSpare BitOffset' for code generation.

module Asterix.Specs.Types where

import           Data.Text
import           GHC.Generics (Generic)

newtype CatNum = CatNum Int
    deriving (Generic, Eq, Ord, Show, Read)

newtype BitSize = BitSize Int
    deriving (Generic, Eq, Ord, Show, Read)

instance Semigroup BitSize where BitSize a <> BitSize b = BitSize (a+b)
instance Monoid BitSize where mempty = BitSize 0

newtype ByteSize = ByteSize Int
    deriving (Generic, Eq, Ord, Show, Read)

instance Semigroup ByteSize where ByteSize a <> ByteSize b = ByteSize (a+b)
instance Monoid ByteSize where mempty = ByteSize 0

newtype ItemName = ItemName Text
    deriving (Generic, Eq, Ord, Show, Read)

newtype Title = Title Text
    deriving (Generic, Eq, Ord, Show, Read)

newtype UapName = UapName Text
    deriving (Generic, Eq, Ord, Show, Read)

newtype Unit = Unit Text
    deriving (Generic, Eq, Ord, Show, Read)

newtype ItemPath = ItemPath [ItemName]
    deriving (Generic, Eq, Ord, Show, Read)

data Documentation = Documentation
    { docDefinition  :: Maybe Text
    , docDescription :: Maybe Text
    , docRemark      :: Maybe Text
    } deriving (Generic, Eq, Ord, Show, Read)

data Edition = Edition
    { editionMajor :: Int
    , editionMinor :: Int
    } deriving (Generic, Eq, Show, Read)

instance Ord Edition where
    compare (Edition a1 b1) (Edition a2 b2) =
        compare a1 a2 <> compare b1 b2

data Date = Date
    { dateYear  :: Integer
    , dateMonth :: Int
    , dateDay   :: Int
    } deriving (Generic, Eq, Ord, Show, Read)

data Number
    = NumInt Integer
    | NumDiv Number Number
    | NumPow Integer Integer
    deriving (Generic, Eq, Ord, Show, Read)

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number
    deriving (Generic, Eq, Ord, Show, Read)

data Signedness
    = Signed
    | Unsigned
    deriving (Generic, Eq, Ord, Show, Read)

data StringType
    = StringAscii
    | StringICAO
    | StringOctal
    deriving (Generic, Eq, Ord, Show, Read)

newtype BdsAddr = BdsAddr Int
    deriving (Generic, Eq, Ord, Show, Read)

data BdsType
    = BdsWithAddress        -- 64 bit value (address is encoded with data)
    | BdsAt (Maybe BdsAddr) -- 56 bit value (address is maybe a priory known)
    deriving (Generic, Eq, Ord, Show, Read)

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
    deriving (Generic, Eq, Ord, Show, Read)

data Rule a
    = ContextFree a
    | Dependent
        [ItemPath]   -- items that this rule depends on
        a            -- default value
        [([Int], a)] -- cases
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data RepetitiveType
    -- N bytes reserved for REP lengt field
    = RepetitiveRegular ByteSize

    -- Number of repetitions are defined by FX bit value
    | RepetitiveFx
    deriving (Generic, Eq, Ord, Show, Read)

data ExplicitType
    = ReservedExpansion
    | SpecialPurpose
    deriving (Generic, Eq, Ord, Show, Read)

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
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data NonSpare offset
    = NonSpare ItemName Title (Rule (Variation offset)) Documentation
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data Item offset
    = Spare offset BitSize
    | Item (NonSpare offset)
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data UapItem a
    = UapItem a
    | UapItemSpare
    | UapItemRFS
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data UapSelector = UapSelector
    { selItem  :: ItemPath           -- UAP depends on this item
    , selTable :: [(Int, UapName)]  -- value lookup table
    } deriving (Generic, Eq, Ord, Show, Read)

-- User applicaton profile type
data Uap r
    -- single UAP
    = Uap r

    -- multiple UAPs
    | Uaps [(UapName, r)] (Maybe UapSelector)
    deriving (Generic, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- Basic category definition
data Basic = Basic
    { basCategory  :: CatNum
    , basTitle     :: Title
    , basEdition   :: Edition
    , basDate      :: Date
    , basPreamble  :: Maybe Text
    , basCatalogue :: [NonSpare ()]
    , basUap       :: Uap [UapItem ItemName]
    } deriving (Generic, Eq, Ord, Show, Read)

-- Expansion category definition
data Expansion = Expansion
    { expCategory  :: CatNum
    , expTitle     :: Title
    , expEdition   :: Edition
    , expDate      :: Date
    , expFspecSize :: Maybe ByteSize
    , expItems     :: [Maybe (NonSpare ())]
    } deriving (Generic, Eq, Ord, Show, Read)

data Asterix
    = AsterixBasic Basic
    | AsterixExpansion Expansion
    deriving (Generic, Eq, Ord, Show, Read)
