{-# LANGUAGE DeriveGeneric #-}

-- Asterix data types.

module Asterix.Specs.Types where

import           GHC.Generics (Generic)
import           Data.Text

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
    } deriving (Generic, Eq, Ord, Show)

data Edition = Edition
    { editionMajor :: Int
    , editionMinor :: Int
    } deriving (Generic, Eq, Show)

instance Ord Edition where
    compare (Edition a1 b1) (Edition a2 b2) =
        compare a1 a2 <> compare b1 b2

data Date = Date
    { dateYear  :: Integer
    , dateMonth :: Int
    , dateDay   :: Int
    } deriving (Generic, Eq, Ord, Show)

data Number
    = NumberZ Integer
    | NumberQ Rational
    | NumberR Rational
    deriving (Generic, Eq, Ord, Show)

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number
    deriving (Generic, Eq, Ord, Show)

data Signed
    = Signed
    | Unsigned
    deriving (Generic, Eq, Ord, Show)

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
    deriving (Generic, Eq, Ord, Show)

data Rule
    = ContextFree Content
    | Dependent [Name] [(Int, Content)]
    deriving (Generic, Eq, Ord, Show)

data Variation
    -- leaf of the structure
    = Element RegisterSize Rule

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
    | Compound (Maybe RegisterSize) [Maybe Item]
    deriving (Generic, Eq, Ord, Show)

data Item
    = Spare RegisterSize
    | Item Name Title Variation Documentation
    deriving (Generic, Eq, Ord, Show)

-- User applicaton profile type
data Uap
    = Uap [Maybe Name]                  -- single UAP
    | Uaps [(UapName, [Maybe Name])]    -- multiple UAPs
    deriving (Generic, Eq, Ord, Show)

-- Basic category definition
data Basic = Basic
    { basCategory   :: Int
    , basTitle      :: Text
    , basEdition    :: Edition
    , basDate       :: Date
    , basPreamble   :: Maybe Text
    , basCatalogue  :: [Item]
    , basUap        :: Uap
    } deriving (Generic, Eq, Ord, Show)

-- Expansion category definition
data Expansion = Expansion
    { expCategory   :: Int
    , expTitle      :: Text
    , expEdition    :: Edition
    , expDate       :: Date
    , expVariation  :: Variation
    } deriving (Generic, Eq, Ord, Show)

data Asterix
    = AsterixBasic Basic
    | AsterixExpansion Expansion
    deriving (Generic, Eq, Ord, Show)

