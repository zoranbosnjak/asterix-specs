{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module:   	Data.Asterix.Types
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines Asterix data types.
--

module Data.Asterix.Types where

import           GHC.Generics (Generic)
import           Data.Ratio (Ratio)
import           Data.Text
import           Data.Word (Word8)

type Name = Text
type Title = Text
type Description = Text
type Remark = Text
type UapName = Text

data Rule a
    = Unspecified
    | ContextFree a
    | Dependent [Name] [(Int, a)]
    deriving (Generic, Eq, Show)

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
    } deriving (Generic, Eq, Show)

data Number
    = NumberZ Integer
    | NumberQ (Ratio Int)
    | NumberR Double
    deriving (Generic, Eq, Ord, Show)

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number
    deriving (Generic, Eq, Ord, Show)

data Signed = Signed | Unsigned deriving (Generic, Eq, Show)

data StringType
    = StringAscii
    | StringICAO
    deriving (Generic, Eq, Show, Read)

data Content
    = ContentTable
        [(Int, Text)]
    | ContentString
        StringType
    | ContentInteger
        Signed
        [Constrain]
    | ContentQuantity
        Signed      -- unsigned/signed
        Number      -- scaling factor
        Int         -- number for fractional bits
        Text        -- unit
        [Constrain]
    deriving (Generic, Eq, Show)

type RegisterSize = Int

data Element
    = Fixed RegisterSize (Rule Content)
    | Group [Subitem]
    | Extended Int Int [Subitem]
    | Repetitive Int Element
    | Explicit
    | Compound [Maybe Subitem]
    | Rfs
    deriving (Generic, Eq, Show)

data Subitem
    = Spare RegisterSize
    | Subitem Name Title (Maybe Description) Element (Maybe Remark)
    deriving (Generic, Eq, Show)

data Encoding = Mandatory | Optional | Absent
    deriving (Generic, Eq, Show, Read)

data Item = Item
    { itemEncoding  :: Rule Encoding
    , itemDefinition :: Text
    , itemSubitem   :: Subitem
    } deriving (Generic, Eq, Show)

data Uap
    = Uap [Maybe Name]
    | Uaps [(UapName, [Maybe Name])]
    deriving (Generic, Eq, Show)

data Asterix = Asterix
    { astCategory   :: Word8
    , astTitle      :: Text
    , astEdition    :: Edition
    , astDate       :: Date
    , astPreamble   :: Maybe Text
    , astCatalogue  :: [Item]
    , astUap        :: Uap
    } deriving (Generic, Eq, Show)

