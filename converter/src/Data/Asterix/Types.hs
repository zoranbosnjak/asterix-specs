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
import           Data.Ratio (Ratio, numerator, denominator)
import           Data.Text
import           Data.Char
import qualified Text.Printf as TP
import           Data.Word (Word8)
import           Data.Aeson (ToJSON, toJSON, object, (.=))

type ItemName = String
type ItemTitle = Text
type Description = Text
type Remark = Text
type UapName = String

data Rule a
    = ContextFree a
    | Dependent [ItemName] [(Int, a)]
    deriving (Generic, Eq, Show)

instance ToJSON a => ToJSON (Rule a)
  where
    toJSON (ContextFree rule) = object
        [ "type" .= ("ContextFree" :: String)
        , "rule" .= rule
        ]
    toJSON (Dependent item rules) = object
        [ "type" .= ("Dependent" :: String)
        , "item" .= item
        , "rules" .= rules
        ]

data Edition = Edition
    { editionMajor :: Int
    , editionMinor :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON Edition where
    toJSON (Edition a b) = toJSON $ show a ++ "." ++ show b

data Date = Date
    { dateYear :: Int
    , dateMonth :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON Date where
    toJSON (Date y m) = toJSON $ show y ++ "-" ++ TP.printf "%02d" m

data Number
    = NumberZ Int
    | NumberQ (Ratio Int)
    | NumberR Double
    deriving (Generic, Eq, Show)

instance ToJSON Number where
    toJSON = \case
        NumberZ val -> toJSON $ "Natural " ++ show val
        NumberQ val -> toJSON $ "Ratio " ++ show (numerator val) ++ " " ++ show (denominator val)
        NumberR val -> toJSON $ "Real " ++ show val

data Limit
    = Including Number
    | Excluding Number
    deriving (Generic, Eq, Show)

instance ToJSON Limit where
    toJSON (Including x) = object ["including" .= True, "limit" .= x]
    toJSON (Excluding x) = object ["including" .= False, "limit" .= x]

data Quantity = Quantity
    { qScale    :: Number   -- scaling factor
    , qFract    :: Int      -- number of fractional bits
    , qUnit     :: Maybe Text
    , qLimitLo  :: Maybe Limit
    , qLimitHi  :: Maybe Limit
    } deriving (Generic, Eq, Show)

instance ToJSON Quantity where
    toJSON (Quantity scale fract unit limLow limHigh) = object
        [ "scaling" .= scale
        , "fractionalBits" .= fract
        , "unit"    .= unit
        , "lowLimit" .= limLow
        , "highLimit" .= limHigh
        ]

data Content
    = Raw
    | Unsigned Quantity
    | Signed Quantity
    | Table [(Int, Text)]
    | StringAscii
    | StringICAO
    deriving (Generic, Eq, Show)

instance ToJSON Content where
    toJSON = \case
        Raw -> object
            [ "type" .= ("Raw" :: String)
            ]
        Unsigned q -> object
            [ "type" .= ("Unsigned" :: String)
            , "quantity" .= q
            ]
        Signed q -> object
            [ "type" .= ("Signed" :: String)
            , "quantity" .= q
            ]
        Table lst -> object
            [ "type" .= ("Table" :: String)
            , "values" .= lst
            ]
        StringAscii -> object
            [ "type" .= ("StringAscii" :: String)
            ]
        StringICAO -> object
            [ "type" .= ("StringICAO" :: String)
            ]

type RegisterSize = Int

data Variation
    = Fixed RegisterSize (Rule Content)
    | Group [Item]
    | Extended Int Int [Item]
    | Repetitive Variation
    | Explicit
    | Compound [Item]
    | Rfs
    deriving (Generic, Eq, Show)

instance ToJSON Variation where
    toJSON (Fixed n x) = object
        [ "type"    .= ("Fixed" :: String)
        , "size"    .= n
        , "value"   .= x
        ]
    toJSON (Group lst) = object
        [ "type"    .= ("Group" :: String)
        , "items"   .= lst
        ]
    toJSON (Extended n1 n2 lst) = object
        [ "type"    .= ("Extended" :: String)
        , "first"   .= n1
        , "extents" .= n2
        , "items"   .= lst
        ]
    toJSON (Repetitive i) = object
        [ "type"    .= ("Repetitive" :: String)
        , "item"    .= i
        ]
    toJSON Explicit = object
        [ "type"    .= ("Explicit" :: String)
        ]
    toJSON (Compound lst) = object
        [ "type"    .= ("Compound" :: String)
        , "items"   .= lst
        ]
    toJSON _ = undefined -- TODO

data Item
    = Spare RegisterSize
    | Item ItemName ItemTitle (Maybe Description) Variation (Maybe Remark)
    deriving (Generic, Eq, Show)

instance ToJSON Item where
    toJSON (Spare n) = object
        [ "spare"       .= True
        , "length"      .= n
        ]
    toJSON (Item name tit dsc var remark) = object
        [ "spare"       .= False
        , "name"        .= name
        , "title"       .= tit
        , "description" .= dsc
        , "variation"   .= var
        , "remark"      .= remark
        ]

data Encoding = Mandatory | Optional | Absent
    deriving (Generic, Eq, Show)

instance ToJSON Encoding where
    toJSON encoding = toJSON $ fmap Data.Char.toLower $ show encoding

data Toplevel = Toplevel
    { topEncoding   :: Rule Encoding
    , topDefinition :: Text
    , topItem       :: Item
    } deriving (Generic, Eq, Show)

instance ToJSON Toplevel where
    toJSON t = case topItem t of
        Spare _ -> error "spare toplevel item"
        i -> object
            [ "encoding"    .= topEncoding t
            , "definition"  .= topDefinition t
            , "item"        .= i
            ]

data Uap
    = Uap [Maybe ItemName]
    | Uaps [(UapName, [Maybe ItemName])]
    deriving (Generic, Eq, Show)

instance ToJSON Uap where
    toJSON (Uap lst) = object
        [ "type" .= ("uap" :: String)
        , "items" .= lst
        ]
    toJSON (Uaps variations) = object
        [ "type" .= ("uaps" :: String)
        , "variations" .= fmap variation variations
        ]
      where
        variation (uapName, lst) = object
            [ "name" .= uapName
            , "items" .= lst
            ]

data Category = Category
    { catCat        :: Word8
    , catTitle      :: Text
    , catEdition    :: Edition
    , catDate       :: Date
    , catPreamble   :: Maybe Text
    , catItems      :: [Toplevel]
    , catUap        :: Uap
    } deriving (Generic, Eq, Show)

instance ToJSON Category where
    toJSON c = object
        [ "category"    .= (TP.printf "%03d" (catCat c) :: String)
        , "title"       .= catTitle c
        , "edition"     .= catEdition c
        , "date"        .= catDate c
        , "preamble"    .= catPreamble c
        , "items"       .= catItems c
        , "uap"         .= catUap c
        ]

