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
    | ItemDependent [ItemName] [(Int, a)]
    deriving (Generic, Eq, Show)

instance ToJSON a => ToJSON (Rule a)
  where
    toJSON (ContextFree rule) = object
        [ "type" .= ("ContextFree" :: String)
        , "rule" .= rule
        ]
    toJSON (ItemDependent item rules) = object
        [ "type" .= ("ItemDependent" :: String)
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
    { dateYear  :: Integer
    , dateMonth :: Int
    , dateDay   :: Int
    } deriving (Generic, Eq, Show)

instance ToJSON Date where
    toJSON (Date y m d) = toJSON $
        show y ++ "-" ++ TP.printf "%02d" m ++ "-" ++ TP.printf "%02d" d

data Number
    = NumberZ Integer
    | NumberQ (Ratio Int)
    | NumberR Double
    deriving (Generic, Eq, Ord, Show)

instance ToJSON Number where
    toJSON = \case
        NumberZ val -> object
            [ "type" .= ("Integer" :: String)
            , "value" .= val
            ]
        NumberQ val -> object
            [ "type" .= ("Ratio" :: String)
            , "value" .= val
            ]
        NumberR val -> object
            [ "type" .= ("Real" :: String)
            , "value" .= val
            ]

data Constrain
    = EqualTo Number
    | NotEqualTo Number
    | GreaterThan Number
    | GreaterThanOrEqualTo Number
    | LessThan Number
    | LessThanOrEqualTo Number
    deriving (Generic, Eq, Ord, Show)

instance ToJSON Constrain where
    toJSON (EqualTo val) = object ["type" .= ("=="::String), "value" .= val]
    toJSON (NotEqualTo val) = object ["type" .= ("/="::String), "value" .= val]
    toJSON (GreaterThan val) = object ["type" .= (">"::String), "value" .= val]
    toJSON (GreaterThanOrEqualTo val) = object ["type" .= (">="::String), "value" .= val]
    toJSON (LessThan val) = object ["type" .= ("<"::String), "value" .= val]
    toJSON (LessThanOrEqualTo val) = object ["type" .= ("<="::String), "value" .= val]

newtype Signed = Signed Bool deriving (Generic, Eq, Show)

instance ToJSON Signed where
    toJSON (Signed val) = toJSON val

data StringType
    = StringAscii
    | StringICAO
    deriving (Generic, Eq, Show)

instance ToJSON StringType where
    toJSON = toJSON . show

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
        Int         -- number for fractional bits
        Text        -- unit
        [Constrain]
    deriving (Generic, Eq, Show)

instance ToJSON Content where
    toJSON = \case
        ContentRaw -> object
            [ "type" .= ("Raw" :: String)
            ]
        ContentTable lst -> object
            [ "type" .= ("Table" :: String)
            , "values" .= lst
            ]
        ContentString st -> object
            [ "type" .= ("String" :: String)
            , "variation" .= st
            ]
        ContentInteger signed lst -> object
            [ "type" .= ("Integer" :: String)
            , "signed" .= signed
            , "constraints" .= lst
            ]
        ContentQuantity signed scaling fractional unit constraints -> object
            [ "type" .= ("Quantity" :: String)
            , "signed" .= signed
            , "scaling" .= scaling
            , "fractionalBits" .= fractional
            , "unit"    .= unit
            , "constraints" .= constraints
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
    toJSON (Fixed n content) = object
        [ "type"    .= ("Fixed" :: String)
        , "size"    .= n
        , "content" .= content
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

