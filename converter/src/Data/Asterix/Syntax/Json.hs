{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Asterix.Syntax.Json
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines '.json' syntax.
--

module Data.Asterix.Syntax.Json (syntax) where

import           Data.Char (toLower)
import           Data.Aeson (ToJSON, toJSON, object, (.=))
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.ByteString.Lazy (toStrict)

import           Data.Asterix

instance ToJSON a => ToJSON (Rule a)
  where
    toJSON (Unspecified) = object
        [ "type" .= ("Unspecified" :: String)
        ]
    toJSON (ContextFree rule) = object
        [ "type" .= ("ContextFree" :: String)
        , "rule" .= rule
        ]
    toJSON (Dependent name rules) = object
        [ "type" .= ("Dependent" :: String)
        , "name" .= name
        , "rules" .= rules
        ]

instance ToJSON Edition where
    toJSON (Edition a b) = object
        [ "major" .= a
        , "minor" .= b
        ]

instance ToJSON Date where
    toJSON (Date y m d) = object
        [ "year"    .= y
        , "month"   .= m
        , "day"     .= d
        ]

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

instance ToJSON Constrain where
    toJSON (EqualTo val) = object ["type" .= ("=="::String), "value" .= val]
    toJSON (NotEqualTo val) = object ["type" .= ("/="::String), "value" .= val]
    toJSON (GreaterThan val) = object ["type" .= (">"::String), "value" .= val]
    toJSON (GreaterThanOrEqualTo val) = object ["type" .= (">="::String), "value" .= val]
    toJSON (LessThan val) = object ["type" .= ("<"::String), "value" .= val]
    toJSON (LessThanOrEqualTo val) = object ["type" .= ("<="::String), "value" .= val]

instance ToJSON Signed where
    toJSON (Signed val) = toJSON val

instance ToJSON StringType where
    toJSON = toJSON . show

instance ToJSON Content where
    toJSON = \case
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

instance ToJSON Element where
    toJSON (Fixed n content) = object
        [ "type"    .= ("Fixed" :: String)
        , "size"    .= n
        , "content" .= content
        ]
    toJSON (Group lst) = object
        [ "type"    .= ("Group" :: String)
        , "subitems" .= lst
        ]
    toJSON (Extended n1 n2 lst) = object
        [ "type"    .= ("Extended" :: String)
        , "first"   .= n1
        , "extents" .= n2
        , "subitems" .= lst
        ]
    toJSON (Repetitive n el) = object
        [ "type"    .= ("Repetitive" :: String)
        , "rep"     .= n
        , "element" .= el
        ]
    toJSON Explicit = object
        [ "type"    .= ("Explicit" :: String)
        ]
    toJSON (Compound lst) = object
        [ "type"    .= ("Compound" :: String)
        , "subitems" .= lst
        ]
    toJSON _ = undefined -- TODO

instance ToJSON Subitem where
    toJSON (Spare n) = object
        [ "spare"       .= True
        , "length"      .= n
        ]
    toJSON (Subitem name tit dsc el remark) = object
        [ "spare"       .= False
        , "name"        .= name
        , "title"       .= tit
        , "description" .= dsc
        , "element"     .= el
        , "remark"      .= remark
        ]

instance ToJSON Encoding where
    toJSON encoding = toJSON $ fmap toLower $ show encoding

instance ToJSON Item where
    toJSON t = case itemSubitem t of
        Spare _ -> error "spare toplevel item"
        si -> object
            [ "encoding"    .= itemEncoding t
            , "definition"  .= itemDefinition t
            , "subitem"     .= si
            ]

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

instance ToJSON Asterix where
    toJSON c = object
        [ "number"      .= astCategory c
        , "title"       .= astTitle c
        , "edition"     .= astEdition c
        , "date"        .= astDate c
        , "preamble"    .= astPreamble c
        , "catalogue"   .= astCatalogue c
        , "uap"         .= astUap c
        ]

-- | Syntax implementation
syntax :: Syntax
syntax = Syntax
    { syntaxDescription = "JSON asterix syntax."
    , encodeAsterix = Just encoder
    , decodeAsterix = Nothing       -- decoding not supported
    }
  where
    encoder = toStrict
        . JsonP.encodePretty' JsonP.defConfig {JsonP.confCompare = compare}

