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

import           Data.Aeson hiding (Encoding)
import           Data.Bool
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HMS

import           Data.Asterix.Types
import           Data.Asterix.Common

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

instance FromJSON a => FromJSON (Rule a) where
    parseJSON = withObject "Rule" $ \v -> case HMS.lookup "type" v of
        Just "Unspecified" -> pure Unspecified
        Just "ContextFree" -> ContextFree
            <$> v .: "rule"
        Just "Dependent" -> Dependent
            <$> v .: "name"
            <*> v .: "rules"
        _ -> typeMismatch "Rule" $ String "wrong type"

instance ToJSON Edition where
    toJSON (Edition a b) = object
        [ "major" .= a
        , "minor" .= b
        ]

instance FromJSON Edition where
    parseJSON = withObject "Edition" $ \v -> Edition
        <$> v .: "major"
        <*> v .: "minor"

instance ToJSON Date where
    toJSON (Date y m d) = object
        [ "year"    .= y
        , "month"   .= m
        , "day"     .= d
        ]

instance FromJSON Date where
    parseJSON = withObject "Date" $ \v -> Date
        <$> v .: "year"
        <*> v .: "month"
        <*> v .: "day"

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

instance FromJSON Number where
    parseJSON = withObject "Number" $ \v -> case HMS.lookup "type" v of
        Just "Integer" -> NumberZ <$> v .: "value"
        Just "Ratio" -> NumberQ <$> v .: "value"
        Just "Real" -> NumberR <$> v .: "value"
        _ -> typeMismatch "Number" $ String "wrong type"

instance ToJSON Constrain where
    toJSON (EqualTo val) = object ["type" .= ("=="::String), "value" .= val]
    toJSON (NotEqualTo val) = object ["type" .= ("/="::String), "value" .= val]
    toJSON (GreaterThan val) = object ["type" .= (">"::String), "value" .= val]
    toJSON (GreaterThanOrEqualTo val) = object ["type" .= (">="::String), "value" .= val]
    toJSON (LessThan val) = object ["type" .= ("<"::String), "value" .= val]
    toJSON (LessThanOrEqualTo val) = object ["type" .= ("<="::String), "value" .= val]

instance FromJSON Constrain where
    parseJSON = withObject "Constrain" $ \v -> case HMS.lookup "type" v of
        Just "==" -> EqualTo <$> v .: "value"
        Just "/=" -> NotEqualTo <$> v .: "value"
        Just ">" -> GreaterThan <$> v .: "value"
        Just ">=" -> GreaterThanOrEqualTo <$> v .: "value"
        Just "<" -> LessThan <$> v .: "value"
        Just "<=" -> LessThanOrEqualTo <$> v .: "value"
        _ -> typeMismatch "Constrain" $ String "wrong type"

instance ToJSON Signed where
    toJSON = \case
        Unsigned -> toJSON False
        Signed -> toJSON True

instance FromJSON Signed  where
    parseJSON s = bool Unsigned Signed <$> parseJSON s

instance ToJSON StringType where
    toJSON = toJSON . show

instance FromJSON StringType  where
    parseJSON s = read <$> parseJSON s

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
        ContentBds -> object
            [ "type" .= ("Bds" :: String)
            ]

instance FromJSON Content  where
    parseJSON = withObject "Content" $ \v -> case HMS.lookup "type" v of
        Just "Table" -> ContentTable
            <$> v .: "values"
        Just "String" -> ContentString
            <$> v .: "variation"
        Just "Integer" -> ContentInteger
            <$> v .: "signed"
            <*> v .: "constraints"
        Just "Quantity" -> ContentQuantity
            <$> v .: "signed"
            <*> v .: "scaling"
            <*> v .: "fractionalBits"
            <*> v .: "unit"
            <*> v .: "constraints"
        Just "Bds" -> pure ContentBds
        _ -> typeMismatch "Content" $ String "wrong type"

instance ToJSON Variation where
    toJSON (Element n content) = object
        [ "type"    .= ("Element" :: String)
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
    toJSON (Repetitive n el) = object
        [ "type"    .= ("Repetitive" :: String)
        , "rep"     .= n
        , "variation" .= el
        ]
    toJSON Explicit = object
        [ "type"    .= ("Explicit" :: String)
        ]
    toJSON (Compound mSize lst) = object
        [ "type"    .= ("Compound" :: String)
        , "fspec"   .= mSize
        , "items"   .= lst
        ]

instance FromJSON Variation  where
    parseJSON = withObject "Variation" $ \v -> case HMS.lookup "type" v of
        Just "Element" -> Element
            <$> v .: "size"
            <*> v .: "content"
        Just "Group" -> Group
            <$> v .: "items"
        Just "Extended" -> Extended
            <$> v .: "first"
            <*> v .: "extents"
            <*> v .: "items"
        Just "Repetitive" -> Repetitive
            <$> v .: "rep"
            <*> v .: "variation"
        Just "Explicit" -> pure Explicit
        Just "Compound" -> Compound
            <$> v .: "fspec"
            <*> v .: "items"
        _ -> typeMismatch "Element" $ String "wrong type"

instance ToJSON Item where
    toJSON (Spare n) = object
        [ "spare"       .= True
        , "length"      .= n
        ]
    toJSON (Item name title variation doc) = object
        [ "spare"       .= False
        , "name"        .= name
        , "title"       .= title
        , "definition"  .= docDefinition doc
        , "description" .= docDescription doc
        , "variation"   .= variation
        , "remark"      .= docRemark doc
        ]

instance FromJSON Item  where
    parseJSON = withObject "Item" $ \v -> case HMS.lookup "spare" v of
        Just (Bool True) -> Spare
            <$> v .: "length"
        Just (Bool False) -> Item
            <$> v .: "name"
            <*> v .: "title"
            <*> v .: "variation"
            <*> (Documentation
                <$> v .: "definition"
                <*> v .: "description"
                <*> v .: "remark"
                )
        _ -> typeMismatch "Item" $ String "spare field not present"

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

instance FromJSON Uap  where
    parseJSON = withObject "Uap" $ \v -> case HMS.lookup "type" v of
        Just "uap" -> Uap <$> v .: "items"
        Just "uaps" -> Uap <$> v .: "variations"
        _ -> typeMismatch "Uap" $ String "wrong type"

instance ToJSON Basic where
    toJSON c = object
        [ "type"        .= ("Basic" :: String)
        , "number"      .= basCategory c
        , "title"       .= basTitle c
        , "edition"     .= basEdition c
        , "date"        .= basDate c
        , "preamble"    .= basPreamble c
        , "catalogue"   .= basCatalogue c
        , "uap"         .= basUap c
        ]

instance FromJSON Basic  where
    parseJSON = withObject "Basic" $ \v -> Basic
        <$> v .: "number"
        <*> v .: "title"
        <*> v .: "edition"
        <*> v .: "date"
        <*> v .: "preamble"
        <*> v .: "catalogue"
        <*> v .: "uap"

instance ToJSON Expansion where
    toJSON c = object
        [ "type"        .= ("Expansion" :: String)
        , "number"      .= expCategory c
        , "title"       .= expTitle c
        , "edition"     .= expEdition c
        , "date"        .= expDate c
        , "variation"   .= expVariation c
        ]

instance FromJSON Expansion where
    parseJSON = withObject "Expansion" $ \v -> Expansion
        <$> v .: "number"
        <*> v .: "title"
        <*> v .: "edition"
        <*> v .: "date"
        <*> v .: "variation"

instance ToJSON Asterix where
    toJSON = \case
        AsterixBasic x -> toJSON x
        AsterixExpansion x -> toJSON x

instance FromJSON Asterix  where
    parseJSON = withObject "Asterix" $ \v -> case HMS.lookup "type" v of
        Just "Basic" -> AsterixBasic <$> parseJSON (Object v)
        Just "Expansion" -> AsterixExpansion <$> parseJSON (Object v)
        _ -> typeMismatch "Asterix" $ String "wrong type"

-- | Syntax implementation
syntax :: Syntax
syntax = Syntax
    { syntaxDescription = "JSON asterix syntax."
    , syntaxEncoder = Just encoder
    , syntaxDecoder = Just decoder
    }
  where
    encoder = toStrict
        . JsonP.encodePretty' JsonP.defConfig {JsonP.confCompare = compare}
    decoder filename s = case eitherDecodeStrict' s of
        Left e -> Left $ filename ++ ": " ++ e
        Right val -> Right val

