-- '.json' syntax implementation

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Asterix.Specs.Syntax.Json where

import           Data.Text.Encoding as T
import           Data.Aeson hiding (Encoding)
import qualified Data.Aeson.Encode.Pretty as JsonP

import           Asterix.Specs.Syntax
import           Asterix.Specs.Types

instance ToJSON BitSize
instance FromJSON BitSize

instance ToJSON ByteSize
instance FromJSON ByteSize

instance ToJSON Signedness
instance FromJSON Signedness

instance ToJSON RepetitiveType
instance FromJSON RepetitiveType

instance ToJSON ExplicitType
instance FromJSON ExplicitType

instance ToJSON StringType
instance FromJSON StringType

instance ToJSON Constrain
instance FromJSON Constrain

instance ToJSON Number
instance FromJSON Number

instance ToJSON Unit
instance FromJSON Unit

instance ToJSON BdsAddr
instance FromJSON BdsAddr

instance ToJSON BdsType
instance FromJSON BdsType

instance ToJSON Content
instance FromJSON Content

instance ToJSON ItemName
instance FromJSON ItemName

instance ToJSON ItemPath
instance FromJSON ItemPath

instance ToJSON a => ToJSON (Rule a)
instance FromJSON a => FromJSON (Rule a)

instance ToJSON a => ToJSON (Variation a)
instance FromJSON a => FromJSON (Variation a)

instance ToJSON a => ToJSON (Item a)
instance FromJSON a => FromJSON (Item a)

instance ToJSON Documentation
instance FromJSON Documentation

instance ToJSON Title
instance FromJSON Title

instance ToJSON Date
instance FromJSON Date

instance ToJSON Edition
instance FromJSON Edition

instance ToJSON CatNum
instance FromJSON CatNum

instance ToJSON a => ToJSON (UapItem a)
instance FromJSON a => FromJSON (UapItem a)

instance ToJSON UapSelector
instance FromJSON UapSelector

instance ToJSON UapName
instance FromJSON UapName

instance ToJSON Uap
instance FromJSON Uap

instance ToJSON Expansion
instance FromJSON Expansion

instance ToJSON Basic
instance FromJSON Basic

instance ToJSON Asterix
instance FromJSON Asterix

coder :: Coder
coder = Coder
    { cDescription = "JSON format"
    , cDecoder = Just decoder
    , cEncoder = Just encoder
    }
  where
    decoder filename s = case eitherDecodeStrict' (T.encodeUtf8 s) of
        Left e -> Left $ filename ++ ": " ++ e
        Right val -> Right val

    encoder = JsonP.encodePrettyToTextBuilder' JsonP.defConfig
        {JsonP.confCompare = compare}
