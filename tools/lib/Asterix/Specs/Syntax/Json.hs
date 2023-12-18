{-# OPTIONS_GHC -fno-warn-orphans #-}

-- '.json' syntax implementation

module Asterix.Specs.Syntax.Json (syntax) where

import           Control.Monad
import           Data.Aeson hiding (Encoding)
import           Data.Bool
import           Data.Aeson.Types (typeMismatch, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.ByteString.Lazy (toStrict)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import qualified Data.Text as T

import           Asterix.Specs.Types
import           Asterix.Specs.Common

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
        NumInt val -> object
            [ "type" .= ("Integer" :: String)
            , "value" .= val
            ]
        NumDiv a b -> object
            [ "type" .= ("Div" :: String)
            , "numerator" .= a
            , "denominator" .= b
            ]
        NumPow a b -> object
            [ "type" .= ("Pow" :: String)
            , "base" .= a
            , "exponent" .= b
            ]

instance FromJSON Number where
    parseJSON = withObject "Number" $ \v -> case KM.lookup "type" v of
        Just "Integer" -> NumInt <$> v .: "value"
        Just "Div" -> NumDiv
            <$> v .: "numerator"
            <*> v .: "denominator"
        Just "Pow" -> NumPow
            <$> v .: "base"
            <*> v .: "exponent"
        _ -> typeMismatch "Number" $ String "wrong type"

instance ToJSON Constrain where
    toJSON (EqualTo val) = object ["type" .= ("=="::String), "value" .= val]
    toJSON (NotEqualTo val) = object ["type" .= ("/="::String), "value" .= val]
    toJSON (GreaterThan val) = object ["type" .= (">"::String), "value" .= val]
    toJSON (GreaterThanOrEqualTo val) = object ["type" .= (">="::String), "value" .= val]
    toJSON (LessThan val) = object ["type" .= ("<"::String), "value" .= val]
    toJSON (LessThanOrEqualTo val) = object ["type" .= ("<="::String), "value" .= val]

instance FromJSON Constrain where
    parseJSON = withObject "Constrain" $ \v -> case KM.lookup "type" v of
        Just "==" -> EqualTo <$> v .: "value"
        Just "/=" -> NotEqualTo <$> v .: "value"
        Just ">" -> GreaterThan <$> v .: "value"
        Just ">=" -> GreaterThanOrEqualTo <$> v .: "value"
        Just "<" -> LessThan <$> v .: "value"
        Just "<=" -> LessThanOrEqualTo <$> v .: "value"
        _ -> typeMismatch "Constrain" $ String "wrong type"

instance ToJSON Signedness where
    toJSON = \case
        Unsigned -> toJSON False
        Signed -> toJSON True

instance FromJSON Signedness  where
    parseJSON s = bool Unsigned Signed <$> parseJSON s

instance ToJSON StringType where
    toJSON = toJSON . show

instance FromJSON StringType  where
    parseJSON s = read <$> parseJSON s

instance ToJSON BdsAddr where
    toJSON (BdsAddr val) = toJSON (printf "%2.2x" val :: String)

instance FromJSON BdsAddr where
    parseJSON = withText "BdsAddr" $ \val -> case readMaybe ("0x" ++ T.unpack val) of
        Nothing -> fail $ "can not parse BdsAddr: " ++ show val
        Just x -> return $ BdsAddr x

instance ToJSON BdsType where
    toJSON = \case
        BdsWithAddress -> object
            [ "type" .= ("BdsWithAddress" :: String)
            ]
        BdsAt mAddr -> object
            [ "type" .= ("BdsAt" :: String)
            , "address" .= mAddr
            ]

instance FromJSON BdsType where
    parseJSON = withObject "BdsType" $ \v -> case KM.lookup "type" v of
        Just "BdsWithAddress" -> pure BdsWithAddress
        Just "BdsAt" -> BdsAt
            <$> v .: "address"
        _ -> typeMismatch "BdsType" $ String "wrong type"

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
        ContentInteger signedness lst -> object
            [ "type" .= ("Integer" :: String)
            , "signed" .= signedness
            , "constraints" .= lst
            ]
        ContentQuantity signedness lsb unit constraints -> object
            [ "type" .= ("Quantity" :: String)
            , "signed" .= signedness
            , "lsb" .= lsb
            , "unit"    .= unit
            , "constraints" .= constraints
            ]
        ContentBds bt -> object $
            [ "type" .= ("Bds" :: String)
            , "variation" .= bt
            ]

instance FromJSON Content  where
    parseJSON = withObject "Content" $ \v -> case KM.lookup "type" v of
        Just "Raw" -> pure ContentRaw
        Just "Table" -> ContentTable
            <$> v .: "values"
        Just "String" -> ContentString
            <$> v .: "variation"
        Just "Integer" -> ContentInteger
            <$> v .: "signed"
            <*> v .: "constraints"
        Just "Quantity" -> ContentQuantity
            <$> v .: "signed"
            <*> v .: "lsb"
            <*> v .: "unit"
            <*> v .: "constraints"
        Just "Bds" -> ContentBds
            <$> v .: "variation"
        _ -> typeMismatch "Content" $ String "wrong type"

instance ToJSON Rule
  where
    toJSON (ContextFree content) = object
        [ "type"    .= ("ContextFree" :: String)
        , "content" .= content
        ]
    toJSON (Dependent name rules) = object
        [ "type" .= ("Dependent" :: String)
        , "name" .= name
        , "rules" .= rules
        ]

instance FromJSON Rule where
    parseJSON = withObject "Rule" $ \v -> case KM.lookup "type" v of
        Just "ContextFree" -> ContextFree
            <$> v .: "content"
        Just "Dependent" -> Dependent
            <$> v .: "name"
            <*> v .: "rules"
        _ -> typeMismatch "Rule" $ String "wrong type"

instance ToJSON RepetitiveType where
    toJSON = \case
        RepetitiveRegular n -> object
            [ "type" .= ("Regular" :: String)
            , "size" .= n
            ]
        RepetitiveFx -> object
            [ "type" .= ("Fx" :: String)
            ]

instance FromJSON RepetitiveType where
    parseJSON = withObject "RepetitiveType" $ \v -> case KM.lookup "type" v of
        Just "Regular" -> RepetitiveRegular
            <$> v .: "size"
        Just "Fx" -> pure RepetitiveFx
        _ -> typeMismatch "RepetitiveType" $ String "wrong type"

instance ToJSON ExplicitType where
    toJSON = \case
        ReservedExpansion -> "RE"
        SpecialPurpose -> "SP"

instance FromJSON ExplicitType where
    parseJSON = withText "ExplicitType" $ \case
        "RE" -> pure ReservedExpansion
        "SP" -> pure SpecialPurpose
        val -> fail $ "unexpected value: " ++ show val

instance ToJSON Variation where
    toJSON (Element n rule) = object
        [ "type"    .= ("Element" :: String)
        , "size"    .= n
        , "rule"    .= rule
        ]
    toJSON (Group lst) = object
        [ "type"    .= ("Group" :: String)
        , "items"   .= lst
        ]
    toJSON (Extended lst) = object
        [ "type"    .= ("Extended" :: String)
        , "items"   .= lst
        ]
    toJSON (Repetitive rt el) = object
        [ "type"    .= ("Repetitive" :: String)
        , "rep"     .= rt
        , "variation" .= el
        ]
    toJSON (Explicit mt) = object
        [ "type"    .= ("Explicit" :: String)
        , "expl"    .= mt
        ]
    toJSON RandomFieldSequencing = object
        [ "type"    .= ("Rfs" :: String)
        ]
    toJSON (Compound mSize lst) = object
        [ "type"    .= ("Compound" :: String)
        , "fspec"   .= mSize
        , "items"   .= lst
        ]

instance FromJSON Variation where
    parseJSON = withObject "Variation" $ \v -> case KM.lookup "type" v of
        Just "Element" -> Element
            <$> v .: "size"
            <*> v .: "rule"
        Just "Group" -> Group
            <$> v .: "items"
        Just "Extended" -> Extended
            <$> v .: "items"
        Just "Repetitive" -> Repetitive
            <$> v .: "rep"
            <*> v .: "variation"
        Just "Explicit" -> Explicit
            <$> v .: "expl"
        Just "Rfs" -> pure RandomFieldSequencing
        Just "Compound" -> Compound
            <$> v .: "fspec"
            <*> v .: "items"
        _ -> typeMismatch "Variation" $ String "wrong type"

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
    parseJSON = withObject "Item" $ \v -> case KM.lookup "spare" v of
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

instance ToJSON UapSelector where
    toJSON (UapSelector item lst) = object
        [ "name" .= item
        , "rules" .= lst
        ]

instance FromJSON UapSelector where
    parseJSON = withObject "UapSelector" $ \v -> UapSelector
        <$> v .: "name"
        <*> v .: "rules"

instance ToJSON Uap where
    toJSON (Uap lst) = object
        [ "type" .= ("uap" :: String)
        , "items" .= lst
        ]
    toJSON (Uaps variations msel) = object
        [ "type" .= ("uaps" :: String)
        , "variations" .= fmap variation variations
        , "selector" .= msel
        ]
      where
        variation (uapName, lst) = object
            [ "name" .= uapName
            , "items" .= lst
            ]

instance FromJSON Uap  where
    parseJSON = withObject "Uap" $ \v -> case KM.lookup "type" v of
        Just "uap" -> Uap <$> v .: "items"
        Just "uaps" -> Uaps <$> var (v .: "variations") <*> (v .: "selector")
          where
            var :: Parser [Value] -> Parser [(UapName, [Maybe Name])]
            var p = do
                lst <- p
                forM lst $ withObject "(,)" $ \v' -> (,)
                    <$> v' .: "name"
                    <*> v' .: "items"
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
    parseJSON = withObject "Asterix" $ \v -> case KM.lookup "type" v of
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
