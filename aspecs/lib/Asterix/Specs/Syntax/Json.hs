-- | '.json' syntax implementation

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Asterix.Specs.Syntax.Json where

import           Data.Aeson               hiding (Encoding)
import           Data.Aeson.Encode.Pretty as JsonP
import qualified Data.Aeson.KeyMap        as KM
import           Data.Aeson.Types         (Parser, emptyArray, typeMismatch)
import           Data.Text                as T
import           Data.Text.Encoding       as T

import           Asterix.Specs.Syntax
import           Asterix.Specs.Types

-- | Aeson lib does this by default, but be explicit.
tagged :: ToJSON val => String -> val -> Value
tagged tag val = object
    [ "tag" .= tag
    , "contents" .= toJSON val
    ]

-- | Helper function for parsing.
untagged :: String -> [(Text, Value -> Parser a)] -> Value -> Parser a
untagged s lst = withObject s $ \obj -> do
    tag <- case KM.lookup "tag" obj of
        Just (String tag) -> pure tag
        _                 -> typeMismatch "tag" $ String "wrong type"
    val <- case KM.lookup "contents" obj of
        Nothing  -> fail "'contents' element not present"
        Just val -> pure val
    f <- case lookup tag lst of
        Nothing -> fail ("Unexpected tag: " <> T.unpack tag)
        Just f  -> pure f
    f val

parseUnit :: Value -> Parser ()
parseUnit v = case v == emptyArray of
    True  -> pure ()
    False -> fail "Expecting empty array"

instance ToJSON CatNum
instance FromJSON CatNum

instance ToJSON ByteSize
instance FromJSON ByteSize

instance ToJSON BitSize
instance FromJSON BitSize

instance ToJSON ItemName
instance FromJSON ItemName

instance ToJSON Unit
instance FromJSON Unit

instance ToJSON UapName
instance FromJSON UapName

instance ToJSON ItemPath
instance FromJSON ItemPath

instance ToJSON Title
instance FromJSON Title

instance ToJSON BdsAddr
instance FromJSON BdsAddr

instance ToJSON Edition where
    toJSON (Edition a b) = object
        [ "major" .= a
        , "minor" .= b
        ]

instance FromJSON Edition where
    parseJSON = withObject "Edition" $ \o -> Edition
        <$> o .: "major"
        <*> o .: "minor"

instance ToJSON Date where
    toJSON (Date y m d) = object
        [ "year"    .= y
        , "month"   .= m
        , "day"     .= d
        ]

instance FromJSON Date where
    parseJSON = withObject "Date" $ \o -> Date
        <$> o .: "year"
        <*> o .: "month"
        <*> o .: "day"

instance ToJSON StringType where
    toJSON = \case
        StringAscii -> tagged "StringAscii" ()
        StringICAO -> tagged "StringICAO" ()
        StringOctal -> tagged "StringOctal" ()

instance FromJSON StringType where
    parseJSON = untagged "StringType"
        [ ("StringAscii", \o -> parseUnit o >> pure StringAscii)
        , ("StringICAO", \o -> parseUnit o >> pure StringICAO)
        , ("StringOctal", \o -> parseUnit o >> pure StringOctal)
        ]

instance ToJSON Signedness where
    toJSON = \case
        Signed -> tagged "Signed" ()
        Unsigned -> tagged "Unsigned" ()

instance FromJSON Signedness where
    parseJSON = untagged "Signedness"
        [ ("Signed", \o -> parseUnit o >> pure Signed)
        , ("Unsigned", \o -> parseUnit o >> pure Unsigned)
        ]

instance ToJSON Number where
    toJSON = \case
        NumInt x -> tagged "NumInt" x
        NumDiv a b -> tagged "NumDiv" $ object
            [ "numerator" .= a
            , "denominator" .= b
            ]
        NumPow a b -> tagged "NumPow" $ object
            [ "base" .= a
            , "exponent" .= b
            ]

instance FromJSON Number where
    parseJSON = untagged "Number"
        [ ("NumInt", fmap NumInt . parseJSON)
        , ("NumDiv", withObject "NumDiv" $ \o -> NumDiv
              <$> o .: "numerator"
              <*> o .: "denominator"
          )
        , ("NumPow", withObject "NumPow" $ \o -> NumPow
              <$> o .: "base"
              <*> o .: "exponent"
          )
        ]

instance ToJSON Constrain where
    toJSON = \case
        EqualTo n -> tagged "EqualTo" n
        NotEqualTo n -> tagged "NotEqualTo" n
        GreaterThan n -> tagged "GreaterThan" n
        GreaterThanOrEqualTo n -> tagged "GreaterThanOrEqualTo" n
        LessThan n -> tagged "LessThan" n
        LessThanOrEqualTo n -> tagged "LessThanOrEqualTo" n

instance FromJSON Constrain where
    parseJSON = untagged "Constrain"
        [ ("EqualTo", fmap EqualTo . parseJSON)
        , ("NotEqualTo", fmap NotEqualTo . parseJSON)
        , ("GreaterThan", fmap GreaterThan . parseJSON)
        , ("GreaterThanOrEqualTo", fmap GreaterThanOrEqualTo . parseJSON)
        , ("LessThan", fmap LessThan . parseJSON)
        , ("LessThanOrEqualTo", fmap LessThanOrEqualTo . parseJSON)
        ]

instance ToJSON BdsType where
    toJSON = \case
        BdsWithAddress -> tagged "BdsWithAddress" ()
        BdsAt ma -> tagged "BdsAt" ma

instance FromJSON BdsType where
    parseJSON = untagged "BdsType"
        [ ("BdsWithAddress", \o -> parseUnit o >> pure BdsWithAddress)
        , ("BdsAt", fmap BdsAt . parseJSON)
        ]

instance ToJSON Content where
    toJSON = \case
        ContentRaw -> tagged "ContentRaw" ()
        ContentTable lst -> tagged "ContentTable" lst
        ContentString t -> tagged "ContentString" t
        ContentInteger sig cstr -> tagged "ContentInteger" $ object
            [ "signedness" .= sig
            , "constraints" .= cstr
            ]
        ContentQuantity sig lsb unit cstr -> tagged "ContentQuantity" $ object
            [ "signedness" .= sig
            , "lsb" .= lsb
            , "unit" .= unit
            , "constraints" .= cstr
            ]
        ContentBds t -> tagged "ContentBds" t

instance FromJSON Content where
    parseJSON = untagged "Content"
        [ ("ContentRaw", \o -> parseUnit o >> pure ContentRaw)
        , ("ContentTable", fmap ContentTable . parseJSON)
        , ("ContentString", fmap ContentString . parseJSON)
        , ("ContentInteger", withObject "ContentInteger" $ \o -> ContentInteger
            <$> o .: "signedness"
            <*> o .: "constraints"
          )
        , ("ContentQuantity", withObject "ContentQuantity" $ \o -> ContentQuantity
            <$> o .: "signedness"
            <*> o .: "lsb"
            <*> o .: "unit"
            <*> o .: "constraints"
          )
        , ("ContentBds", fmap ContentBds . parseJSON)
        ]

instance ToJSON RepetitiveType where
    toJSON = \case
        RepetitiveRegular byteSize -> tagged "RepetitiveRegular" $ object
            [ "byteSize" .= byteSize
            ]
        RepetitiveFx -> tagged "RepetitiveFx" ()

instance FromJSON RepetitiveType where
    parseJSON = untagged "RepetitiveType"
        [ ("RepetitiveRegular", withObject "RepetitiveRegular" $ \o ->
            RepetitiveRegular <$> o .: "byteSize")
        , ("RepetitiveFx", \o -> parseUnit o >> pure RepetitiveFx)
        ]

instance ToJSON ExplicitType where
    toJSON = \case
        ReservedExpansion -> tagged "ReservedExpansion" ()
        SpecialPurpose -> tagged "SpecialPurpose" ()

instance FromJSON ExplicitType where
    parseJSON = untagged "ExplicitType"
        [ ("ReservedExpansion", \o -> parseUnit o >> pure ReservedExpansion)
        , ("SpecialPurpose", \o -> parseUnit o >> pure SpecialPurpose)
        ]

instance ToJSON (Variation ()) where
    toJSON = \case
        Element () bitSize rule -> tagged "Element" $ object
            [ "bitSize" .= bitSize
            , "rule" .= rule
            ]
        Group () lst -> tagged "Group" lst
        Extended lst -> tagged "Extended" lst
        Repetitive t var -> tagged "Repetitive" $ object
            [ "type" .= t
            , "variation" .= var
            ]
        Explicit mt -> tagged "Explicit" mt
        Compound lst -> tagged "Compound" lst

instance FromJSON (Variation ()) where
    parseJSON = untagged "Variation"
        [ ("Element", withObject "Element" $ \o -> Element ()
          <$> o .: "bitSize"
          <*> o .: "rule"
          )
        , ("Group", fmap (Group ()) . parseJSON)
        , ("Extended", fmap Extended . parseJSON)
        , ("Repetitive", withObject "Repetitive" $ \o -> Repetitive
          <$> o .: "type"
          <*> o .: "variation"
          )
        , ("Explicit", fmap Explicit . parseJSON)
        , ("Compound", fmap Compound . parseJSON)
        ]

instance ToJSON (Item ()) where
    toJSON = \case
        Spare () bitSize -> tagged "Spare" bitSize
        Item nsp -> tagged "Item" nsp

instance FromJSON (Item ()) where
    parseJSON = untagged "Item"
        [ ("Spare", fmap (Spare ()) . parseJSON)
        , ("Item", fmap Item . parseJSON)
        ]

instance ToJSON r => ToJSON (Rule r) where
    toJSON = \case
        ContextFree val -> tagged "ContextFree" val
        Dependent path dv cases -> tagged "Dependent" $ object
            [ "path" .= path
            , "default" .= dv
            , "cases" .= cases
            ]

instance FromJSON r => FromJSON (Rule r) where
    parseJSON = untagged "Rule"
        [ ("ContextFree", fmap ContextFree . parseJSON)
        , ("Dependent", withObject "Dependent" $ \o -> Dependent
          <$> o .: "path"
          <*> o .: "default"
          <*> o .: "cases"
          )
        ]

instance ToJSON Documentation where
    toJSON (Documentation definition description remark) = object
        [ "definition" .= definition
        , "description" .= description
        , "remark" .= remark
        ]

instance FromJSON Documentation where
    parseJSON = withObject "Documentation" $ \o -> Documentation
        <$> o .: "definition"
        <*> o .: "description"
        <*> o .: "remark"

instance ToJSON (NonSpare ()) where
    toJSON (NonSpare name title rule doc) = object
        [ "name" .= name
        , "title" .= title
        , "rule" .= rule
        , "documentation" .= doc
        ]

instance FromJSON (NonSpare ()) where
    parseJSON = withObject "NonSpare" $ \o -> NonSpare
        <$> o .: "name"
        <*> o .: "title"
        <*> o .: "rule"
        <*> o .: "documentation"

instance ToJSON a => ToJSON (UapItem a) where
    toJSON = \case
        UapItem a -> tagged "UapItem" a
        UapItemSpare -> tagged "UapItemSpare" ()
        UapItemRFS -> tagged "UapItemRFS" ()

instance FromJSON a => FromJSON (UapItem a) where
    parseJSON = untagged "UapItem"
        [ ("UapItem", fmap UapItem . parseJSON)
        , ("UapItemSpare", \o -> parseUnit o >> pure UapItemSpare)
        , ("UapItemRFS", \o -> parseUnit o >> pure UapItemRFS)
        ]

instance ToJSON UapSelector where
    toJSON (UapSelector item cases) = object
        [ "item" .= item
        , "cases" .= cases
        ]

instance FromJSON UapSelector where
    parseJSON = withObject "UapSelector" $ \o -> UapSelector
        <$> o .: "item"
        <*> o .: "cases"

instance ToJSON r => ToJSON (Uap r) where
    toJSON = \case
        Uap val -> tagged "Uap" val
        Uaps cases msel -> tagged "Uaps" $ object
            [ "cases" .= cases
            , "selector" .= msel
            ]

instance FromJSON r => FromJSON (Uap r) where
    parseJSON = untagged "Uap"
        [ ("Uap", fmap Uap . parseJSON)
        , ("Uaps", withObject "Uap" $ \o -> Uaps
            <$> o .: "cases"
            <*> o .: "selector"
            )
        ]

instance ToJSON Basic where
    toJSON (Basic cat title ed date preamble catalogue uap) = object
        [ "category" .= cat
        , "title" .= title
        , "edition" .= ed
        , "date" .= date
        , "preamble" .= preamble
        , "catalogue" .= catalogue
        , "uap" .= uap
        ]

instance FromJSON Basic where
    parseJSON = withObject "Basic" $ \o -> Basic
        <$> o .: "category"
        <*> o .: "title"
        <*> o .: "edition"
        <*> o .: "date"
        <*> o .: "preamble"
        <*> o .: "catalogue"
        <*> o .: "uap"

instance ToJSON Expansion where
    toJSON (Expansion cat title ed date fspecByteSize items) = object
        [ "category" .= cat
        , "title" .= title
        , "edition" .= ed
        , "date" .= date
        , "fspecByteSize" .= fspecByteSize
        , "items" .= items
        ]

instance FromJSON Expansion where
    parseJSON = withObject "Expansion" $ \o -> Expansion
        <$> o .: "category"
        <*> o .: "title"
        <*> o .: "edition"
        <*> o .: "date"
        <*> o .: "fspecByteSize"
        <*> o .: "items"

instance ToJSON Asterix where
    toJSON = \case
        AsterixBasic x -> tagged "AsterixBasic" x
        AsterixExpansion x -> tagged "AsterixExpansion" x

instance FromJSON Asterix where
    parseJSON = untagged "Asterix"
        [ ("AsterixBasic", fmap AsterixBasic . parseJSON)
        , ("AsterixExpansion", fmap AsterixExpansion . parseJSON)
        ]

coder :: Coder
coder = Coder
    { cDescription = "JSON format"
    , cDecoder = Just decoder
    , cEncoder = Just encoder
    }
  where
    decoder filename s = case eitherDecodeStrict (T.encodeUtf8 s) of
        Left e    -> Left $ filename ++ ": " ++ e
        Right val -> Right val
    encoder = encodePrettyToTextBuilder' JsonP.Config
        { confIndent = Spaces 4
        , confCompare = compare
        , confNumFormat = Generic
        , confTrailingNewline = False
        }
