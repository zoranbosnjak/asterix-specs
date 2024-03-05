-- '.json' syntax implementation
--
-- Conversion rules between haskell ADT types and JSON:
-- - newtypes wrappers are encoded as raw types
-- - product types are encoded as objects:
--      {'field1' : value1, 'field2' : value2, ...}
-- - sum types are encoded as arrays
--      [ {NameOfConstructorWithoutPrefix}, value1, value2, ...]

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Asterix.Specs.Syntax.Json where

import           Control.Monad
import           Data.Aeson               hiding (Encoding)
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.Aeson.Types
import           Data.List                (intersperse)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       as T
import qualified Data.Vector              as V

import           Asterix.Specs.Syntax
import           Asterix.Specs.Types

array :: [Value] -> Value
array = Array . V.fromList

parseSum :: String -> [(Text, (Int, V.Vector Value -> Parser a))] -> Value -> Parser a
parseSum name lst = withArray name $ \v -> do
    when (V.length v < 1) $ fail "array length"
    flip (withText name) (V.head v) $ \t -> case lookup t lst of
        Nothing -> fail $ "expecting ["
            <> join (intersperse ", " [T.unpack (fst i) | i <- lst])
            <> "], got '"
            <> T.unpack t <> "'"
        Just (n, f) -> do
            when (V.length v /= succ n) $ fail "array length"
            f $ V.tail v

instance ToJSON CatNum
instance FromJSON CatNum

instance ToJSON BitSize
instance FromJSON BitSize

instance ToJSON ByteSize
instance FromJSON ByteSize

instance ToJSON ItemName
instance FromJSON ItemName

instance ToJSON Title
instance FromJSON Title

instance ToJSON UapName
instance FromJSON UapName

instance ToJSON Unit
instance FromJSON Unit

instance ToJSON ItemPath
instance FromJSON ItemPath

instance ToJSON Documentation where
    toJSON (Documentation definition description remark) = object
        [ "definition" .= definition
        , "description" .= description
        , "remark" .= remark
        ]

instance FromJSON Documentation where
    parseJSON = withObject "Documentation" $ \v -> Documentation
        <$> v .: "definition"
        <*> v .: "description"
        <*> v .: "remark"

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
        NumInt a -> array ["Int", toJSON a]
        NumDiv a b -> array ["Div", toJSON a, toJSON b]
        NumPow a b -> array ["Pow", toJSON a, toJSON b]

instance FromJSON Number where
    parseJSON = parseSum "Num"
        [ ("Int", (1, \v -> NumInt <$> parseJSON (v V.! 0)))
        , ("Div", (2, \v -> NumDiv <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        , ("Pow", (2, \v -> NumPow <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        ]

instance ToJSON Constrain where
    toJSON = \case
        EqualTo n -> array ["==", toJSON n]
        NotEqualTo n -> array ["/=", toJSON n]
        GreaterThan n -> array [">", toJSON n]
        GreaterThanOrEqualTo n -> array [">=", toJSON n]
        LessThan n -> array ["<", toJSON n]
        LessThanOrEqualTo n -> array ["<=", toJSON n]

instance FromJSON Constrain where
    parseJSON = parseSum "Constrain"
        [ ("==", (1, \v -> EqualTo <$> parseJSON (v V.! 0)))
        , ("/=", (1, \v -> NotEqualTo <$> parseJSON (v V.! 0)))
        , (">", (1, \v -> GreaterThan <$> parseJSON (v V.! 0)))
        , (">=", (1, \v -> GreaterThanOrEqualTo <$> parseJSON (v V.! 0)))
        , ("<", (1, \v -> LessThan <$> parseJSON (v V.! 0)))
        , ("<=", (1, \v -> LessThanOrEqualTo <$> parseJSON (v V.! 0)))
        ]

instance ToJSON Signedness where
    toJSON = \case
        Signed -> array ["Signed"]
        Unsigned -> array ["Unsigned"]

instance FromJSON Signedness where
    parseJSON = parseSum "Signedness"
        [ ("Signed", (0, \_v -> pure Signed))
        , ("Unsigned", (0, \_v -> pure Unsigned))
        ]

instance ToJSON StringType where
    toJSON = \case
        StringAscii -> array ["StringAscii"]
        StringICAO -> array ["StringICAO"]
        StringOctal -> array ["StringOctal"]

instance FromJSON StringType where
    parseJSON = parseSum "StringType"
        [ ("StringAscii", (0, \_v -> pure StringAscii))
        , ("StringICAO", (0, \_v -> pure StringICAO))
        , ("StringOctal", (0, \_v -> pure StringOctal))
        ]

instance ToJSON BdsAddr
instance FromJSON BdsAddr

instance ToJSON BdsType where
    toJSON = \case
        BdsWithAddress -> array ["BdsWithAddress"]
        BdsAt ma -> array ["BdsAt", toJSON ma]

instance FromJSON BdsType where
    parseJSON = parseSum "BdsType"
        [ ("BdsWithAddress", (0, \_v -> pure BdsWithAddress))
        , ("BdsAt", (1, \v -> BdsAt <$> parseJSON (v V.! 0)))
        ]

instance ToJSON Content where
    toJSON = \case
        ContentRaw -> array ["Raw"]
        ContentTable lst -> array ["Table", toJSON lst]
        ContentString st -> array ["String", toJSON st]
        ContentInteger sig cstr -> array ["Integer", toJSON sig, toJSON cstr]
        ContentQuantity sig lsb unit cstr ->
            array ["Quantity", toJSON sig, toJSON lsb, toJSON unit, toJSON cstr]
        ContentBds bt -> array ["Bds", toJSON bt]

instance FromJSON Content where
    parseJSON = parseSum "Content"
        [ ("Raw", (0, \_v -> pure ContentRaw))
        , ("Table", (1, \v -> ContentTable <$> parseJSON (v V.! 0)))
        , ("String", (1, \v -> ContentString <$> parseJSON (v V.! 0)))
        , ("Integer", (2, \v -> ContentInteger <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        , ("Quantity", (4, \v -> ContentQuantity <$> parseJSON (v V.! 0)
                           <*> parseJSON (v V.! 1)
                           <*> parseJSON (v V.! 2)
                           <*> parseJSON (v V.! 3)))
        , ("Bds", (1, \v -> ContentBds <$> parseJSON (v V.! 0)))
        ]

instance ToJSON a => ToJSON (Rule a) where
    toJSON = \case
        ContextFree a -> array ["ContextFree", toJSON a]
        Dependent items dv cases -> array ["Dependent", toJSON items, toJSON dv, toJSON cases]

instance FromJSON a => FromJSON (Rule a) where
    parseJSON = parseSum "Rule"
        [ ("ContextFree", (1, \v -> ContextFree <$> parseJSON (v V.! 0)))
        , ("Dependent", (3, \v -> Dependent <$> parseJSON (v V.! 0)
                           <*> parseJSON (v V.! 1)
                           <*> parseJSON (v V.! 2)))
        ]

instance ToJSON RepetitiveType where
    toJSON = \case
        RepetitiveRegular bs -> array ["Regular", toJSON bs]
        RepetitiveFx -> array ["Fx"]

instance FromJSON RepetitiveType where
    parseJSON = parseSum "RepetitiveType"
        [ ("Regular", (1, \v -> RepetitiveRegular <$> parseJSON (v V.! 0)))
        , ("Fx", (0, \_v -> pure RepetitiveFx))
        ]

instance ToJSON ExplicitType where
    toJSON = \case
        ReservedExpansion -> array ["ReservedExpansion"]
        SpecialPurpose -> array ["SpecialPurpose"]

instance FromJSON ExplicitType where
    parseJSON = parseSum "ExplicitType"
        [ ("ReservedExpansion", (0, \_v -> pure ReservedExpansion))
        , ("SpecialPurpose", (0, \_v -> pure SpecialPurpose))
        ]

instance ToJSON a => ToJSON (Variation a) where
    toJSON = \case
        Element o n cont -> array ["Element", toJSON o, toJSON n, toJSON cont]
        Group lst -> array ["Group", toJSON lst]
        Extended lst -> array ["Extended", toJSON lst]
        Repetitive rt var -> array ["Repetitive", toJSON rt, toJSON var]
        Explicit mt -> array ["Explicit", toJSON mt]
        Compound lst -> array ["Compound", toJSON lst]

instance FromJSON a => FromJSON (Variation a) where
    parseJSON = parseSum "Variation"
        [ ("Element", (3, \v -> Element <$> parseJSON (v V.! 0)
                           <*> parseJSON (v V.! 1)
                           <*> parseJSON (v V.! 2)))
        , ("Group", (1, \v -> Group <$> parseJSON (v V.! 0)))
        , ("Extended", (1, \v -> Extended <$> parseJSON (v V.! 0)))
        , ("Repetitive", (2, \v -> Repetitive <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        , ("Explicit", (1, \v -> Explicit <$> parseJSON (v V.! 0)))
        , ("Compound", (1, \v -> Compound <$> parseJSON (v V.! 0)))
        ]

instance ToJSON a => ToJSON (Item a) where
    toJSON = \case
        Spare o n -> array ["Spare", toJSON o, toJSON n]
        Item name title rule doc ->
            array ["Item", toJSON name, toJSON title, toJSON rule, toJSON doc]

instance FromJSON a => FromJSON (Item a) where
    parseJSON = parseSum "Item"
        [ ("Spare", (2, \v -> Spare <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        , ("Item", (4, \v -> Item <$> parseJSON (v V.! 0)
                           <*> parseJSON (v V.! 1)
                           <*> parseJSON (v V.! 2)
                           <*> parseJSON (v V.! 3)))
        ]

instance ToJSON a => ToJSON (UapItem a) where
    toJSON = \case
        UapItem a -> array ["Item", toJSON a]
        UapItemSpare -> array ["Spare"]
        UapItemRFS -> array ["RFS"]

instance FromJSON a => FromJSON (UapItem a) where
    parseJSON = parseSum "UapItem"
        [ ("Item", (1, \v -> UapItem <$> parseJSON (v V.! 0)))
        , ("Spare", (0, \_v -> pure UapItemSpare))
        , ("RFS", (0, \_v -> pure UapItemRFS))
        ]

instance ToJSON UapSelector where
    toJSON (UapSelector item table) = object
        [ "item" .= item
        , "table" .= table
        ]

instance FromJSON UapSelector where
    parseJSON = withObject "UapSelector" $ \v -> UapSelector
        <$> v .: "item"
        <*> v .: "table"

instance ToJSON Uap where
    toJSON = \case
        Uap lst -> array ["Uap", toJSON lst]
        Uaps lst sel -> array ["Uaps", toJSON lst, toJSON sel]

instance FromJSON Uap where
    parseJSON = parseSum "Uap"
        [ ("Uap", (1, \v -> Uap <$> parseJSON (v V.! 0)))
        , ("Uaps", (2, \v -> Uaps <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)))
        ]

instance ToJSON Expansion where
    toJSON (Expansion catNum title edition date fspecSize items) = object
        [ "category" .= catNum
        , "title" .= title
        , "edition" .= edition
        , "date" .= date
        , "fspec-size" .= fspecSize
        , "items" .= items
        ]

instance FromJSON Expansion where
    parseJSON = withObject "Expansion" $ \v -> Expansion
        <$> v .: "category"
        <*> v .: "title"
        <*> v .: "edition"
        <*> v .: "date"
        <*> v .: "fspec-size"
        <*> v .: "items"

instance ToJSON Basic where
    toJSON (Basic catNum title edition date preamble catalogue uap) = object
        [ "category" .= catNum
        , "title" .= title
        , "edition" .= edition
        , "date" .= date
        , "preamble" .= preamble
        , "catalogue" .= catalogue
        , "uap" .= uap
        ]

instance FromJSON Basic where
    parseJSON = withObject "Basic" $ \v -> Basic
        <$> v .: "category"
        <*> v .: "title"
        <*> v .: "edition"
        <*> v .: "date"
        <*> v .: "preamble"
        <*> v .: "catalogue"
        <*> v .: "uap"

instance ToJSON Asterix where
    toJSON = \case
        AsterixBasic x -> array ["AsterixBasic", toJSON x]
        AsterixExpansion x -> array ["AsterixExpansion", toJSON x]

instance FromJSON Asterix where
    parseJSON = parseSum "Asterix"
        [ ("AsterixBasic", (1, \v -> AsterixBasic <$> parseJSON (v V.! 0)))
        , ("AsterixExpansion", (1, \v -> AsterixExpansion <$> parseJSON (v V.! 0)))
        ]

coder :: Coder
coder = Coder
    { cDescription = "JSON format"
    , cDecoder = Just decoder
    , cEncoder = Just encoder
    }
  where
    decoder filename s = case eitherDecodeStrict' (T.encodeUtf8 s) of
        Left e    -> Left $ filename ++ ": " ++ e
        Right val -> Right val

    encoder = JsonP.encodePrettyToTextBuilder' JsonP.defConfig
        {JsonP.confCompare = compare}
