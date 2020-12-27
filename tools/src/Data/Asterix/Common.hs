{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Common asterix definitions.

module Data.Asterix.Common where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Ratio
import           Numeric
import           Formatting as F

import           Data.Asterix.Types

type Encoder = Asterix -> ByteString
type Decoder = FilePath -> ByteString -> Either String Asterix

data Syntax = Syntax
    { syntaxDescription :: String
    , syntaxEncoder :: Maybe Encoder
    , syntaxDecoder :: Maybe Decoder
    }

instance Show Syntax where
    show = syntaxDescription

showNumber :: Number -> Text
showNumber = \case
    NumberZ i -> sformat (int) i
    NumberQ q -> sformat (int % "/" % int)
        (Data.Ratio.numerator q) (Data.Ratio.denominator q)
    NumberR r -> sformat (F.string) (showFFloat Nothing r "")

showConstrain :: Constrain -> Text
showConstrain = \case
    EqualTo num -> "== " <> showNumber num
    NotEqualTo num -> "/= " <> showNumber num
    GreaterThan num -> "> " <> showNumber num
    GreaterThanOrEqualTo num -> ">= " <> showNumber num
    LessThan num -> "< " <> showNumber num
    LessThanOrEqualTo num -> "<= " <> showNumber num

showPath :: [Name] -> Text
showPath = T.intercalate "/"

