{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs.Syntax where

import           Data.Text              (Text, intercalate)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             as F

import           Asterix.Specs.Types

type Decoder = FilePath -> Text -> Either String Asterix
type Encoder = Asterix -> Builder

data Coder = Coder
    { cDescription :: String
    , cDecoder     :: Maybe Decoder
    , cEncoder     :: Maybe Encoder
    }

-- | Number pretty-printer. Try to avoid unnecessary parentheses.
showNumber :: Number -> Text
showNumber (NumInt i) = sformat int i
showNumber (NumPow a b) = sformat (int % "^" % int) a b
showNumber (NumDiv a b) = sformat (stext % "/" % stext) (f a) (f b)
  where
    f x = case x of
        NumDiv _ _ -> "(" <> showNumber x <> ")"
        _          -> showNumber x

showConstrain :: Constrain -> Text
showConstrain = \case
    EqualTo num -> "== " <> showNumber num
    NotEqualTo num -> "/= " <> showNumber num
    GreaterThan num -> "> " <> showNumber num
    GreaterThanOrEqualTo num -> ">= " <> showNumber num
    LessThan num -> "< " <> showNumber num
    LessThanOrEqualTo num -> "<= " <> showNumber num

showPath :: ItemPath -> Text
showPath (ItemPath lst) = intercalate "/" [name | ItemName name <- lst]

evalNumber :: Fractional a => Number -> a
evalNumber = \case
    NumInt i -> fromIntegral i
    NumDiv a b -> evalNumber a / evalNumber b
    NumPow a b -> fromIntegral (a ^ b)

