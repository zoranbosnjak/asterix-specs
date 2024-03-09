-- Common asterix definitions.

module Asterix.Specs.Common where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting as F

import           Asterix.Specs.Types

type Encoder = Asterix -> ByteString
type Decoder = FilePath -> ByteString -> Either String Asterix

data Syntax = Syntax
    { syntaxDescription :: String
    , syntaxEncoder :: Maybe Encoder
    , syntaxDecoder :: Maybe Decoder
    }

instance Show Syntax where
    show = syntaxDescription

-- | Number pretty-printer. Try to avoid unnecessary parentheses.
showNumber :: Number -> Text
showNumber (NumInt i) = sformat (int) i
showNumber (NumPow a b) = sformat (int % "^" % int) a b
showNumber (NumDiv a b) = sformat (stext % "/" % stext) (f a) (f b)
  where
    f x = case x of
        NumDiv _ _ -> "(" <> showNumber x <> ")"
        _ -> showNumber x

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
