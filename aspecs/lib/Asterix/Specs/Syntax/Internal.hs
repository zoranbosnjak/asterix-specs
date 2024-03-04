-- | Internal syntax implementation

{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs.Syntax.Internal where

import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as T
import           Text.Read              (readEither)
import           Text.Show.Pretty       (ppShow)

import           Asterix.Specs.Syntax

coder :: Coder
coder = Coder
    { cDescription = "Internal format"
    , cDecoder = Just decoder
    , cEncoder = Just (T.fromText . T.pack . ppShow)
    }
  where
    decoder filename s = case readEither (T.unpack s) of
        Left e    -> Left $ filename ++ ": " ++ e
        Right val -> Right val
