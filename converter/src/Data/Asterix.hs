{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Asterix
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- Common asterix data types and functions.
--

module Data.Asterix
( module Data.Asterix
, module Data.Asterix.Types
, module Data.Asterix.Syntax
) where

import           Data.List
import           Data.Maybe (catMaybes)
import           Data.ByteString (ByteString)
import qualified Data.Text as T

import           Data.Asterix.Types
import           Data.Asterix.Syntax

type EncodeAsterix = Asterix -> ByteString
type DecodeAsterix = FilePath -> ByteString -> Either String Asterix

data Syntax = Syntax
    { syntaxDescription :: String
    , encodeAsterix :: Maybe EncodeAsterix
    , decodeAsterix :: Maybe DecodeAsterix
    }

instance Show Syntax where
    show = syntaxDescription

showPath :: [Name] -> T.Text
showPath = T.intercalate "/"

findSubitemByName :: Asterix -> [Name] -> Maybe Subitem
findSubitemByName _ [] = Nothing
findSubitemByName asterix (x:xs) = do
    let f (Spare _) = False
        f (Subitem name _ _ _ _) = name == x
    subitem <- find f (itemSubitem <$> astCatalogue asterix)
    go subitem xs
  where
    go subitem [] = Just subitem
    go subitem (y:ys) = case subitem of
        Spare _ -> Nothing
        Subitem _ _ _ element _ -> do
            let candidates = case element of
                    Group lst -> lst
                    Extended _ _ lst -> lst
                    Compound lst -> catMaybes lst
                    _ -> []
                byName (Spare _) = False
                byName (Subitem n _ _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys

