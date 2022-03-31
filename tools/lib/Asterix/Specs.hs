{-# LANGUAGE OverloadedStrings #-}

-- Common asterix data types and functions.

module Asterix.Specs
( module Asterix.Specs
, module Asterix.Specs.Types
, module Asterix.Specs.Syntax
) where

import           Data.List (find)
import           Data.Maybe (catMaybes)

import           Asterix.Specs.Types
import           Asterix.Specs.Syntax

findItemByName :: Basic -> [Name] -> Maybe Item
findItemByName _ [] = Nothing
findItemByName basic (x:xs) = do
    let f (Spare _) = False
        f (Item name _ _ _) = name == x
    item <- find f $ basCatalogue basic
    go item xs
  where
    go item [] = Just item
    go item (y:ys) = case item of
        Spare _ -> Nothing
        Item _ _ variation _ -> do
            let candidates = case variation of
                    Group lst -> lst
                    Extended _ _ lst -> lst
                    Compound _fspecSize lst -> catMaybes lst
                    _ -> []
                byName (Spare _) = False
                byName (Item n _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys

