-- Common asterix data types and functions.

module Asterix.Specs
( module Asterix.Specs
, module Asterix.Specs.Common
, module Asterix.Specs.Types
, module Asterix.Specs.Syntax
) where

import           Control.Monad (guard)
import           Data.Function (fix)
import           Data.List (find)
import           Data.Maybe (catMaybes, isJust)

import           Asterix.Specs.Common
import           Asterix.Specs.Types
import           Asterix.Specs.Syntax

class Fixed a where
    bitSize :: a -> Maybe RegisterSize

instance Fixed a => Fixed [a] where
    bitSize lst = sum <$> sequence (fmap bitSize lst)

instance Fixed Variation where
    bitSize (Element n _content) = Just n
    bitSize (Group items) = bitSize items
    bitSize _ = Nothing

instance Fixed Item where
    bitSize (Spare n) = Just n
    bitSize (Item _name _title variation _doc) = bitSize variation

isFixed :: Fixed a => a -> Bool
isFixed = isJust . bitSize

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
                    Extended _ _ _ lst -> lst
                    Compound _fspecSize lst -> catMaybes lst
                    _ -> []
                byName (Spare _) = False
                byName (Item n _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys

extendedItemGroups :: ExtendedType -> RegisterSize -> RegisterSize -> [Item] -> Maybe [[Item]]
extendedItemGroups et n1 n2 lst = go [] (n1:repeat n2) lst
  where
    split :: RegisterSize -> [Item] -> Int -> Maybe ([Item], [Item])
    split fx items = fix $ \loop x -> do
        guard $ x <= length items
        let (a,b) = splitAt x items
        n <- bitSize a
        if
            | succ n >= fx -> pure (a,b)
            | otherwise -> loop (succ x)

    go acc _fx [] = pure acc
    go acc fx items = do
        (a,b) <- split (head fx) items 1
        n <- bitSize a
        let m = if
                | null b && et == ExtendedNoTrailingFx -> head fx
                | otherwise -> pred (head fx)
        guard $ n == m
        go (acc ++ [a]) (tail fx) b

