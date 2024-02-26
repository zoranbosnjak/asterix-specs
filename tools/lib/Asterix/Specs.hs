-- Common asterix data types and functions.

module Asterix.Specs
( module Asterix.Specs
, module Asterix.Specs.Common
, module Asterix.Specs.Types
, module Asterix.Specs.Syntax
) where

import           Data.List (find)
import           Data.Maybe (catMaybes, isJust)

import           Asterix.Specs.Common
import           Asterix.Specs.Types
import           Asterix.Specs.Syntax

class Fixed a where
    bitSize :: a -> Maybe RegisterSize

instance Fixed a => Fixed [a] where
    bitSize lst = sum <$> sequence (fmap bitSize lst)

instance Fixed a => Fixed (Rule a) where
    bitSize = \case
        ContextFree value -> bitSize value
        Dependent _item dv _lst -> bitSize dv

instance Fixed Variation where
    bitSize (Element n _rule) = Just n
    bitSize (Group items) = bitSize items
    bitSize _ = Nothing

instance Fixed Item where
    bitSize (Spare n) = Just n
    bitSize (Item _name _title rule _doc) = bitSize rule

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
        Item _ _ rule _ -> case rule of
            ContextFree variation -> do
                let candidates = case variation of
                        Group lst -> lst
                        Extended lst -> catMaybes lst
                        Compound _fspecSize lst -> catMaybes lst
                        _ -> []
                    byName (Spare _) = False
                    byName (Item n _ _ _) = n == y
                nextItem <- find byName candidates
                go nextItem ys
            Dependent _ _ _ -> Nothing
