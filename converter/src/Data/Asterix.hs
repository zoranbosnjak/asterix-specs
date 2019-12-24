
module Data.Asterix where

import           Data.List
import           Data.Asterix.Types

findItemByName :: Category -> [ItemName] -> Maybe Item
findItemByName _ [] = Nothing
findItemByName category (x:xs) = do
    let f (Spare _) = False
        f (Item name _ _ _ _) = name == x
    item <- find f (topItem <$> catItems category)
    go item xs
  where
    go item [] = Just item
    go item (y:ys) = case item of
        Spare _ -> Nothing
        Item _ _ _ variation _ -> do
            let candidates = case variation of
                    Group lst -> lst
                    Extended _ _ lst -> lst
                    Compound lst -> lst
                    _ -> []
                byName (Spare _) = False
                byName (Item n _ _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys

