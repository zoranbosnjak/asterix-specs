
module Data.Asterix where

import           Data.List
import           Data.Asterix.Types

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
                    Compound lst -> lst
                    _ -> []
                byName (Spare _) = False
                byName (Subitem n _ _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys

