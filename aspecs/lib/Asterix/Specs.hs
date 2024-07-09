-- | Common asterix data types and functions.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs
( module Asterix.Specs
, module Asterix.Specs.Types
, module Asterix.Specs.Syntax
) where

import           Data.Foldable
import           Data.Maybe

import           Asterix.Specs.Syntax
import           Asterix.Specs.Types

newtype BitOffset = BitOffset Int
    deriving (Show, Eq, Ord)

instance Semigroup BitOffset where
    BitOffset a <> BitOffset b = bitOffset (a+b)

instance Monoid BitOffset where
    mempty = bitOffset 0

bitOffset :: Int -> BitOffset
bitOffset n = BitOffset (n `mod` 8)

sameValue :: Eq a => [a] -> a
sameValue [] = error "empty list"
sameValue [x] = x
sameValue (x:y:xs)
    | x == y = sameValue (y:xs)
    | otherwise = error "value is not the same"

getConstrainNumber :: Constrain -> Number
getConstrainNumber = \case
    EqualTo val -> val
    NotEqualTo val -> val
    GreaterThan val -> val
    GreaterThanOrEqualTo val -> val
    LessThan val -> val
    LessThanOrEqualTo val -> val

evalNumber :: Fractional a => Number -> a
evalNumber = \case
    NumInt i -> fromIntegral i
    NumDiv a b -> evalNumber a / evalNumber b
    NumPow a b -> fromIntegral (a ^ b)

isNegative :: Number -> Bool
isNegative = \case
    NumInt val -> val < 0
    NumDiv a b -> isNegative a /= isNegative b
    NumPow a _ -> a < 0

class IsAligned a where
    offset :: a -> BitOffset
    bitSize :: a -> Maybe Int

isFixed :: IsAligned a => a -> Bool
isFixed = isJust . bitSize

instance IsAligned BitSize where
    offset (BitSize n) = bitOffset n
    bitSize (BitSize n) = Just n

instance IsAligned a => IsAligned (Rule a) where
    offset = sameValue . fmap offset . toList
    bitSize = sameValue . fmap bitSize . toList

instance IsAligned (Variation a) where
    offset (Element _ n _rule) = offset n
    offset (Group _ lst) = mconcat $ fmap offset lst
    offset (Extended lst) = mconcat $ do
        lst >>= \case
            Nothing -> pure $ bitOffset 1
            Just item -> pure $ offset item
    offset (Repetitive rt variation) = case rt of
        RepetitiveRegular _ -> offset variation
        RepetitiveFx        -> offset variation <> bitOffset 1
    offset (Explicit _) = mempty
    offset (Compound lst) = sameValue $ do
        lst >>= \case
            Nothing -> pure mempty
            Just item -> pure $ offset item

    bitSize = \case
        Element _ n _rule -> bitSize n
        Group _ lst -> sum <$> mapM bitSize lst
        _ -> Nothing

instance IsAligned (NonSpare a) where
    offset (NonSpare _name _title rule _doc) = offset rule
    bitSize (NonSpare _name _title rule _doc) = bitSize rule

instance IsAligned (Item a) where
    offset (Spare _ n) = offset n
    offset (Item nsp)  = offset nsp

    bitSize (Spare _ n) = bitSize n
    bitSize (Item nsp)  = bitSize nsp

findItemByName :: [NonSpare a] -> ItemPath -> Maybe (NonSpare a)
findItemByName _ (ItemPath []) = Nothing
findItemByName catalogue (ItemPath (x : xs)) = do
    let f (NonSpare name _ _ _) = name == x
    item <- find f catalogue
    go item xs
  where
    go :: NonSpare a -> [ItemName] -> Maybe (NonSpare a)
    go item [] = Just item
    go (NonSpare _ _ rule _) (y:ys) = case rule of
        ContextFree variation -> do
            let candidates = case variation of
                    Group _ lst   -> lst >>= \case
                        Spare _ _ -> []
                        Item nsp -> [nsp]
                    Extended lst -> lst >>= \case
                        Just (Item nsp) -> [nsp]
                        _ -> []
                    Compound lst -> catMaybes lst
                    _            -> []
                byName (NonSpare n _ _ _) = n == y
            nextItem <- find byName candidates
            go nextItem ys
        Dependent {} -> Nothing
