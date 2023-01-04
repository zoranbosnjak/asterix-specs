-- | Block is a list/tree-like structure with support for indented sub-block(s).
-- Eventually, it is rendered to output with required indentation (spaces).

module Asterix.Indent
    ( Block
    , line
    , emptyLine
    , blocksLn
    , indent
    , enclose
    , renderBlock
    )
where

import           Data.List (intersperse)
import           Data.String (IsString, fromString)

-- | A 'Block' is either a line or nested sub-block plus continuation.
data Block a
    = Nil
    | Block
        (Either a (Block a))
        (Block a)
    deriving (Eq, Show)

instance Functor Block where
    fmap _ Nil = Nil
    fmap f (Block (Left a) cont) = Block (Left $ f a) (fmap f cont)
    fmap f (Block (Right sub) cont) = Block (Right $ fmap f sub) (fmap f cont)

instance Semigroup (Block a) where
    Nil <> val = val
    val <> Nil = val
    Block a1 a2 <> val = Block a1 (a2 <> val)

instance Monoid (Block a) where
    mempty = Nil

instance IsString a => IsString (Block a) where
    fromString s = Block (Left $ fromString s) Nil

-- | Single line.
line :: a -> Block a
line t = Block (Left t) Nil

-- | Empty line.
emptyLine :: Monoid a => Block a
emptyLine = line mempty

-- | Put empty line between sub-blocks.
blocksLn :: (Monoid a, Eq a) => [Block a] -> Block a
blocksLn lst = mconcat $ intersperse emptyLine $ filter (/= mempty) lst

-- | Indented block.
indent :: Block a -> Block a
indent body = Block (Right body) Nil

-- | Enclose indented body between 'header' and 'footer'.
enclose :: Block a -> Block a -> Block a -> Block a
enclose hdr ft body = mconcat [hdr, indent body, ft]

-- | Convert block to output string.
renderBlock :: (Eq t, Monoid t, IsString t) => Int -> Block t -> t
renderBlock tab = go mempty 0
  where
    prepend 0 _s = mempty
    prepend n s = s <> prepend (pred n) s
    go acc _level Nil = acc
    go acc level (Block val cont) = case val of
        Left s ->
            let t = case s == mempty of
                    True -> mempty
                    False -> prepend (tab*level) " " <> s
            in go (acc <> t <> "\n") level cont
        Right blk -> go (go acc (succ level) blk) level cont

