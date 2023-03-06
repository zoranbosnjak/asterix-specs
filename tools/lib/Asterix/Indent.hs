-- | Block is a list/tree-like structure with support for indented sub-block(s).
-- Eventually, it is rendered to output with required indentation (spaces).

module Asterix.Indent where

import           Data.List (intersperse)
import           Data.String (IsString, fromString)
import           Control.Monad.Trans.Writer

-- | Building block
data Block a
    = Nil
    | Block
        (Either a (Block a))    -- line or nested sub-block
        (Block a)               -- continuation
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

lineB :: t -> Block t
lineB t = Block (Left t) Nil

indentB :: Block t -> Block t
indentB body = Block (Right body) Nil

-- | Monadic block construction helper type
newtype BlockM t a = BlockM { unBlockM :: Writer (Block t) a }
    deriving (Eq, Show, Functor, Applicative, Monad)

instance Semigroup a => Semigroup (BlockM t a) where
    act1 <> act2 = (<>) <$> act1 <*> act2

instance Monoid a => Monoid (BlockM t a) where
    mempty = pure mempty

instance (IsString t, Monoid a) => IsString (BlockM t a) where
    fromString s = BlockM (tell (fromString s) >> pure mempty)

line :: t -> BlockM t ()
line t = BlockM (tell $ lineB t)

indent :: BlockM t () -> BlockM t ()
indent (BlockM act) = BlockM (tell (indentB (execWriter act)))

-- | Empty line
emptyLine :: Monoid a => BlockM a ()
emptyLine = line mempty

-- | Put empty line between sub-blocks
blocksLn :: (Monoid a, Eq a) => [BlockM a ()] -> BlockM a ()
blocksLn lst = mconcat $ intersperse emptyLine $ filter (/= mempty) lst

-- | Enclose indented body between 'header' and 'footer'.
enclose :: BlockM t () -> BlockM t () -> BlockM t () -> BlockM t ()
enclose hdr ft body = mconcat [hdr, indent body, ft]

-- | Convert block to output string (Block)
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

-- | Convert block to output string (BlockM)
renderBlockM :: (Eq t, Monoid t, IsString t) => Int -> BlockM t a -> t
renderBlockM tab (BlockM act) = renderBlock tab $ execWriter act

