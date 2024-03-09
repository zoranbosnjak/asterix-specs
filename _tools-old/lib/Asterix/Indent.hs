--------
-- |
-- Module : Asterix.Indent
--
-- This module provides primitives to support source code generation.
-- A basic structure is "Block" - a list/tree-like structure,
-- with support for indented sub-block(s). It's parametrized over
-- string type, like ["String", "Text", "Text.Builder",...]
-- Eventually, it is rendered to output with required indentation (spaces).
--
-- Monoidal and Monadic interfaces are provided for convenience.
--
-- == Example code builder
--
-- === Monoidal "Block" builder
--
-- > prog1 :: Block String
-- > prog1 = mconcat
-- >     [ "test1"
-- >     , line ("test" <> show (2::Int))
-- >     , indent $ mconcat
-- >         [ "test3"
-- >        , "test4"
-- >        ]
-- >    , "test5"
-- >    ]
--
-- === Monadic "BlockM" builder
--
-- > prog2 :: BlockM String ()
-- > prog2 = do
-- >     "test1"
-- >     line ("test" <> show (2::Int))
-- >     indent $ do
-- >         "test3"
-- >         "test4"
-- >     "test5"
--
-- === Rendering
--
-- Both versions produce the same result.
--
-- > test :: IO ()
-- > test = do
-- >     print (prog1 == toBlock prog2)
-- >     putStrLn $ render "    " "\n" prog1
-- >     putStrLn $ render "    " "\n" prog2
--
-- > test1
-- > test2
-- >     test3
-- >     test4
-- > test5

module Asterix.Indent where

import Data.String (IsString, fromString)
import Control.Monad.Trans.Writer

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

lineBlock :: t -> Block t
lineBlock s = Block (Left s) Nil

indentBlock :: Block t -> Block t
indentBlock body = Block (Right body) Nil

renderBlock :: (Eq t, Monoid t) => t -> t -> Block t -> t
renderBlock tab newline = go mempty 0
  where
    prepend (n :: Int)
        | n <= 0 = mempty
        | otherwise = tab <> prepend (pred n)
    go acc _level Nil = acc
    go acc level (Block val cont) = case val of
        Left s ->
            let t = case s == mempty of
                    True -> mempty
                    False -> prepend level <> s
            in go (acc <> t <> newline) level cont
        Right blk -> go (go acc (succ level) blk) level cont

-- | Monadic block (Writer wrapper)
newtype BlockM t a = BlockM { unBlockM :: Writer (Block t) a }
    deriving (Functor, Applicative, Monad)

instance Semigroup a => Semigroup (BlockM t a) where
    act1 <> act2 = (<>) <$> act1 <*> act2

instance Monoid a => Monoid (BlockM t a) where
    mempty = pure mempty

instance (IsString t, a ~ ()) => IsString (BlockM t a) where
    fromString s = BlockM $ do
        tell $ fromString s

lineBlockM :: t -> BlockM t ()
lineBlockM t = BlockM $ do
    tell $ lineBlock t

indentBlockM :: BlockM t () -> BlockM t ()
indentBlockM (BlockM act) = BlockM $ do
    tell $ indentBlock (execWriter act)

class HasIndent t where
    indent :: t -> t

class HasIndent t => IsBlock t a where
    line :: a -> t
    toBlock :: t -> Block a

render :: (Eq c, Monoid c, IsBlock a c) => c -> c -> a -> c
render tab newline = renderBlock tab newline . toBlock

instance HasIndent (Block a) where
    indent = indentBlock

instance (a ~ b) => IsBlock (Block a) b where
    line = lineBlock
    toBlock = id

instance HasIndent (BlockM t ()) where
    indent = indentBlockM

instance (a ~ b, t ~ ()) => IsBlock (BlockM a t) b where
    line = lineBlockM
    toBlock = execWriter . unBlockM
