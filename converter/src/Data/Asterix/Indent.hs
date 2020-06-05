{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module:      Data.Asterix.Indent
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- Text indent handling.
--

module Data.Asterix.Indent where

import           Control.Monad.Trans.State
import           Data.Text (Text)
import qualified Data.Text as T

type Accumulator = State (Int, [(Int, Text)])

-- | Append line of text to accumulator.
tell :: Text -> Accumulator ()
tell t = mapM_ tellOne $ T.splitOn "\n" $ T.stripEnd t
  where
    tellOne a = modify $ \(indent, buffer) -> (indent, buffer <> [(indent, a)])

-- | Indented block.
block :: Accumulator a -> Accumulator a
block act = do
    modify $ \(a,b) -> (succ a, b)
    result <- act
    modify $ \(a,b) -> (pred a, b)
    return result

-- | Convert accumulator to text.
renderBuffer :: Int -> [(Int,Text)] -> Text
renderBuffer tabSize buffer = mconcat $ do
    (n,line) <- buffer
    let spaces = T.pack $ Prelude.take (tabSize*n) $ repeat ' '
    return $ case T.null line of
        True -> "\n"
        False -> spaces <> line <> "\n"

