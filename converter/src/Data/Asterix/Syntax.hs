{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Asterix.Syntax
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- Asterix syntaxes.
--

module Data.Asterix.Syntax
where

import           Data.Asterix.Common
import qualified Data.Asterix.Syntax.Ast
import qualified Data.Asterix.Syntax.Json

-- | Syntax implementations.
syntaxes :: [(String, Syntax)]
syntaxes =
    [ ("ast", Data.Asterix.Syntax.Ast.syntax)
    , ("json", Data.Asterix.Syntax.Json.syntax)
    ]

availableEncoders :: [(String, String, EncodeAsterix)]
availableEncoders = do
    (shortName, syntax) <- syntaxes
    f <- maybe [] pure $ encodeAsterix syntax
    return (shortName, syntaxDescription syntax, f)

availableDecoders :: [(String, String, DecodeAsterix)]
availableDecoders = do
    (shortName, syntax) <- syntaxes
    f <- maybe [] pure $ decodeAsterix syntax
    return (shortName, syntaxDescription syntax, f)

