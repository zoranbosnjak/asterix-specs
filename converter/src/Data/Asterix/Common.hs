{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Asterix.Common
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- Common asterix definitions.
--

module Data.Asterix.Common where

import           Data.ByteString (ByteString)

import           Data.Asterix.Types

type EncodeAsterix = Asterix -> ByteString
type DecodeAsterix = FilePath -> ByteString -> Either String Asterix

data Syntax = Syntax
    { syntaxDescription :: String
    , encodeAsterix :: Maybe EncodeAsterix
    , decodeAsterix :: Maybe DecodeAsterix
    }

instance Show Syntax where
    show = syntaxDescription

