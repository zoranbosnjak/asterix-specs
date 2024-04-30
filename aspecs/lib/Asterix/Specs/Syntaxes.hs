{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs.Syntaxes
( module Asterix.Specs.Syntaxes
, module Asterix.Specs.Syntax
) where

import           Asterix.Specs.Syntax
import qualified Asterix.Specs.Syntax.Ast      as Sast
import qualified Asterix.Specs.Syntax.Internal as Sint
import qualified Asterix.Specs.Syntax.Json     as Sjson

syntaxes :: [(String, Coder)]
syntaxes =
    [ ("internal", Sint.coder)
    , ("ast", Sast.coder)
    , ("json", Sjson.coder)
    ]
