{-# LANGUAGE OverloadedStrings #-}

-- Asterix syntaxes.

module Asterix.Specs.Syntax
where

import           Asterix.Specs.Common
import qualified Asterix.Specs.Syntax.Ast
import qualified Asterix.Specs.Syntax.Json

-- | Syntax implementations.
syntaxes :: [(String, Syntax)]
syntaxes =
    [ ("ast", Asterix.Specs.Syntax.Ast.syntax)
    , ("json", Asterix.Specs.Syntax.Json.syntax)
    ]

availableEncoders :: [(String, String, Encoder)]
availableEncoders = do
    (shortName, syntax) <- syntaxes
    f <- maybe [] pure $ syntaxEncoder syntax
    return (shortName, syntaxDescription syntax, f)

availableDecoders :: [(String, String, Decoder)]
availableDecoders = do
    (shortName, syntax) <- syntaxes
    f <- maybe [] pure $ syntaxDecoder syntax
    return (shortName, syntaxDescription syntax, f)

availablePrettifiers :: [(String, String, Decoder, Encoder)]
availablePrettifiers = do
    (shortName, syntax) <- syntaxes
    case (syntaxDecoder syntax, syntaxEncoder syntax) of
        (Just decoder, Just encoder) -> return (shortName, syntaxDescription syntax, decoder, encoder)
        _ -> []

