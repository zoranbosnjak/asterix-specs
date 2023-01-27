module Folds
( module Folds
, module Control.Lens
, module Asterix.Specs
) where

import           Options.Applicative as Opt
import qualified Data.ByteString as BS

import           Control.Lens

import           Asterix.Specs
import           Asterix.Specs.Common

-- | Load spec file from disk.
loadSpec :: Monad m => String -> String -> m BS.ByteString -> m Asterix
loadSpec fmt path getS = do
    s <- getS
    let Just syntax = lookup fmt syntaxes
        Just decoder = syntaxDecoder syntax
        Right ast = decoder path s
    return ast

focusItemVariation :: Getter Item (Maybe Variation)
focusItemVariation = to $ \case
    Spare _n -> Nothing
    Item _name _title variation _doc -> Just variation

focusAsterixVariation :: Fold Asterix Variation
focusAsterixVariation = folding $ \case
    AsterixBasic bas -> bas ^.. to basCatalogue . folded . focusItemVariation . _Just
    AsterixExpansion val -> [expVariation val]

focusVariationRule :: Fold Variation Rule
focusVariationRule = folding $ \case
    Element _n rule -> [rule]
    Group lst -> lst ^.. folded . focusItemVariation . _Just . focusVariationRule
    Extended _et _n1 _n2 lst -> lst ^.. folded . focusItemVariation . _Just . focusVariationRule
    Repetitive _n variation -> variation ^.. focusVariationRule
    Explicit -> []
    Compound _mn lst -> lst ^.. folded . _Just . focusItemVariation . _Just . focusVariationRule

focusRuleContent :: Fold Rule Content
focusRuleContent = folding $ \case
    ContextFree content -> [content]
    Dependent _path lst -> lst ^.. folded . _2

focusContentUnit :: Getter Content (Maybe Unit)
focusContentUnit = to $ \case
    ContentQuantity _signed _number _fractBits unit _constraints -> Just unit
    _ -> Nothing

focusContentScaling :: Getter Content (Maybe Number)
focusContentScaling = to $ \case
    ContentQuantity _signed scaling _fractBits _unit _constraints -> Just scaling
    _ -> Nothing

