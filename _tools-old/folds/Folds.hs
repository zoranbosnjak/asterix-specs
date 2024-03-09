module Folds
( module Folds
, module Control.Lens
, module Asterix.Specs
) where

import           Options.Applicative as Opt
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Either

import           Control.Lens

import           Asterix.Specs

-- | Load spec file from disk.
loadSpec :: Monad m => String -> String -> m BS.ByteString -> m Asterix
loadSpec fmt path getS = do
    s <- getS
    let syntax = fromJust $ lookup fmt syntaxes
        decoder = fromJust $ syntaxDecoder syntax
        ast = fromRight (error "unexpected") $ decoder path s
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
    Extended lst -> lst ^.. folded . _Just . focusItemVariation . _Just . focusVariationRule
    Repetitive _n variation -> variation ^.. focusVariationRule
    Explicit _ -> []
    RandomFieldSequencing -> []
    Compound _mn lst -> lst ^.. folded . _Just . focusItemVariation . _Just . focusVariationRule

focusRuleContent :: Fold Rule Content
focusRuleContent = folding $ \case
    ContextFree content -> [content]
    Dependent _path lst -> lst ^.. folded . _2

focusContentUnit :: Getter Content (Maybe Unit)
focusContentUnit = to $ \case
    ContentQuantity _signedness _lsb unit _constraints -> Just unit
    _ -> Nothing

focusContentLsb :: Getter Content (Maybe Number)
focusContentLsb = to $ \case
    ContentQuantity _signedness lsb _unit _constraints -> Just lsb
    _ -> Nothing
