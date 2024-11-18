{-# LANGUAGE LambdaCase #-}

module Folds
( module Folds
, module Asterix.Specs
) where

import           Data.Either
import           Data.Maybe
import           Data.Text

import           Asterix.Specs
import           Asterix.Specs.Syntaxes

-- | Load spec file from disk.
loadSpec :: Monad m => String -> String -> m Text -> m Asterix
loadSpec fmt path getS = do
    s <- getS
    let syntax = fromJust $ lookup fmt syntaxes
        decoder = fromJust $ cDecoder syntax
        ast = fromRight (error "unexpected") $ decoder path s
    return ast

augment :: (a -> b) -> (a -> [c]) -> a -> [(b, [c])]
augment f1 f2 x = do
    let b = f1 x
    c <- f2 x
    pure (b, pure c)

fAstNsp :: Asterix -> [NonSpare ()]
fAstNsp = \case
    AsterixBasic val -> basCatalogue val
    AsterixExpansion val -> catMaybes $ expItems val

fNspRuleVar :: NonSpare () -> [Rule (Variation ())]
fNspRuleVar (NonSpare _name _title rule _doc) = [rule]

fRuleX :: Rule a -> [a]
fRuleX = \case
    ContextFree val -> [val]
    Dependent _p dv cases -> [dv] <> fmap snd cases

fVarSizeRuleContent :: Variation () -> [(BitSize, Rule Content)]
fVarSizeRuleContent = \case
    Element _ n val -> [(n, val)]
    Group _ lst -> lst >>= fItemSizeRuleContent
    Extended lst -> catMaybes lst >>= fItemSizeRuleContent
    Repetitive _ var -> [var] >>= fVarSizeRuleContent
    Explicit _ -> []
    Compound lst -> catMaybes lst
        >>= fNspRuleVar
        >>= fRuleX
        >>= fVarSizeRuleContent
  where
    fItemSizeRuleContent :: Item () -> [(BitSize, Rule Content)]
    fItemSizeRuleContent item = [item]
        >>= fItemNsp
        >>= fNspRuleVar
        >>= fRuleX
        >>= fVarSizeRuleContent

fItemNsp :: Item a -> [NonSpare a]
fItemNsp = \case
    Spare _ _ -> []
    Item nsp -> [nsp]

fContentUnit :: Content -> [Unit]
fContentUnit = \case
    ContentQuantity _sig _lsb unit _cstr -> [unit]
    _ -> []

fContentLsb :: Content -> [Number]
fContentLsb = \case
    ContentQuantity _sig lsb _unit _cstr -> [lsb]
    _ -> []

fContentString :: Content -> [StringType]
fContentString = \case
    ContentString st -> [st]
    _ -> []
