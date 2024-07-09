{-# LANGUAGE LambdaCase        #-}

module Folds
( module Folds
, module Asterix.Specs
) where

import           Data.Text
import           Data.Maybe
import           Data.Either

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

fVarRuleContent :: Variation () -> [Rule Content]
fVarRuleContent = \case
    Element _ _ val -> [val]
    Group _ lst -> lst >>= fItemRuleContent
    Extended lst -> catMaybes lst >>= fItemRuleContent
    Repetitive _ var -> [var] >>= fVarRuleContent
    Explicit _ -> []
    Compound lst -> catMaybes lst
        >>= fNspRuleVar
        >>= fRuleX
        >>= fVarRuleContent

fItemNsp :: Item a -> [NonSpare a]
fItemNsp = \case
    Spare _ _ -> []
    Item nsp -> [nsp]

fItemRuleContent :: Item () -> [Rule Content]
fItemRuleContent item = [item]
    >>= fItemNsp
    >>= fNspRuleVar
    >>= fRuleX
    >>= fVarRuleContent

fContentUnit :: Content -> [Unit]
fContentUnit = \case
    ContentQuantity _sig _lsb unit _cstr -> [unit]
    _ -> []

fContentLsb :: Content -> [Number]
fContentLsb = \case
    ContentQuantity _sig lsb _unit _cstr -> [lsb]
    _ -> []
