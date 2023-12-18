{-# LANGUAGE PackageImports #-}

-- Specs migration script:
--  - load old spec (use old decoder)
--  - adjust structure (old -> new)
--  - dump new structure (use new encoder)

module Main where

import           Unsafe.Coerce
import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Either
import           Data.Ratio

import "aspecs" Asterix.Specs.Common as Old
import "aspecs" Asterix.Specs.Types as Old
import "aspecs" Asterix.Specs.Syntax.Ast as Old

import "this" Asterix.Specs.Common as New
import "this" Asterix.Specs.Types as New
import "this" Asterix.Specs.Syntax.Ast as New

data Args = Args
    { argFile :: FilePath
    , argInPlace :: Bool
    }

args :: Parser Args
args = Args
    <$> strArgument ( metavar "FILENAME"
        <> help "File to process in .ast format" )
    <*> switch ( long "in-place" <> short 'i'
        <> help "Update file in place")

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc <> Opt.header "Data migration script" )

main :: IO ()
main = withUtf8 $ execParser opts >>= \a -> do
    let path = argFile a
    s <- BS.readFile path
    let decoder = fromJust $ Old.syntaxDecoder $ Old.syntax
        encoder = fromJust $ New.syntaxEncoder $ New.syntax
        asterix = fromRight (error "decoder problem") $ decoder path s
        asterix' = migAsterix asterix
    case argInPlace a of
        False -> print asterix'
        True -> BS.writeFile path $ encoder asterix'
  where
    uc = unsafeCoerce

    migAsterix = \case
        Old.AsterixBasic val -> New.AsterixBasic $ migBasic val
        Old.AsterixExpansion val -> New.AsterixExpansion $ migExpansion val

    migBasic (Old.Basic cat title edition date preamble items uap) =
        New.Basic cat title (uc edition) (uc date) preamble (fmap migItem items) (uc uap)

    migExpansion (Old.Expansion cat title edition date var) =
        New.Expansion cat title (uc edition) (uc date) (migVariation var)

    migItem = \case
        Old.Spare n -> New.Spare n
        Old.Item name title var doc -> New.Item name title (migVariation var) (uc doc)

    migVariation = \case
        Old.Element n r -> New.Element n (migRule r)
        Old.Group lst -> New.Group (fmap migItem lst)
        Old.Extended lst -> New.Extended (fmap (fmap migItem) lst)
        Old.Repetitive rt var -> New.Repetitive (uc rt) (migVariation var)
        Old.Explicit t -> New.Explicit (uc t)
        Old.RandomFieldSequencing -> New.RandomFieldSequencing
        Old.Compound mn lst -> New.Compound mn (fmap (fmap migItem) lst)

    migRule = \case
        Old.ContextFree cont -> New.ContextFree $ migContent cont
        Old.Dependent name lst -> New.Dependent name $ do
            (a, cont) <- lst
            pure (a, migContent cont)

    migContent = \case
        Old.ContentRaw -> New.ContentRaw
        Old.ContentTable lst -> New.ContentTable lst
        Old.ContentString st -> New.ContentString (uc st)
        Old.ContentInteger t lst -> New.ContentInteger (uc t) (fmap migConstrain lst)
        Old.ContentQuantity sig scal frac unit lst ->
            New.ContentQuantity (uc sig) lsb unit (fmap migConstrain lst)
          where
            lsb = case (scal, frac) of
                (_, 0) -> migNumber scal
                (NumberZ i, 1) -> New.NumDiv (New.NumInt i) (New.NumInt 2)
                (NumberZ i, n) -> New.NumDiv
                    (New.NumInt i)
                    (New.NumPow 2 $ fromIntegral n)
                _ -> error $ "unhandled LSB: " <> show (scal, frac)
        Old.ContentBds t -> New.ContentBds (uc t)

    migConstrain = \case
        Old.EqualTo n -> New.EqualTo (migNumber n)
        Old.NotEqualTo n -> New.NotEqualTo (migNumber n)
        Old.GreaterThan n -> New.GreaterThan (migNumber n)
        Old.GreaterThanOrEqualTo n -> New.GreaterThanOrEqualTo (migNumber n)
        Old.LessThan n -> New.LessThan (migNumber n)
        Old.LessThanOrEqualTo n -> New.LessThanOrEqualTo (migNumber n)

    migNumber = \case
        Old.NumberZ i -> New.NumInt i
        Old.NumberQ rat -> go rat
        Old.NumberR rat -> go rat
      where
        go rat = New.NumDiv
            (New.NumInt $ numerator rat)
            (New.NumInt $ denominator rat)
