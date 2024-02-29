{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- TODO: remove this
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Main.Utf8                (withUtf8)
import           Options.Applicative as Opt
import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO as IO
import           System.Exit (die)
-- import           Crypto.Hash

import           Data.Version (showVersion)
import           Paths_aspecs (version)

import           Asterix.Specs.Types
import           Asterix.Specs.Syntax
-- import           Asterix.Specs.Validation (validate)
import qualified Asterix.Specs.Syntax.Ast as Sast

syntaxes :: [(String, String, Maybe Decoder, Maybe Encoder)]
syntaxes =
    [ ("ast", "Compact asterix syntax", Just Sast.decoder, Just Sast.encoder)
    -- , ("json", "JSON asterix syntax", Just undefined, Just undefined)
    ]

data Input
    = FileInput FilePath
    | StdInput

data Command
    = Dump Input Decoder
    {-
    | Validate Input Decoder
    | Checksum Input Decoder
    | Convert Input Decoder Encoder
    | Prettify FilePath (Decoder, Encoder)
    | Pandoc Input -- generate (pandoc) documentation
    -}

pInput :: Parser Input
pInput = fileInput <|> stdInput where
    fileInput = FileInput <$> strOption
        ( long "file" <> short 'f' <> metavar "FILENAME"
       <> help "Input file")
    stdInput = flag' StdInput
        ( long "stdin" <> help "Read from stdin"
        )

pDecoder :: Parser Decoder
pDecoder = foldr (<|>) empty $ do
    (name, description, mDecoder, _mEncoder) <- syntaxes
    case mDecoder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long name <> help description)

pEncoder :: Parser Encoder
pEncoder = foldr (<|>) empty $ do
    (name, description, _mDecoder_, mEncoder) <- syntaxes
    case mEncoder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long name <> help description)

pCommand :: Parser Command
pCommand = hsubparser
    ( command "dump" (info pDump (progDesc "Dump specification for test purposes"))
   -- <> command "checksum" (info pChecksum (progDesc "Print checksum"))
   -- ...
    )
  where
    pDump = Dump <$> pInput <*> pDecoder

pOpts :: ParserInfo Command
pOpts = info (helper <*> versionOption <*> pCommand)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

decodeInput :: Input -> Decoder -> IO Asterix
decodeInput input decoder = do
    (s, filename) <- case input of
        FileInput f -> (,) <$> T.readFile f <*> pure f
        StdInput -> (,) <$> T.hGetContents IO.stdin <*> pure "<stdin>"
    either die pure (decoder filename s)

main :: IO ()
main = withUtf8 $ execParser pOpts >>= \case
    Dump input decoder -> do
        asterix <- decodeInput input decoder
        print asterix
