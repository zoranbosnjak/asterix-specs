
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative as Opt
import           Data.Text.IO
import           System.IO (stdin)
import           System.Exit (die)
import qualified Data.ByteString.Lazy as BSL
import           Text.Megaparsec hiding (State)
import qualified Data.Aeson.Encode.Pretty as JsonP

import           Data.Asterix.Parser

data Input
    = FileInput FilePath
    | StdInput
    deriving (Show)

data OutputFormat
    = OutputJson
    deriving (Show)

data Options = Options
    { optInput          :: Input
    , optOutputFormat   :: OutputFormat
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> input
    <*> outfmt
  where
    input = fileInput <|> stdInput where
        fileInput = FileInput <$> strOption
            ( long "file"
           <> short 'f'
           <> metavar "FILENAME"
           <> help "Input file" )
        stdInput = flag' StdInput
            ( long "stdin"
           <> help "Read from stdin" )
    outfmt = json where
        json = flag' OutputJson ( long "json" <> help "JSON format" )

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    (s, name) <- case optInput opt of
        FileInput f -> (,) <$> Data.Text.IO.readFile f <*> pure f
        StdInput -> (,) <$> Data.Text.IO.hGetContents stdin <*> pure "<stdin>"

    case parse pCategory name s of
        Left e -> die $ errorBundlePretty e
        Right val -> case optOutputFormat opt of
            OutputJson -> do
                BSL.putStr $ JsonP.encodePretty'
                    JsonP.defConfig {JsonP.confCompare = compare} val

