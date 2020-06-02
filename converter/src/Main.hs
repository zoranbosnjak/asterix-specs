
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.IO
import           System.IO as IO
import           System.Exit (die)
import qualified Data.ByteString as BS
import           Data.Maybe

import           Data.Asterix
import           Data.Asterix.Validation (validate, isValid)

data Input
    = FileInput FilePath
    | StdInput

data Output
    = ValidateOnly
    | OutputList
    | OutputSyntax EncodeAsterix

data Options = Options
    { optInput  :: (Input, DecodeAsterix)
    , optOutput :: Output
    }

options :: Opt.Parser Options
options = Options
    <$> inputOpts
    <*> outputOpts
  where
    inputOpts = (,) <$> (fileInput <|> stdInput) <*> decoderOpt where
        fileInput = FileInput <$> strOption
            ( long "file"
           <> short 'f'
           <> metavar "FILENAME"
           <> help "Input file" )
        stdInput = flag' StdInput
            ( long "stdin"
           <> help "Read from stdin" )
        decoderOpt = foldr (<|>) empty $ do
            (shortName, description, f) <- availableDecoders
            return $ flag' f (long shortName <> help ("input syntax: " ++ description))
    outputOpts = (validateOnly <|> outList <|> (fmap OutputSyntax astEncoder)) where
        validateOnly = flag' ValidateOnly ( long "validate" <> help "validate only" )
        outList = flag' OutputList ( long "list" <> help "list output" )
        astEncoder = foldr (<|>) empty $ do
            (shortName, description, f) <- availableEncoders
            return $ flag' f (long shortName <> help ("output syntax: " ++ description))

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    result <- do
        let (i, astDecoder) = optInput opt
        (s, filename) <- case i of
            FileInput f -> (,) <$> BS.readFile f <*> pure f
            StdInput -> (,) <$> BS.hGetContents IO.stdin <*> pure "<stdin>"
        return $ astDecoder filename s

    case result of
        Left e -> die e
        Right asterix -> case optOutput opt of
            ValidateOnly -> case validate asterix of
                [] -> print ("ok" :: String)
                lst -> do
                    mapM_ (Data.Text.IO.hPutStrLn stderr) lst
                    IO.hPutStrLn stderr ""
                    die "Validation error(s) present!"
            OutputList -> do
                mapM_ (dumpItem []) (astCatalogue asterix)
                unless (isValid asterix) $ do
                    die "Validation error(s) present, run 'validate' for details!"
            OutputSyntax astEncoder -> do
                BS.putStr $ astEncoder asterix
                unless (isValid asterix) $ do
                    die "Validation error(s) present, run 'validate' for details!"
  where
    dumpItem parent = \case
        Spare _ -> return ()
        Item name _title variation _doc -> do
            let path = parent ++ [name]
                next = \case
                    Element size _ -> ("Element " <> T.pack (show size), return ())
                    Group lst -> ("Group", mapM_ (dumpItem path) lst)
                    Extended _ _ lst -> ("Extended", mapM_ (dumpItem path) lst)
                    Repetitive _ var -> ("Repetitive", snd $ next var)
                    Explicit -> ("Explicit", return ())
                    Compound lst -> ("Compound", mapM_ (dumpItem path) (catMaybes lst))
                (details, act) = next variation
            Data.Text.IO.putStrLn (showPath path <> ": " <> details)
            act

