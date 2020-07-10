
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
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Crypto.Hash

import           Data.Asterix
import           Data.Asterix.Validation (validate, isValid)

data Input
    = FileInput FilePath
    | StdInput

data Output
    = ValidateOnly
    | Sha1
    | OutputList
    | OutputSyntax EncodeAsterix

data Options = Options
    { optInput  :: (Input, DecodeAsterix)
    , optWarnings :: Bool
    , optOutput :: Output
    }

options :: Opt.Parser Options
options = Options
    <$> inputOpts
    <*> includeWarnings
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

    includeWarnings = switch ( long "warnings" <> help "Include warnings on validation")

    outputOpts = (validateOnly <|> fp_sha1 <|> outList <|> (fmap OutputSyntax astEncoder))
      where
        validateOnly = flag' ValidateOnly ( long "validate" <> help "validate only" )
        fp_sha1 = flag' Sha1 ( long "sha1" <> help "show sha1 fingerprint" )
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

    let warnings = optWarnings opt
    case result of
        Left e -> die e
        Right asterix -> case optOutput opt of
            ValidateOnly -> case validate warnings asterix of
                [] -> print ("ok" :: String)
                lst -> do
                    mapM_ (Data.Text.IO.hPutStrLn stderr) lst
                    IO.hPutStrLn stderr ""
                    die "Validation error(s) present!"
            Sha1 -> do
                let sha1 :: BS.ByteString -> Digest SHA1
                    sha1 = hash
                print $ sha1 $ BS8.pack $ show asterix
                unless (isValid warnings asterix) $ do
                    die "Validation error(s) present, run 'validate' for details!"
            OutputList -> do
                mapM_ (dumpItem []) (astCatalogue asterix)
                unless (isValid warnings asterix) $ do
                    die "Validation error(s) present, run 'validate' for details!"
            OutputSyntax astEncoder -> do
                BS.putStr $ astEncoder asterix
                unless (isValid warnings asterix) $ do
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

