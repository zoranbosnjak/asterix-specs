
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.IO
import qualified System.Environment
import           System.IO as IO
import           System.Exit (die, exitWith, ExitCode(ExitSuccess))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Crypto.Hash

import           TH (getEnvVariableExpr)
import           Data.Asterix
import           Data.Asterix.Common
import           Data.Asterix.Validation (validate, isValid)

data Input
    = FileInput FilePath
    | StdInput

data Output
    = ValidateOnly
    | Sha1
    | OutputList
    | OutputSyntax Encoder

data Convert = Convert
    { convInput  :: (Input, Decoder)
    , convWarnings :: Bool
    , convOutput :: Output
    }

data Options
    = OptConvert Convert
    | OptPretify FilePath (Decoder, Encoder)

convertOpts :: Opt.Parser Convert
convertOpts = Convert
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

options :: Opt.Parser Options
options = fmap OptConvert convertOpts <|> optPretify
  where
    optPretify = OptPretify
        <$> (strOption ( long "pretify" <> metavar "FILENAME" <> help "Reformat file to normal form.")
            <* (flag' () ( long "remove-comments" <> help "This process removes comments.")))
        <*> pretifyOpt
      where
        pretifyOpt = foldr (<|>) empty $ do
            (shortName, description, decoder, encoder) <- availablePretifiers
            return $ flag' (decoder, encoder) (long shortName <> help ("syntax: " ++ description))

convert :: Convert -> IO ()
convert opt = do
    result <- do
        let (i, astDecoder) = convInput opt
        (s, filename) <- case i of
            FileInput f -> (,) <$> BS.readFile f <*> pure f
            StdInput -> (,) <$> BS.hGetContents IO.stdin <*> pure "<stdin>"
        return $ astDecoder filename s

    let warnings = convWarnings opt
    case result of
        Left e -> die e
        Right asterix -> case convOutput opt of
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
                case asterix of
                    AsterixBasic basic -> mapM_ (dumpItem []) (basCatalogue basic)
                    AsterixExpansion expansion -> case expVariation expansion of
                        Compound _ lst -> mapM_ (dumpItem []) (catMaybes lst)
                        _ -> fail "unexpected expansion variation"
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
                    Compound Nothing lst -> ("Compound", mapM_ (dumpItem path) (catMaybes lst))
                    Compound (Just _n) lst -> ("Compound(n)", mapM_ (dumpItem path) (catMaybes lst))
                (details, act) = next variation
            Data.Text.IO.putStrLn (showPath path <> ": " <> details)
            act

pretify :: FilePath -> Decoder -> Encoder -> IO ()
pretify filename decoder encoder = do
    s <- BS.readFile filename
    case decoder filename s of
        Left e -> die e
        Right asterix -> do
            BS.writeFile filename $ encoder asterix

main :: IO ()
main = do
    pName <- System.Environment.getProgName
    _pArgs <- System.Environment.getArgs

    -- get build environment variables
    let swVersion :: String
        swVersion = $( getEnvVariableExpr "SW_VERSION" )

        versionString :: String
        versionString = "version: " ++ swVersion

    opt <- do
        let showVersion = flag' True (long "version" <> help "Show version and exit")
            options'
                = (showVersion *> pure Nothing)
              <|> fmap Just options
        execParser (info (options' <**> helper) idm) >>= \case
            Nothing -> do
                putStrLn $ pName ++ ", " ++ versionString
                exitWith ExitSuccess
            Just opt -> return opt

    case opt of
        OptConvert convOpt -> convert convOpt
        OptPretify filename (decoder, encoder) -> pretify filename decoder encoder

