{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Crypto.Hash
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.IORef
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy.Builder   as T
import qualified Data.Text.Lazy.IO        as TL
import           Main.Utf8                (withUtf8)
import           Options.Applicative      as Opt
import           System.Exit              (die)
import           System.IO                as IO

import           Data.Version             (showVersion)
import           Paths_aspecs             (version)

import           Asterix.Specs.Syntax
import qualified Asterix.Specs.Syntax.Ast as Sast
import           Asterix.Specs.Types
import           Asterix.Specs.Validation

syntaxes :: [(String, Coder)]
syntaxes =
    [ ("dump", dump)
    , ("ast", Sast.coder)
    ]
  where
    dump = Coder
        { cDescription = "Internal coder"
        , cDecoder = Nothing
        , cEncoder = Just (T.fromText . T.pack . show)
        }

data Input
    = FileInput FilePath
    | StdInput

hashing :: [(String, BS.ByteString -> String)]
hashing =
    [ ("md5", show . (hash :: BS.ByteString -> Digest MD5))
    , ("sha1", show . (hash :: BS.ByteString -> Digest SHA1))
    , ("sha256", show . (hash :: BS.ByteString -> Digest SHA256))
    , ("sha512", show . (hash :: BS.ByteString -> Digest SHA512))
    ]

data Command
    = Convert Input Decoder Encoder
    | Prettify (Decoder, Encoder) [FilePath]
    | Checksum Input Decoder (BS.ByteString -> String)
    | Validate Decoder [FilePath]
    {-
    | Pandoc Input Decoder -- generate pandoc native format for documentation
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
pDecoder = asum $ do
    (name, coder) <- syntaxes
    case cDecoder coder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long name <> help (cDescription coder <> " (decoding)"))

pEncoder :: Parser Encoder
pEncoder = asum $ do
    (name, coder) <- syntaxes
    case cEncoder coder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long name <> help (cDescription coder <> " (encoding)"))

pCoder :: Parser (Decoder, Encoder)
pCoder = asum $ do
    (name, coder) <- syntaxes
    case (cDecoder coder, cEncoder coder) of
        (Just f1, Just f2) -> pure $ flag' (f1, f2)
            (long name <> help (cDescription coder))
        _ -> empty

pHashing :: Parser (BS8.ByteString -> String)
pHashing = asum $ do
    (name, f) <- hashing
    pure $ flag' f (long name)

pCommand :: Parser Command
pCommand = hsubparser
    ( command "convert" (info pConvert (progDesc "Syntax format conversion"))
   <> command "prettify" (info pPrettify (progDesc "Reformat file to a normal form"))
   <> command "checksum" (info pChecksum (progDesc "Print file checksum"))
   <> command "validate" (info pValidate (progDesc "Validate input"))
    )
  where
    pConvert = Convert <$> pInput <*> pDecoder <*> pEncoder
    pPrettify = Prettify <$> pCoder <*> some (strArgument (metavar "PATH"))
    pChecksum = Checksum <$> pInput <*> pDecoder <*> pHashing
    pValidate = Validate <$> pDecoder <*> some (strArgument (metavar "PATH"))

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
        StdInput    -> (,) <$> T.hGetContents IO.stdin <*> pure "<stdin>"
    either die pure (decoder filename s)

main :: IO ()
main = withUtf8 $ execParser pOpts >>= \case
    Convert input decoder encoder -> do
        asterix <- decodeInput input decoder
        TL.putStr $ T.toLazyText $ encoder asterix
    Prettify (decoder, encoder) paths -> forM_ paths $ \path -> do
        asterix <- decodeInput (FileInput path) decoder
        TL.writeFile path $ T.toLazyText $ encoder asterix
    Checksum input decoder hsh -> do
        asterix <- decodeInput input decoder
        putStrLn $ hsh $ BS8.pack $ show asterix
    Validate decoder paths -> do
        ok <- newIORef True
        forM_ paths $ \path -> do
            asterix <- decodeInput (FileInput path) decoder
            forM_ (runErrM $ validate asterix) $ \err -> do
                writeIORef ok False
                T.hPutStrLn stderr (T.pack path <> ": " <> err)
        readIORef ok >>= \case
            True -> pure ()
            False -> die "Validation error(s) present!"
