{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Main where

import           Control.Monad
import           "cryptonite" Crypto.Hash
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as BSL
import           Data.IORef
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy.Builder   as T
import qualified Data.Text.Lazy.Encoding  as T
import           Main.Utf8                (withUtf8)
import           Options.Applicative      as Opt
import           System.Exit              (die)
import           System.IO                as IO

#ifndef NOPANDOC
import           Asterix.Specs.Pandoc     (toPandoc)
import           Text.Pandoc              (def, runIOorExplode, writeNative)
#endif

import           Data.Version             (showVersion)
import           Paths_aspecs             (version)

import           Asterix.Specs

hashing :: [(String, BS.ByteString -> String)]
hashing =
    [ ("md5", show . (hash :: BS.ByteString -> Digest MD5))
    , ("sha1", show . (hash :: BS.ByteString -> Digest SHA1))
    , ("sha256", show . (hash :: BS.ByteString -> Digest SHA256))
    , ("sha512", show . (hash :: BS.ByteString -> Digest SHA512))
    ]

data Command
    = Convert Decoder Encoder (Maybe FilePath)
    | Prettify (Decoder, Encoder) [FilePath]
    | Checksum Decoder (BS.ByteString -> String) (Maybe FilePath)
    | Validate Decoder [FilePath]
    | Pandoc Decoder (Maybe FilePath)

pDecoder :: Parser Decoder
pDecoder = asum $ do
    (name, coder) <- syntaxes
    case cDecoder coder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long ("input-" <> name) <> help (cDescription coder))

pEncoder :: Parser Encoder
pEncoder = asum $ do
    (name, coder) <- syntaxes
    case cEncoder coder of
        Nothing -> empty
        Just f -> pure $ flag' f
            (long ("output-" <> name) <> help (cDescription coder))

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
   <> command "pandoc"
      (info pPandoc (progDesc "Convert to 'pandoc' documentation format"))
    )
  where
    pConvert = Convert <$> pDecoder <*> pEncoder
        <*> optional (strArgument (metavar "PATH"))
    pPrettify = Prettify <$> pCoder
        <*> some (strArgument (metavar "PATH"))
    pChecksum = Checksum <$> pDecoder <*> pHashing
        <*> optional (strArgument (metavar "PATH"))
    pValidate = Validate <$> pDecoder
        <*> some (strArgument (metavar "PATH"))
    pPandoc = Pandoc <$> pDecoder
        <*> optional (strArgument (metavar "PATH"))

pOpts :: ParserInfo Command
pOpts = info (helper <*> versionOption <*> pCommand)
    ( fullDesc <> Opt.header "aspecs" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

decodeInput :: Maybe FilePath -> Decoder -> IO Asterix
decodeInput input decoder = do
    (s, filename) <- case input of
        Just f  -> (,) <$> T.readFile f <*> pure f
        Nothing -> (,) <$> T.hGetContents IO.stdin <*> pure "<stdin>"
    either die pure (decoder filename s)

main :: IO ()
main = withUtf8 $ execParser pOpts >>= \case
    Convert decoder encoder input -> do
        asterix <- decodeInput input decoder
        BSL.putStr $ T.encodeUtf8 $ T.toLazyText $ encoder asterix
    Prettify (decoder, encoder) paths -> forM_ paths $ \path -> do
        asterix <- decodeInput (Just path) decoder
        BSL.writeFile path $ T.encodeUtf8 $ T.toLazyText $ encoder asterix
    Checksum decoder hsh input -> do
        asterix <- decodeInput input decoder
        putStrLn $ hsh $ BS8.pack $ show asterix
    Validate decoder paths -> do
        ok <- newIORef True
        forM_ paths $ \path -> do
            asterix <- decodeInput (Just path) decoder
            forM_ (runErrM $ validate asterix) $ \err -> do
                writeIORef ok False
                T.hPutStrLn stderr (T.pack path <> ": " <> err)
        readIORef ok >>= \case
            True -> pure ()
            False -> die "Validation error(s) present!"
#ifndef NOPANDOC
    Pandoc decoder input -> do
        asterix <- decodeInput input decoder
        t <- runIOorExplode $ writeNative def $ toPandoc asterix
        T.putStrLn t
#else
    Pandoc _decoder _input -> do
        error "Pandoc is not supported in this build instance."
#endif

