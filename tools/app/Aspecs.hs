module Main where

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.IO
import           System.IO as IO
import           System.Exit (die)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Data.Version (showVersion)
import           Crypto.Hash

import           Asterix.Specs
import           Asterix.Specs.Validation (validate)

import           Paths_aspecs (version)

data Input
    = FileInput FilePath
    | StdInput
    deriving (Eq, Show)

data Command
    = Validate Input Decoder Bool
    | Convert Input Decoder Encoder
    | Prettify FilePath (Decoder, Encoder)
    | List Input Decoder
    | Checksum Input Decoder

inputOpts :: Parser Input
inputOpts = fileInput <|> stdInput where
    fileInput = FileInput <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILENAME"
       <> help "Input file"
        )
    stdInput = flag' StdInput (long "stdin" <> help "Read from stdin")

decoderOpt :: Parser Decoder
decoderOpt = foldr (<|>) empty $ do
    (shortName, description, f) <- availableDecoders
    return $ flag' f (long shortName <> help ("input syntax: " ++ description))

encoderOpt :: Parser Encoder
encoderOpt = foldr (<|>) empty $ do
    (shortName, description, f) <- availableEncoders
    return $ flag' f (long shortName <> help ("output syntax: " ++ description))

prettifyOpt :: Parser (Decoder, Encoder)
prettifyOpt = foldr (<|>) empty $ do
    (shortName, description, decoder, encoder) <- availablePrettifiers
    return $ flag' (decoder, encoder)
        (long shortName <> help ("syntax: " ++ description))

validateOptions :: Parser Command
validateOptions = Validate
    <$> inputOpts
    <*> decoderOpt
    <*> switch (long "warnings" <> help "Include warnings on validation")

convertOptions :: Parser Command
convertOptions = Convert
    <$> inputOpts
    <*> decoderOpt
    <*> encoderOpt

prettifyOptions :: Parser Command
prettifyOptions = Prettify <$> targetFile <*> prettifyOpt where
    targetFile = strOption (long "remove-comments" <> metavar "FILENAME")

listOptions :: Parser Command
listOptions = List <$> inputOpts <*> decoderOpt

checksumOptions :: Parser Command
checksumOptions = Checksum <$> inputOpts <*> decoderOpt

optCommand :: Parser Command
optCommand = hsubparser
    ( command "validate" (info validateOptions (progDesc "Validate input"))
   <> command "convert" (info convertOptions (progDesc "Format conversion"))
   <> command "prettify" (info prettifyOptions (progDesc "Reformat file to normal form"))
   <> command "list" (info listOptions (progDesc "List items"))
   <> command "checksum" (info checksumOptions (progDesc "Print file checksum"))
    )

opts :: ParserInfo Command
opts = info (helper <*> versionOption <*> optCommand)
    ( fullDesc <> Opt.header "Asterix specs tools" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

decodeInput :: Input -> (FilePath -> BS8.ByteString -> Either String b) -> IO b
decodeInput input decoder = do
    (s, filename) <- case input of
        FileInput f -> (,) <$> BS.readFile f <*> pure f
        StdInput -> (,) <$> BS.hGetContents IO.stdin <*> pure "<stdin>"
    case decoder filename s of
        Left e -> die e
        Right val -> return val

main :: IO ()
main = withUtf8 $ execParser opts >>= \case

    Validate input decoder warnings -> do
        asterix <- decodeInput input decoder
        case validate warnings asterix of
            [] -> print ("ok" :: String)
            lst -> do
                mapM_ (Data.Text.IO.hPutStrLn stderr) lst
                IO.hPutStrLn stderr ""
                die "Validation error(s) present!"

    Convert input decoder encoder -> do
        asterix <- decodeInput input decoder
        BS.putStr $ encoder asterix

    Prettify filename (decoder, encoder) -> do
        asterix <- decodeInput (FileInput filename) decoder
        BS.writeFile filename $ encoder asterix

    List input decoder -> do
        let dumpItem parent = \case
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

        asterix <- decodeInput input decoder
        case asterix of
            AsterixBasic basic -> mapM_ (dumpItem []) (basCatalogue basic)
            AsterixExpansion expansion -> case expVariation expansion of
                Compound _ lst -> mapM_ (dumpItem []) (catMaybes lst)
                _ -> fail "unexpected expansion variation"

    Checksum input decoder -> do
        asterix <- decodeInput input decoder
        let sha1 :: BS.ByteString -> Digest SHA1
            sha1 = hash
        print $ sha1 $ BS8.pack $ show asterix

