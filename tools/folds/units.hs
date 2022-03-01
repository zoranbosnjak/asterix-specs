{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import qualified Data.ByteString as BS
import           Control.Monad
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set

import           Data.Version (showVersion)
import           Paths_aspecs (version)

import           Data.Asterix
import           Data.Asterix.Common

data Options = Options
    { paths :: [FilePath]
    , verbose :: Bool
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> some (argument str (metavar "PATH..."))
    <*> switch ( long "verbose")

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "Gather units" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

-- | Load spec file from disk.
loadSpec :: Monad m => String -> String -> m BS.ByteString -> m Asterix
loadSpec fmt path getS = do
    s <- getS -- BS.readFile path
    let Just syntax = lookup fmt syntaxes
        Just decoder = syntaxDecoder syntax
        Right ast = decoder path s
    return ast

class HasUnit a where
    units :: a -> Set.Set Unit

instance HasUnit a => HasUnit (Maybe a) where
    units Nothing = mempty
    units (Just val) = units val

instance HasUnit Content where
    units = \case
        ContentQuantity _Signed _Number _FractBits unit _constraints -> Set.singleton unit
        _ -> mempty

instance HasUnit Rule where
    units (ContextFree content) = units content
    units (Dependent _name lst) = mconcat [units cont | (_,cont) <- lst]

instance HasUnit Variation where
    units (Element _RegisterSize rule) = units rule
    units (Group lst) = mconcat $ fmap units lst
    units (Extended _PrimarySize _ExtensionSize lst) = mconcat $ fmap units lst
    units (Repetitive _RepetitionSize variation) = units variation
    units (Explicit) = mempty
    units (Compound _regSize lst) = mconcat $ fmap units lst

instance HasUnit Item where
    units (Spare _n) = mempty
    units (Item _name _title variation _doc) = units variation

instance HasUnit Basic where
    units val = mconcat $ fmap units (basCatalogue val)

instance HasUnit Expansion where
    units val = units (expVariation val)

instance HasUnit Asterix where
    units (AsterixBasic val) = units val
    units (AsterixExpansion val) = units val

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        spec <- loadSpec "ast" path (BS.readFile path)
        return (path, spec)

    let allUnits = mconcat $ fmap (units . snd) specs
        unitsPerFile = do
            (path, spec) <- specs
            return (path, units spec)

    forM_ (Set.toAscList allUnits) $ \unit -> case verbose cmdOptions of
        False -> do
            T.putStrLn $ "\"" <> unit <> "\""
        True -> do
            let incl :: [FilePath]
                incl = fmap fst $ filter (\(_path, lst) -> unit `elem` lst) unitsPerFile
            T.putStrLn $ "\"" <> unit <> "\" -> " <> T.pack (show incl)

