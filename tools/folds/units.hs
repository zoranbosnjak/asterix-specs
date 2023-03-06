-- Gather all units from 'Quantity' elements

import           Options.Applicative as Opt
import qualified Data.Set as Set
import           Main.Utf8 (withUtf8)
import qualified Data.ByteString as BS
import           Control.Monad
import qualified Data.Text.IO as T
import qualified Data.Text as T

import           Folds

data Options = Options
    { paths :: [FilePath]
    , verbose :: Bool
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> some (Opt.argument str (metavar "PATH..."))
    <*> switch ( long "verbose")

opts :: ParserInfo Options
opts = info (parseOptions <**> helper) fullDesc

focus :: Fold Asterix Unit
focus
    = focusAsterixVariation
    . focusVariationRule
    . focusRuleContent
    . focusContentUnit
    . _Just

units :: Asterix -> Set.Set Unit
units = foldMapOf focus Set.singleton

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

