-- Names of toplevel 'explicit' items.

import           Options.Applicative as Opt
import qualified Data.Set as Set
import           Main.Utf8 (withUtf8)
import qualified Data.ByteString as BS
import           Control.Monad

import           Folds

data Options = Options
    { paths :: [FilePath]
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> some (Opt.argument str (metavar "PATH..."))

opts :: ParserInfo Options
opts = info (parseOptions <**> helper) fullDesc

focusBasic :: Getter Asterix (Maybe Basic)
focusBasic = to $ \case
    AsterixBasic bas -> Just bas
    AsterixExpansion _ -> Nothing

focusNameVar :: Getter Item (Maybe (Name, Variation))
focusNameVar = to $ \case
    Spare _n -> Nothing
    Item name _title variation _doc -> Just (name, variation)

focusExplicitName :: Fold (Name, Variation) Name
focusExplicitName = folding $ \case
    (name, Explicit _met) -> [name]
    _ -> []

focus :: Fold [Asterix] Name
focus
    = folded
    . focusBasic
    . _Just
    . folding basCatalogue
    . focusNameVar
    . _Just
    . focusExplicitName

result :: [Asterix] -> Set.Set Name
result = foldMapOf focus Set.singleton

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        loadSpec "ast" path (BS.readFile path)
    mapM_ print (result specs)
