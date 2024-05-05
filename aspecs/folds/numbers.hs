-- What kind of numbers do we have in scaling factor?

import           Options.Applicative as Opt
import           Data.List (sort, nub)
import           Main.Utf8 (withUtf8)
import           Control.Monad
import qualified Data.Text.IO as T

import           Folds

data Options = Options
    { paths :: [FilePath]
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> some (Opt.argument str (metavar "PATH..."))

opts :: ParserInfo Options
opts = info (parseOptions <**> helper) fullDesc

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        loadSpec "ast" path (T.readFile path)

    let result = specs
            >>= fAstNsp
            >>= fNspRuleVar
            >>= fRuleX
            >>= fVarRuleContent
            >>= fRuleX
            >>= fContentLsb

    forM_ (sort $ nub result) $ \x -> do
        print x
