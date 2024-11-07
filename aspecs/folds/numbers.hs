-- What kind of numbers do we have in scaling factor?

import           Control.Monad
import           Data.List           (nub, sort)
import qualified Data.Text.IO        as T
import           Main.Utf8           (withUtf8)
import           Options.Applicative as Opt

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
