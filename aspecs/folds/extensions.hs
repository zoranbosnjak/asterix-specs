-- Names of toplevel 'explicit' items.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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

fBasic :: Asterix -> [Basic]
fBasic = \case
    AsterixBasic bas -> [bas]
    AsterixExpansion _ -> []

fNspRuleNameVar :: NonSpare () -> [(ItemName, Rule (Variation ()))]
fNspRuleNameVar (NonSpare name _title rule _doc) = [(name, rule)]

fNameVarExplicitNameVar :: (ItemName, Rule (Variation ()))
    -> [(ItemName, Variation ())]
fNameVarExplicitNameVar (name, rule) = do
    x <- fRuleX rule
    pure (name, x)

fExplicitName :: (ItemName, Variation ()) -> [ItemName]
fExplicitName = \case
    (name, Explicit _t) -> [name]
    _ -> []

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        loadSpec "ast" path (T.readFile path)
    let result =
            specs
            >>= fBasic
            >>= basCatalogue
            >>= fNspRuleNameVar
            >>= fNameVarExplicitNameVar
            >>= fExplicitName
    forM_ (sort $ nub result) $ \(ItemName x) -> do
        print x
