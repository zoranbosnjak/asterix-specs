-- Find situations where 'group' is nested inside another 'group'

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.Text.IO        as T
import           Main.Utf8           (withUtf8)
import           Options.Applicative as Opt

import           Folds

newtype Options = Options
    { paths :: [FilePath]
    } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> some (Opt.argument str (metavar "PATH..."))

opts :: ParserInfo Options
opts = info (parseOptions <**> helper) fullDesc

fGroup :: Variation o -> [Item o]
fGroup = \case
    Group _ lst -> lst
    _ -> []

identify :: Asterix -> (String, CatNum, Edition)
identify = \case
    AsterixBasic val -> ("Basic", basCategory val, basEdition val)
    AsterixExpansion val -> ("Expansion", expCategory val, expEdition val)

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        loadSpec "ast" path (T.readFile path)

    let result = specs >>= augment identify
            ( fAstNsp
            >=> fNspRuleVar
            >=> fRuleX
            >=> fGroup
            >=> fItemNsp
            >=> fNspRuleVar
            >=> fRuleX
            >=> fGroup
            )

    mapM_ print (fmap fst result)

