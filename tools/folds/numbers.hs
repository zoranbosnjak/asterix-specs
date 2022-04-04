{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- What kind of numbers do we have in scaling factor?

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

focus :: Fold [Asterix] Number
focus
    = folded
    . focusAsterixVariation
    . focusVariationRule
    . focusRuleContent
    . focusContentScaling
    . _Just

result :: [Asterix] -> Set.Set Number
result = foldMapOf focus Set.singleton

main :: IO ()
main = withUtf8 $ do
    cmdOptions <- execParser opts
    specs <- forM (paths cmdOptions) $ \path -> do
        loadSpec "ast" path (BS.readFile path)
    mapM_ print (result specs)

