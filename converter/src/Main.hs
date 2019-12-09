
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Options.Applicative as Opt
import           Data.Text.IO
import           System.IO as IO
import           System.Exit (die)
import qualified Data.ByteString.Lazy as BSL
import           Text.Megaparsec hiding (State)
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.Maybe
import           Data.List

import           Data.Asterix.Parser
import           Data.Asterix.Types

type ValidationError = String

data Input
    = FileInput FilePath
    | StdInput
    deriving (Show)

data OutputFormat
    = ValidateOnly
    | OutputJson
    deriving (Show)

data Options = Options
    { optInput          :: Input
    , optOutputFormat   :: OutputFormat
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> input
    <*> outfmt
  where
    input = fileInput <|> stdInput where
        fileInput = FileInput <$> strOption
            ( long "file"
           <> short 'f'
           <> metavar "FILENAME"
           <> help "Input file" )
        stdInput = flag' StdInput
            ( long "stdin"
           <> help "Read from stdin" )
    outfmt = (validateOnly <|> json) where
        validateOnly = flag' ValidateOnly ( long "validate" <> help "validate only" )
        json = flag' OutputJson ( long "json" <> help "JSON format" )

validate :: Category -> [ValidationError]
validate category = join
    [ allToplevelDefined
    , join $ (snd . validateItem [] . topItem <$> catItems category)
    ]
  where

    allToplevelDefined :: [ValidationError]
    allToplevelDefined = join [requiredNotDefined, definedNotRequired]
      where
        required = case catUap category of
            Uap lst -> catMaybes lst
            Uaps _lst -> undefined

        defined = (topItem <$> catItems category) >>= \case
            Spare _ -> return []
            Item name _ _ _ _ -> [name]

        requiredNotDefined = do
            x <- required
            guard $ x `notElem` defined
            return $ show x ++ " required, but not defined."

        definedNotRequired = do
            x <- defined
            guard $ x `notElem` required
            return $ show x ++ " defined, but not required."

reportWhen :: Bool -> [String] -> ValidationError -> [ValidationError]
reportWhen False _ _ = []
reportWhen True path msg = [show (reverse path) ++ " -> " ++ msg]

validateItem :: [String] -> Item -> (Int, [ValidationError])
validateItem path = \case
    Spare n -> (n, reportWhen (n <= 0) ("spare":path) "size")
    Item name _ _ variation _ -> validateVariation (name:path) variation

validateVariation :: [String] -> Variation -> (Int, [ValidationError])
validateVariation path = \case
    Fixed n content -> (n, join
        [ reportWhen (n <= 0) path "size"
        , validateContent path n content
        ])
    Group lst -> checkSubitems lst
    Extended n1 n2 lst -> loop (0, join [check n1, check n2]) fxPositions lst where
        check n = reportWhen (mod n 8 /= 0) path ("extended size: " ++ show n)
        fxPositions = tail (sum <$> inits (n1:repeat n2))
        loop (n,problems) fx = \case
            [] -> (n, join
                [ problems
                , reportWhen ((mod n 8) /= 0) path "not aligned"
                , reportWhen (dupNames lst) path "duplicated names"
                ])
            (i:is) ->
                let (a,b) = (head fx, tail fx)
                    (n', problems') = validateItem path i
                in case compare (n+n'+1) a of
                    LT -> loop (n+n', problems'++problems) (a:b) is
                    EQ -> loop (n+n'+1, problems'++problems) b is
                    GT -> loop (n+n', problems'++problems++["overflow"]) b is
    Repetitive subVariation ->
        let (n, problems) = validateVariation path subVariation
        in (8+n, problems)
    Explicit -> (0, [])
    Compound lst -> checkSubitems lst
    Rfs -> (0, [])
  where
    dupNames lst =
        let getName = \case
                Spare _ -> Nothing
                Item name' _ _ _ _ -> Just name'
            names = catMaybes (fmap getName lst)
        in names /= nub names
    checkSubitems lst =
        let result = fmap (validateItem path) lst
            n = sum (fst <$> result)
        in (n, join
            [ mconcat (snd <$> result)
            , reportWhen (null lst) path "empty"
            , reportWhen ((mod n 8) /= 0) path "size reminder error"
            , reportWhen (dupNames lst) path "duplicated names"
            ])

validateContent :: [String] -> Int -> ItemContent -> [ValidationError]
validateContent path n = \case
    Table lst ->
        let keys = fst <$> lst
            size = compare (length keys) (2 ^ n)
        in join
            [ reportWhen (keys /= nub keys) path "duplicated keys"
            , reportWhen (size == GT) path "table too big"
            ]
    _ -> []

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    (s, name) <- case optInput opt of
        FileInput f -> (,) <$> Data.Text.IO.readFile f <*> pure f
        StdInput -> (,) <$> Data.Text.IO.hGetContents IO.stdin <*> pure "<stdin>"

    case parse pCategory name s of
        Left e -> die $ errorBundlePretty e
        Right val -> case optOutputFormat opt of
            ValidateOnly -> case validate val of
                [] -> print ("ok" :: String)
                lst -> do
                    mapM_ (IO.hPutStrLn stderr) lst
                    IO.hPutStrLn stderr ""
                    die "Validation error(s) present!"
            OutputJson -> do
                BSL.putStr $ JsonP.encodePretty'
                    JsonP.defConfig {JsonP.confCompare = compare} val
                case validate val of
                    [] -> return ()
                    _ -> die "Validation error(s) present, run 'validate' for details!"

