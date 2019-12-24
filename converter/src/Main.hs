
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Options.Applicative as Opt
import           Data.Text.Encoding (decodeUtf8)
import           System.IO as IO
import           System.Exit (die)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Text.Megaparsec hiding (State)
import qualified Data.Aeson.Encode.Pretty as JsonP
import           Data.Maybe
import           Data.List

import           Data.Asterix
import           Data.Asterix.Parser
import           Data.Asterix.Types

type ValidationError = String

data Input
    = FileInput FilePath
    | StdInput
    deriving (Show)

data OutputFormat
    = ValidateOnly
    | OutputList
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
    outfmt = (validateOnly <|> outList <|> json) where
        validateOnly = flag' ValidateOnly ( long "validate" <> help "validate only" )
        outList = flag' OutputList ( long "list" <> help "list output" )
        json = flag' OutputJson ( long "json" <> help "JSON format" )

fixedItemSize :: Category -> [ItemName] -> Maybe RegisterSize
fixedItemSize category path = findItemByName category path >>= \case
    Spare _ -> Nothing
    Item _ _ _ variation _ -> case variation of
        Fixed size _ -> Just size
        _ -> Nothing

validate :: Category -> [ValidationError]
validate category = join
    [ allToplevelDefined
    , join (validateEncoding <$> catItems category)
    , join $ (snd . validateItem category [] . topItem <$> catItems category)
    , validateUap
    ]
  where

    validateEncoding :: Toplevel -> [ValidationError]
    validateEncoding toplevel = case topEncoding toplevel of
        ContextFree encoding -> reportWhen (encoding == Absent) [topName] "Topitem can not be 'absent'"
        Dependent someItem rules -> case fixedItemSize category someItem of
            Nothing -> reportWhen True [topName] (showPath someItem ++ " not defined")
            Just m ->
                let size = compare (length rules) (2 ^ m)
                in reportWhen (size == GT) [topName] "too many encoding variations"
      where
        topName = case topItem toplevel of
            Spare _ -> ""
            Item name _ _ _ _ -> name

    validateUap :: [ValidationError]
    validateUap = case catUap category of
        Uap lst -> validateList ["uap"] lst
        Uaps lst -> join
            [ reportWhen (length lst /= 2) ["uap"] "expecting 2 uap variations"
            , do
                let dupNames = nub x /= x where x = fst <$> lst
                reportWhen dupNames ["uap"] "duplicated names"
            , do
                (uapName, lst') <- lst
                validateList [uapName] lst'
            ]
      where
        validateList path lst =
            let x = catMaybes lst
            in reportWhen (nub x /= x) path "duplicated items"

    allToplevelDefined :: [ValidationError]
    allToplevelDefined = join [requiredNotDefined, definedNotRequired, noDups]
      where
        required = catMaybes $ case catUap category of
            Uap lst -> lst
            Uaps lst -> nub $ join $ fmap snd lst

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

        noDups =
            let dups = defined \\ (nub defined)
            in reportWhen (not $ null dups) ["topitems"] ("duplicate names: " ++ show dups)

showPath :: [ItemName] -> String
showPath = intercalate "/"

reportWhen :: Bool -> [ItemName] -> ValidationError -> [ValidationError]
reportWhen False _ _ = []
reportWhen True path msg = [showPath path ++ " -> " ++ msg]

validateItem :: Category -> [ItemName] -> Item -> (Int, [ValidationError])
validateItem category path = \case
    Spare n -> (n, reportWhen (n <= 0) (path++["spare"]) "size")
    Item name _ _ variation _ -> validateVariation category (path++[name]) variation

validateVariation :: Category -> [ItemName] -> Variation -> (Int, [ValidationError])
validateVariation category path = \case
    Fixed n content -> (n, join
        [ reportWhen (n <= 0) path "size"
        , validateContent category path n content
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
                    (n', problems') = validateItem category path i
                in case compare (n+n'+1) a of
                    LT -> loop (n+n', problems'++problems) (a:b) is
                    EQ -> loop (n+n'+1, problems'++problems) b is
                    GT -> loop (n+n', problems'++problems++["overflow"]) b is
    Repetitive subVariation ->
        let (n, problems) = validateVariation category path subVariation
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
        let result = fmap (validateItem category path) lst
            n = sum (fst <$> result)
        in (n, join
            [ mconcat (snd <$> result)
            , reportWhen (null lst) path "empty"
            , reportWhen ((mod n 8) /= 0) path "size reminder error"
            , reportWhen (dupNames lst) path "duplicated names"
            ])

validateContent :: Category -> [ItemName] -> Int -> Rule Content -> [ValidationError]
validateContent category path n = \case
    ContextFree rule -> validateRule rule
    Dependent someItem rules -> join
        [ case fixedItemSize category someItem of
            Nothing -> reportWhen True path (showPath someItem ++ " not defined")
            Just m ->
                let size = compare (length rules) (2 ^ m)
                in reportWhen (size == GT) path "too many variations"
        , do
            let keys = fst <$> rules
            reportWhen (keys /= nub keys) path "duplicated keys"
        , join (validateRule . snd <$> rules )
        ]
  where
    validateRule = \case
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
    (s, filename) <- case optInput opt of
        FileInput f -> (,) <$> BS.readFile f <*> pure f
        StdInput -> (,) <$> BS.hGetContents IO.stdin <*> pure "<stdin>"

    case parse pCategory filename (decodeUtf8 s) of
        Left e -> die $ errorBundlePretty e
        Right category -> case optOutputFormat opt of
            ValidateOnly -> case validate category of
                [] -> print ("ok" :: String)
                lst -> do
                    mapM_ (IO.hPutStrLn stderr) lst
                    IO.hPutStrLn stderr ""
                    die "Validation error(s) present!"
            OutputList -> do
                mapM_ (dumpItem []) (topItem <$> catItems category)
                case validate category of
                    [] -> return ()
                    _ -> die "Validation error(s) present, run 'validate' for details!"
            OutputJson -> do
                BSL.putStr $ JsonP.encodePretty'
                    JsonP.defConfig {JsonP.confCompare = compare} category
                case validate category of
                    [] -> return ()
                    _ -> die "Validation error(s) present, run 'validate' for details!"

  where
    dumpItem parent = \case
        Spare _ -> return ()
        Item name _title _dsc variation _remark -> do
            let path = parent ++ [name]
                next = \case
                    Fixed size _ -> ("Fixed " ++ show size, return ())
                    Group lst -> ("Group", mapM_ (dumpItem path) lst)
                    Extended _ _ lst -> ("Extended", mapM_ (dumpItem path) lst)
                    Repetitive var -> ("Repetitive", snd $ next var)
                    Explicit -> ("Explicit", return ())
                    Compound lst -> ("Compound", mapM_ (dumpItem path) lst)
                    Rfs -> ("Rfs", return ())
                (details, act) = next variation
            IO.putStrLn $ showPath path ++ ": " ++ details
            act

