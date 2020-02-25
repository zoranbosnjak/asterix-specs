
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.IO
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

type ValidationError = T.Text

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

fixedSubitemSize :: Asterix -> [Name] -> Maybe RegisterSize
fixedSubitemSize asterix path = findSubitemByName asterix path >>= \case
    Spare _ -> Nothing
    Subitem _ _ _ element _ -> case element of
        Fixed size _ -> Just size
        _ -> Nothing

showPath :: [Name] -> T.Text
showPath = T.intercalate "/"

reportWhen :: Bool -> [Name] -> ValidationError -> [ValidationError]
reportWhen False _ _ = []
reportWhen True path msg = [mconcat [showPath path, " -> ", msg]]

validate :: Asterix -> [ValidationError]
validate asterix = join
    [ allItemsDefined
    , join (validateEncoding <$> astCatalogue asterix)
    , join $ (snd . validateSubitem asterix [] . itemSubitem <$> astCatalogue asterix)
    , validateUap
    ]
  where

    validateEncoding :: Item -> [ValidationError]
    validateEncoding item = case itemEncoding item of
        Unspecified -> []
        ContextFree encoding -> reportWhen (encoding == Absent) [itemName] "Item can not be 'absent'"
        Dependent someSubitem rules -> case fixedSubitemSize asterix someSubitem of
            Nothing -> reportWhen True [itemName] (showPath someSubitem <> " not defined")
            Just m ->
                let size = compare (length rules) (2 ^ m)
                in reportWhen (size == GT) [itemName] "too many encoding variations"
      where
        itemName = case itemSubitem item of
            Spare _ -> ""
            Subitem name _ _ _ _ -> name

    validateUap :: [ValidationError]
    validateUap = case astUap asterix of
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

    allItemsDefined :: [ValidationError]
    allItemsDefined = join [requiredNotDefined, definedNotRequired, noDups]
      where
        required :: [Name]
        required = catMaybes $ case astUap asterix of
            Uap lst -> lst
            Uaps lst -> nub $ join $ fmap snd lst

        defined :: [Name]
        defined = (itemSubitem <$> astCatalogue asterix) >>= \case
            Spare _ -> []
            Subitem name _ _ _ _ -> [name]

        requiredNotDefined :: [ValidationError]
        requiredNotDefined = do
            x <- required
            guard $ x `notElem` defined
            return (T.pack (show x) <> " required, but not defined.")

        definedNotRequired :: [ValidationError]
        definedNotRequired = do
            x <- defined
            guard $ x `notElem` required
            return (T.pack (show x) <> " defined, but not required.")

        noDups :: [ValidationError]
        noDups =
            let dups = defined \\ (nub defined)
            in reportWhen (not $ null dups) ["items"]
                ("duplicate names: " <> T.pack (show dups))

validateSubitem :: Asterix -> [Name] -> Subitem -> (Int, [ValidationError])
validateSubitem asterix path = \case
    Spare n -> (n, reportWhen (n <= 0) (path++["spare"]) "size")
    Subitem name _ _ element _ -> validateElement asterix (path++[name]) element

validateElement :: Asterix -> [Name] -> Element -> (Int, [ValidationError])
validateElement asterix path = \case
    Fixed n content -> (n, join
        [ reportWhen (n <= 0) path "size"
        , validateContent asterix path n content
        ])
    Group lst -> checkSubitems lst
    Extended n1 n2 lst -> loop (0, join [check n1, check n2]) fxPositions lst where
        check n = reportWhen (mod n 8 /= 0) path ("extended size: " <> T.pack (show n))
        fxPositions = tail (sum <$> inits (n1:repeat n2))
        loop (n,problems) fx = \case
            [] -> (n, join
                [ problems
                , reportWhen ((mod n 8) /= 0) path "not aligned"
                , reportWhen (dupNames lst) path "duplicated names"
                ])
            (i:is) ->
                let (a,b) = (head fx, tail fx)
                    (n', problems') = validateSubitem asterix path i
                in case compare (n+n'+1) a of
                    LT -> loop (n+n', problems'++problems) (a:b) is
                    EQ -> loop (n+n'+1, problems'++problems) b is
                    GT -> loop (n+n', problems' ++ problems ++ reportWhen True path "overflow") b is
    Repetitive rep subElement ->
        let (n, problems) = validateElement asterix path subElement
        in (8*rep+n, problems)
    Explicit -> (0, [])
    Compound lst -> checkSubitems lst
    Rfs -> (0, [])
  where
    dupNames lst =
        let getName = \case
                Spare _ -> Nothing
                Subitem name' _ _ _ _ -> Just name'
            names = catMaybes (fmap getName lst)
        in names /= nub names
    checkSubitems lst =
        let result = fmap (validateSubitem asterix path) lst
            n = sum (fst <$> result)
        in (n, join
            [ mconcat (snd <$> result)
            , reportWhen (null lst) path "empty"
            , reportWhen ((mod n 8) /= 0) path "size reminder error"
            , reportWhen (dupNames lst) path "duplicated names"
            ])

validateContent :: Asterix -> [Name] -> Int -> Rule Content -> [ValidationError]
validateContent asterix path n = \case
    Unspecified -> []
    ContextFree rule -> validateRule rule
    Dependent someSubitem rules -> join
        [ case fixedSubitemSize asterix someSubitem of
            Nothing -> reportWhen True path (showPath someSubitem <> " not defined")
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
        ContentTable lst ->
            let keys = fst <$> lst
                values = snd <$> lst
                size = compare (length keys) (2 ^ n)
            in join
                [ reportWhen (keys /= nub keys) path "duplicated keys"
                , reportWhen (size == GT) path "table too big"
                , reportWhen (any T.null values) path "empty value"
                ]
        _ -> []

main :: IO ()
main = do
    opt <- execParser (info (options <**> helper) idm)
    (s, filename) <- case optInput opt of
        FileInput f -> (,) <$> BS.readFile f <*> pure f
        StdInput -> (,) <$> BS.hGetContents IO.stdin <*> pure "<stdin>"

    case parse pAsterix filename (decodeUtf8 s) of
        Left e -> die $ errorBundlePretty e
        Right asterix -> case optOutputFormat opt of
            ValidateOnly -> case validate asterix of
                [] -> print ("ok" :: String)
                lst -> do
                    mapM_ (Data.Text.IO.hPutStrLn stderr) lst
                    IO.hPutStrLn stderr ""
                    die "Validation error(s) present!"
            OutputList -> do
                mapM_ (dumpItem []) (itemSubitem <$> astCatalogue asterix)
                case validate asterix of
                    [] -> return ()
                    _ -> die "Validation error(s) present, run 'validate' for details!"
            OutputJson -> do
                BSL.putStr $ JsonP.encodePretty'
                    JsonP.defConfig {JsonP.confCompare = compare} asterix
                case validate asterix of
                    [] -> return ()
                    _ -> die "Validation error(s) present, run 'validate' for details!"

  where
    dumpItem parent = \case
        Spare _ -> return ()
        Subitem name _title _dsc element _remark -> do
            let path = parent ++ [name]
                next = \case
                    Fixed size _ -> ("Fixed " <> T.pack (show size), return ())
                    Group lst -> ("Group", mapM_ (dumpItem path) lst)
                    Extended _ _ lst -> ("Extended", mapM_ (dumpItem path) lst)
                    Repetitive _ var -> ("Repetitive", snd $ next var)
                    Explicit -> ("Explicit", return ())
                    Compound lst -> ("Compound", mapM_ (dumpItem path) lst)
                    Rfs -> ("Rfs", return ())
                (details, act) = next element
            Data.Text.IO.putStrLn (showPath path <> ": " <> details)
            act

