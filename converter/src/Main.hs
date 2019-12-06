
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
validate category = concat
    [ allToplevelDefined    -- Defined items and UAP items must match.
    , itemValid (topItem <$> catItems category) -- Check alignments, sizes...
    ]
  where
    allToplevelDefined =
        let required = case catUap category of
                Uap lst -> catMaybes lst
                Uaps _lst -> undefined
            defined = (topItem <$> catItems category) >>= \case
                Spare _ -> return []
                Item name _ _ _ _ -> [name]

            problemsNotDefined = do
                x <- required
                guard $ x `notElem` defined
                return $ show x ++ " required, but not defined."

            problemsNotRequired = do
                x <- defined
                guard $ x `notElem` required
                return $ show x ++ " defined, but not required."

        in problemsNotDefined ++ problemsNotRequired

    itemValid [] = []
    itemValid (x:xs) = snd (checkItem [] x) ++ itemValid xs
      where
        reportWhen False _ _ = []
        reportWhen True path msg = [show (reverse path) ++ " -> " ++ msg]

        checkItem path (Spare n) = (n, reportWhen (n <= 0) ("spare":path) "size")
        checkItem path (Item name _ _ iType _) = checkType iType
          where
            checkType t =
                let checkSubitems lst =
                        let result = fmap (checkItem (name:path)) lst
                            n = sum (fst <$> result)
                        in (n, join
                            [ mconcat (snd <$> result)
                            , reportWhen (null lst) (name:path) "empty"
                            , reportWhen ((mod n 8) /= 0) (name:path) "size reminder error"
                            ])
                in case t of
                    Fixed n content -> (n, join
                        [ reportWhen (n <= 0) (name:path) "size"
                        , case content of
                            Table lst ->
                                let keys = fst <$> lst
                                    size = compare (length keys) (2 ^ n)
                                in join
                                    [ reportWhen (keys /= nub keys) (name:path) "duplicated keys"
                                    , reportWhen (size == GT) (name:path) "table too big"
                                    ]
                            _ -> []
                        ])
                    Group lst -> checkSubitems lst
                    Extended n1 n2 lst -> loop (0, join [check n1, check n2]) fxPositions lst where
                        check n = reportWhen (mod n 8 /= 0) (name:path) ("extended size: " ++ show n)
                        fxPositions = tail (sum <$> inits (n1:repeat n2))
                        loop (n,problems) fx = \case
                            [] -> (n, join
                                [ problems
                                , reportWhen ((mod n 8) /= 0) (name:path) "not aligned"
                                ])
                            (i:is) ->
                                let (a,b) = (head fx, tail fx)
                                    (n', problems') = checkItem (name:path) i
                                in case compare (n+n'+1) a of
                                    LT -> loop (n+n', problems'++problems) (a:b) is
                                    EQ -> loop (n+n'+1, problems'++problems) b is
                                    GT -> loop (n+n', problems'++problems++["overflow"]) b is
                    Repetitive subType ->
                        let (n, problems) = checkType subType
                        in (8+n, problems)
                    Explicit -> (0, [])
                    Compound lst -> checkSubitems lst
                    Rfs -> (0, [])

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

