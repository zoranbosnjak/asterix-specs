module Main where

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import           Data.Version (showVersion)
import           Data.Text (Text)
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Ratio
import           Numeric (showHex)

import           Asterix.Indent
import           Asterix.Specs

import           Paths_aspecs (version)

data Options = Options
    { optPath :: FilePath
    } deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> Opt.argument str
        ( metavar "PATH"
       <> help ("Input file, supported formats: " ++ show syntaxList)
        )
  where
    syntaxList = do
        (shortName, _, _) <- availableDecoders
        pure shortName

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "Asterix to Rst converter" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

loadSpec :: Monad m => FilePath -> m ByteString -> m Asterix
loadSpec path getS = do
    s <- getS
    let fmt = reverse $ fst $ break (== '.') $ reverse path
        syntax = maybe (error "syntax lookup") id $ lookup fmt syntaxes
        decoder = maybe (error "decoder") id $ syntaxDecoder syntax
        ast = either error id $ decoder path s
    pure ast

type Path = [Text]

class IsBlock a where
    mkBlock :: Path -> a -> Block Text

tShow :: Show a => a -> Text
tShow = T.pack . show

showN :: Show a => Int -> a -> Text
showN i = T.pack . reverse . take i . (\x -> x ++ repeat '0') . reverse . show

underline :: Char -> Text -> Block Text
underline ch t
    = line t
   <> line (T.replicate (T.length t) (T.singleton ch))

tPath :: Path -> Text
tPath = mconcat . intersperse "/"

numRational :: Number -> Rational
numRational = \case
    NumberZ val -> toRational val
    NumberQ val -> val
    NumberR val -> val

tSig :: Signed -> Text
tSig = \case
    Signed -> "signed"
    Unsigned -> "unsigned"

instance IsBlock Content where
    mkBlock _parent = \case
        ContentRaw -> "- raw value"
        ContentTable lst -> mconcat
            [ "- values:"
            , emptyLine
            , indent $ mconcat $ do
                (k,v) <- lst
                pure $ line $ "| " <> tShow k <> ": " <> v
            ]
        ContentString st -> case st of
            StringAscii -> "- Ascii string (8-bits per character)"
            StringICAO  -> "- ICAO string (6-bits per character)"
            StringOctal -> "- Octal string (3-bits per digit)"
        ContentInteger sig constr -> mconcat
            [ line $ "- " <> tSig sig <> " integer"
            , mconcat $ do
                co <- constr
                pure $ line $ "- value :math:`" <> showConstrain co <> "`"
            ]
        ContentQuantity sig scal frac unit constr -> mconcat
            [ line $ "- " <> tSig sig <> " quantity"
            , line $ "- scaling factor: " <> scal'
            , line $ "- fractional bits: " <> tShow frac
            , unit'
            , lsb
            , mconcat $ do
                co <- constr
                pure $ line $ "- value :math:`" <> showConstrain co <> "` " <> unit
            ]
          where
            scal' = showNumber scal
            unit' = case unit of
                "" -> mempty
                _ -> line $ "- unit: \"" <> unit <> "\""
            unit'' = case unit of
                "" -> mempty
                _ -> " " <> unit
            lsb = case frac of
                0 -> line $ "- LSB = " <> ":math:`" <> scal' <> "`" <> unit''
                _ ->
                    let b = "{2^{" <> tShow frac <> "}}"
                        lsb1 = ":math:`" <> scal' <> " / " <> b <> "` " <> unit
                        d = (2::Int) ^ frac
                        c = "{" <> tShow d <> "}"
                        lsb2 = ":math:`" <> scal' <> " / " <> c <> "` " <> unit
                        scl = numRational scal
                        approx :: Double
                        approx = fromIntegral (numerator scl) / (fromIntegral (denominator scl * fromIntegral d))
                        lsb3 = ":math:`\\approx " <> tShow approx <> "` " <> unit
                    in line $ "- LSB = " <> lsb1 <> " = " <> lsb2 <> " " <> lsb3
        ContentBds t -> case t of
            BdsWithAddress -> "- BDS register with address"
            BdsAt mAddr -> case mAddr of
                Nothing -> "- BDS register (unknown)"
                Just (BdsAddr addr) -> line $ "- BDS register " <> x
                  where
                    x = T.reverse $ T.take 2 $ T.reverse ("0" <> T.pack (showHex addr ""))

instance IsBlock Rule where
    mkBlock p = \case
        ContextFree cont -> mkBlock p cont
        Dependent otherItem rules -> mconcat
            [ line $ "* Content of this item depends on the value of item ``" <> tPath otherItem <> "``."
            , emptyLine
            , indent $ blocksLn $ do
                (a, b) <- rules
                pure $ mconcat
                    [ line $ "* In case of ``" <> tPath otherItem <> " == " <> tShow a <> "``:"
                    , indent $ mkBlock p b
                    ]
            ]

bits :: Int -> Text
bits n
    | n == 1 = "1 bit"
    | otherwise = tShow n <> " bits"

dots :: Int -> Text
dots n
    | n <= 32 = T.replicate n "."
    | otherwise = "... " <> tShow n <> " bits ..."

class Fixed a where
    bitSize :: a -> RegisterSize

instance Fixed a => Fixed [a] where
    bitSize lst = foldr (+) 0 (fmap bitSize lst)

instance Fixed Variation where
    bitSize (Element n _) = n
    bitSize (Group lst) = bitSize lst
    bitSize _ = error "non-fixed item"

instance Fixed Item where
    bitSize (Spare n) = n
    bitSize (Item _ _ var _) = bitSize var

instance IsBlock Variation where

    mkBlock p (Element n rule) = blocksLn
        [ line $ "- " <> bits n <> " [``" <> dots n <> "``]"
        , mkBlock p rule
        ]

    mkBlock p (Group lst) = blocksLn (mkBlock p <$> lst)

    mkBlock p (Extended n1 n2 lst) = mconcat
        [ line $ "Extended item with first part ``"
            <> tShow n1 <> " bits`` long and optional ``"
            <> tShow n2 <> " bits`` extends."
        , emptyLine
        , blocksLn $ do
            grp <- groups ns lst
            pure (blocksLn (fmap (mkBlock p) grp) <> emptyLine <> fx)
        ]
      where
        fx = indent $ mconcat
            [ "``(FX)``"
            , emptyLine
            , "- extension bit"
            , emptyLine
            , indent $ mconcat
                [ "| 0: End of data item"
                , "| 1: Extension into next extent"
                ]
            ]
        ns = fmap pred ([n1] <> repeat n2)
        groups :: [PrimarySize] -> [Item] -> [[Item]]
        groups _ [] = []
        groups xs items =
            let findN x
                    | bitSize (take x items) == (head xs) = x
                    | otherwise = findN $ succ x
                n = findN 1
            in (take n items) : groups (tail xs) (drop n items)

    mkBlock p (Repetitive rep var) = mconcat
        [ line $ "Repetitive item, repetition factor " <> tShow rep <> " bits."
        , emptyLine
        , indent $ mkBlock p var
        ]

    mkBlock _parent Explicit = "Explicit item"

    mkBlock p (Compound mn lst) = mconcat
        [ fspec
        , emptyLine
        , blocksLn $ do
            mItem <- lst
            pure $ case mItem of
                Nothing -> "(empty subitem)"
                Just item -> mkBlock p item
        ]
      where
        fspec = case mn of
            Nothing -> "Compound item (FX)"
            Just n -> line $ "Compound item (fspec=" <> tShow n <> " bits)"

instance IsBlock Item where
    mkBlock p = \case
        Spare n ->
            let ref = p <> ["(spare)"]
            in indent $ blocksLn
                [ line $ "**" <> tPath ref <> "**"
                , line $ "- " <> bits n <> " [``" <> dots n <> "``]"
                ]
        Item name title var doc ->
            let ref = p <> [name]
                tit
                    | title == mempty = ""
                    | otherwise = " - *" <> title <> "*"
            in indent $ blocksLn
                [ line $ "**" <> tPath ref <> "**" <> tit
                , maybe mempty remark $ docDescription doc
                , mkBlock ref var
                , case docRemark doc of
                    Nothing -> mempty
                    Just val -> indent ("remark" <> indent (remark val))
                ]
          where
            remark t = mconcat (fmap line $ T.lines t)

newtype TopItem = TopItem Item

instance IsBlock TopItem where
    mkBlock _p (TopItem (Spare _n)) = error "unexpected spare"
    mkBlock p (TopItem (Item name title var doc)) = mconcat
        [ underline '*' (tPath ref <> " - " <> title)
        , emptyLine
        , line $ "*Definition*: " <> maybe "" id (docDefinition doc)
        , "*Structure*:"
        , emptyLine
        , mkBlock ref var
        , emptyLine
        , maybe mempty remark $ docRemark doc
        ]
      where
        ref = p <> [name]
        remark t = emptyLine <> mconcat (fmap line $ T.lines t)

fmtDate :: Date -> Text
fmtDate (Date y m d) =
    "**date**: " <> tShow y <> "-" <> showN 2 m <> "-" <> showN 2 d

instance IsBlock Basic where
    mkBlock _p val = mconcat
        [ underline '=' $ "Asterix category " <> showN 3 cat <> " - " <> basTitle val
        , blocksLn
            [ line $ "**category**: " <> showN 3 cat
            , line $ "**edition**: " <> tShow (editionMajor ed) <> "." <> tShow (editionMinor ed)
            , line $ fmtDate $ basDate val
            ]
        , emptyLine
        , underline '-' "Preamble"
        , mconcat (fmap line preamble)
        , emptyLine
        , underline '-' "Description of standard data items"
        , emptyLine
        , blocksLn (mkBlock [ref] . TopItem <$> basCatalogue val)
        , emptyLine
        , underline '=' $ "User Application Profile for Category " <> showN 3 cat
        , fmtUap (basUap val)
        , emptyLine
        ]
      where
        findTitle name lst = case head lst of
            Spare _ -> findTitle name $ tail lst
            Item iName title _var _doc -> if
                | name == iName -> title
                | otherwise -> findTitle name $ tail lst
        cat = basCategory val
        ed = basEdition val
        preamble = maybe [] T.lines $ basPreamble val
        ref = "I" <> showN 3 cat
        fmtUap = \case
            Uap lst -> oneUap lst
            Uaps lsts -> mconcat
                [ "This category has multiple UAPs."
                , emptyLine
                , blocksLn $ do
                    (name, lst) <- lsts
                    pure $ mconcat
                        [ underline '-' name
                        , oneUap lst
                        ]
                ]
          where
            fx = "- ``(FX)`` - Field extension indicator"
            groups = \case
                [] -> []
                lst -> take 7 lst : groups (drop 7 lst)
            oneItem (i, mItem) = case mItem of
                Nothing -> line $ "- (" <> tShow i <> ") ``(spare)``"
                Just name -> line $ "- (" <> tShow i <> ") ``I" <> showN 3 cat <> "/"
                    <> name <> "`` - " <> findTitle name (basCatalogue val)
            oneUap lst = mconcat $ do
                let r = mod (7 - mod (length lst) 7) 7
                    lst' = zip [(1::Int)..] (lst <> replicate r Nothing)
                grp <- groups lst'
                pure $ mconcat (fmap oneItem grp) <> fx

instance IsBlock Expansion where
    mkBlock _p val = mconcat
        [ underline '=' $ "Asterix expansion " <> showN 3 cat <> " - " <> expTitle val
        , blocksLn
            [ line $ "**category**: " <> showN 3 cat
            , line $ "**edition**: " <> tShow (editionMajor ed) <> "." <> tShow (editionMinor ed)
            , line $ fmtDate $ expDate val
            ]
        , emptyLine
        , underline '-' "Description of asterix expansion"
        , mkBlock [ref] $ expVariation val
        , emptyLine
        ]
      where
        cat = expCategory val
        ed = expEdition val
        ref = "I" <> showN 3 cat

instance IsBlock Asterix where
    mkBlock p (AsterixBasic val) = mkBlock p val
    mkBlock p (AsterixExpansion val) = mkBlock p val

main :: IO ()
main = withUtf8 $ do
    opt <- execParser opts
    let path = optPath opt
    ast <- loadSpec path (BS.readFile path)
    BS.putStr $ T.encodeUtf8 $ renderBlock 4 (mkBlock mempty ast)

