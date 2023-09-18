module Main where

import           Main.Utf8 (withUtf8)
import           Options.Applicative as Opt
import           Data.Version (showVersion)
import           Data.Text (Text)
import           Numeric (showHex)
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Ratio hiding ((%))
import           Control.Monad
import           Formatting as F

import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import qualified Data.Text.Lazy as TL

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
    let astFmt = reverse $ fst $ break (== '.') $ reverse path
        syntax = maybe (error "syntax lookup") id $ lookup astFmt syntaxes
        decoder = maybe (error "decoder") id $ syntaxDecoder syntax
        ast = either error id $ decoder path s
    pure ast

type Path = [Text]

class MkBlock a where
    mkBlock :: Path -> a -> BlockM Builder ()

-- | The same as 'line $ bformat (formating) arg1 arg2 ...'
fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

underline :: Char -> Builder -> BlockM Builder ()
underline ch t = do
    let n = fromIntegral $ TL.length $ BL.toLazyText t
    line t
    line $ BL.fromText $ (T.replicate n (T.singleton ch))

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

instance MkBlock Content where
    mkBlock _parent = \case
        ContentRaw -> "- raw value"
        ContentTable lst -> do
            line "- values:"
            emptyLine
            indent $ forM_ lst $ \(k,v) -> do
                fmt ("| " % int % ": " % stext) k v
        ContentString st -> case st of
            StringAscii -> "- Ascii string (8-bits per character)"
            StringICAO  -> "- ICAO string (6-bits per character)"
            StringOctal -> "- Octal string (3-bits per digit)"
        ContentInteger sig constr -> do
            fmt ("- " % stext % " integer") (tSig sig)
            forM_ constr $ \co -> do
                fmt ("- value :math:`" % stext % "`") (showConstrain co)
        ContentQuantity sig scal frac unit constr -> do
            fmt ("- " % stext % " quantity") (tSig sig)
            fmt ("- scaling factor: " % stext) scal'
            fmt ("- fractional bits: " % int) frac
            unit'
            lsb
            forM_ constr $ \co -> do
                fmt ("- value :math:`" % stext % "` " % stext) (showConstrain co) unit
          where
            scal' = showNumber scal
            unit' = case unit of
                "" -> mempty
                _ -> fmt ("- unit: \"" % stext % "\"") unit
            unit'' = case unit of
                "" -> mempty
                _ -> " " <> unit
            lsb = case frac of
                0 -> fmt ("- LSB = :math:`" % stext % "`" % stext) scal' unit''
                _ ->
                    let b = sformat ("{2^{" % int % "}}") frac
                        lsb1 = ":math:`" <> scal' <> " / " <> b <> "` " <> unit
                        d = (2::Int) ^ frac
                        c = sformat ("{" % int % "}") d
                        lsb2 = ":math:`" <> scal' <> " / " <> c <> "` " <> unit
                        scl = numRational scal
                        approx :: Double
                        approx = fromIntegral (numerator scl) / (fromIntegral (denominator scl * fromIntegral d))
                        lsb3 = sformat (":math:`\\approx " % string % "` " % stext) (show approx) unit
                    in fmt stext ("- LSB = " <> lsb1 <> " = " <> lsb2 <> " " <> lsb3)
        ContentBds t -> case t of
            BdsWithAddress -> "- BDS register with address"
            BdsAt mAddr -> case mAddr of
                Nothing -> "- BDS register (unknown)"
                Just (BdsAddr addr) -> fmt ("- BDS register " % stext) x
                  where
                    x = T.reverse $ T.take 2 $ T.reverse ("0" <> T.pack (showHex addr ""))

instance MkBlock Rule where
    mkBlock p = \case
        ContextFree cont -> mkBlock p cont
        Dependent otherItem rules -> do
            fmt ("* Content of this item depends on the value of item ``" % stext % "``.") (tPath otherItem)
            emptyLine
            indent $ blocksLn $ do
                (a, b) <- rules
                pure $ do
                    fmt ("* In case of ``" % stext % " == " % int % "``:") (tPath otherItem) a
                    indent $ mkBlock p b

bits :: Int -> Text
bits n
    | n == 1 = "1 bit"
    | otherwise = sformat (int % " bits") n

dots :: Int -> Text
dots n
    | n <= 32 = T.replicate n "."
    | otherwise = sformat ("... " % int % " bits ...") n

instance MkBlock Variation where

    mkBlock p (Element n rule) = do
        fmt stext ("- " <> bits n <> " [``" <> dots n <> "``]")
        emptyLine
        mkBlock p rule

    mkBlock p (Group lst) = blocksLn (mkBlock p <$> lst)

    mkBlock p (Extended lst) = do
        line "Extended item."
        emptyLine
        blocksLn $ do
            mItem <- lst
            pure $ case mItem of
                Nothing -> fx
                Just item -> mkBlock p item
      where
        fx = indent $ do
            line $ "``(FX)``"
            emptyLine
            line $ "- extension bit"
            emptyLine
            indent $ mconcat
                [ "| 0: End of data item"
                , "| 1: Extension into next extent"
                ]

    mkBlock p (Repetitive rt var) = do
        case rt of
            RepetitiveRegular rep -> fmt
                ("Repetitive item, repetition factor " % int % " bits.") rep
            RepetitiveFx -> fmt "Repetitive item with FX extension"
        emptyLine
        indent $ mkBlock p var

    mkBlock _parent (Explicit mt) = case mt of
        Nothing -> "Explicit item"
        Just t -> case t of
            ReservedExpansion -> "Explicit item (RE)"
            SpecialPurpose    -> "Explicit item (SP)"

    mkBlock _parent RandomFieldSequencing = "Rfs"

    mkBlock p (Compound mn lst) = do
        fspec
        emptyLine
        blocksLn $ do
            mItem <- lst
            pure $ case mItem of
                Nothing -> "(empty subitem)"
                Just item -> mkBlock p item
      where
        fspec = case mn of
            Nothing -> "Compound item (FX)"
            Just n -> fmt ("Compound item (fspec=" % int % " bits)") n

instance MkBlock Item where
    mkBlock p = \case
        Spare n ->
            let ref = p <> ["(spare)"]
            in indent $ do
                fmt ("**" % stext % "**") (tPath ref)
                emptyLine
                fmt stext ("- " <> bits n <> " [``" <> dots n <> "``]")
        Item name title var doc ->
            let ref = p <> [name]
                tit
                    | title == mempty = ""
                    | otherwise = " - *" <> title <> "*"
            in indent $ do
                fmt stext ("**" <> tPath ref <> "**" <> tit)
                case docDescription doc of
                    Nothing -> pure ()
                    Just val -> do
                        emptyLine
                        remark val
                emptyLine
                mkBlock ref var
                case docRemark doc of
                    Nothing -> pure ()
                    Just val -> do
                        emptyLine
                        indent ("remark" <> indent (remark val))
          where
            remark t = mapM_ (fmt stext) (T.lines t)

newtype TopItem = TopItem Item

instance MkBlock TopItem where
    mkBlock _p (TopItem (Spare _n)) = error "unexpected spare"
    mkBlock p (TopItem (Item name title var doc)) = do
        underline '*' $ bformat stext (tPath ref <> " - " <> title)
        emptyLine
        fmt stext ("*Definition*: " <> maybe "" id (docDefinition doc))
        line "*Structure*:"
        emptyLine
        mkBlock ref var
        case docRemark doc of
            Nothing -> pure ()
            Just val -> do
                emptyLine
                remark val
      where
        ref = p <> [name]
        remark t = mapM_ (fmt stext) (T.lines t)

fmtDate :: Date -> Text
fmtDate (Date y m d) = sformat (int % "-" % left 2 '0' % "-" % left 2 '0') y m d

instance MkBlock Basic where
    mkBlock _p val = do
        underline '=' $ bformat ("Asterix category " % left 3 '0' % " - " % stext) cat (basTitle val)
        blocksLn
            [ fmt ("**category**: " % left 3 '0') cat
            , fmt ("**edition**: " % int % "." % int) (editionMajor ed) (editionMinor ed)
            , fmt ("**date**: " % stext) (fmtDate $ basDate val)
            ]
        emptyLine
        underline '-' "Preamble"
        forM_ preamble $ \i -> do
            fmt stext i
        emptyLine
        underline '-' "Description of standard data items"
        emptyLine
        blocksLn (mkBlock [ref] . TopItem <$> basCatalogue val)
        emptyLine
        underline '=' $ bformat ("User Application Profile for Category " % left 3 '0') cat
        fmtUap (basUap val)
      where
        findTitle name lst = case head lst of
            Spare _ -> findTitle name $ tail lst
            Item iName title _var _doc -> if
                | name == iName -> title
                | otherwise -> findTitle name $ tail lst
        cat = basCategory val
        ed = basEdition val
        preamble = maybe [] T.lines $ basPreamble val
        ref = sformat ("I" % left 3 '0') cat
        fmtUap = \case
            Uap lst -> oneUap lst
            Uaps lsts msel -> do
                line $ "This category has multiple UAPs."
                emptyLine
                case msel of
                    Nothing -> line $ "UAP selection is not defined."
                    Just sel -> do
                        fmt stext ("UAP selection is based on the value of: ``" <> tPath (selItem sel) <> "``:")
                        emptyLine
                        indent $ forM_ (selTable sel) $ \(a, b) -> do
                            fmt ("* ``" % int % "``: " % stext) a b
                emptyLine
                blocksLn $ do
                    (name, lst) <- lsts
                    pure $ do
                        underline '-' $ bformat stext name
                        oneUap lst
          where
            fx = line $ "- ``(FX)`` - Field extension indicator"
            groups = \case
                [] -> []
                lst -> take 7 lst : groups (drop 7 lst)
            oneItem (i, mItem) = case mItem of
                Nothing -> fmt ("- (" % int % ") ``(spare)``") i
                Just name -> fmt
                    ("- (" % int % ") ``I" % left 3 '0' % "/" % stext % "`` - " % stext)
                    i cat name (findTitle name (basCatalogue val))
            oneUap lst = do
                let r = mod (7 - mod (length lst) 7) 7
                    lst' = zip [(1::Int)..] (lst <> replicate r Nothing)
                forM_ (groups lst') $ \grp -> do
                    mapM_ oneItem grp
                    fx

instance MkBlock Expansion where
    mkBlock _p val = do
        underline '=' $ bformat ("Asterix expansion " % left 3 '0' % " - " % stext) cat (expTitle val)
        blocksLn
            [ fmt ("**category**: " % left 3 '0') cat
            , fmt ("**edition**: " % int % "." % int) (editionMajor ed) (editionMinor ed)
            , fmt ("**date**: " % stext) (fmtDate $ expDate val)
            ]
        emptyLine
        underline '-' "Description of asterix expansion"
        mkBlock [ref] $ expVariation val
        emptyLine
      where
        cat = expCategory val
        ed = expEdition val
        ref = sformat ("I" % left 3 '0') cat

instance MkBlock Asterix where
    mkBlock p (AsterixBasic val) = mkBlock p val
    mkBlock p (AsterixExpansion val) = mkBlock p val

main :: IO ()
main = withUtf8 $ do
    opt <- execParser opts
    let path = optPath opt
    ast <- loadSpec path (BS.readFile path)
    BS.putStr $ T.encodeUtf8 $ TL.toStrict $ BL.toLazyText $ renderBlockM 4 (mkBlock mempty ast)

