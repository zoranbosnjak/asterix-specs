-- '.ast' syntax implementation

module Asterix.Specs.Syntax.Ast (syntax) where

import           Control.Monad
import           Data.Void
import           Data.Bool
import           Data.Bifunctor (first)
import qualified Data.Ratio
import           Numeric
import           Formatting as F
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

import           Asterix.Specs.Types
import           Asterix.Specs.Common
import           Asterix.Indent

-- | Dump to Text

class MkBlock a where
    mkBlock :: a -> BlockM Builder ()

-- | The same as 'line $ bformat (formating) arg1 arg2 ...'
fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

instance MkBlock Content where
    mkBlock = \case
        ContentRaw -> "raw"
        ContentTable lst -> do
            line "table"
            indent $ forM_ lst $ \(key, value) -> do
                fmt (int % ": " % stext) key value
        ContentString st -> line $ "string " <> case st of
            StringAscii -> "ascii"
            StringICAO -> "icao"
            StringOctal -> "octal"
        ContentInteger signed constraints -> do
            let sig = toLower <$> show signed
                cst = case constraints of
                    [] -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            fmt (F.string % " integer" % stext) sig cst
        ContentQuantity signed scaling fract unit constraints -> do
            let sig = toLower <$> show signed
                cst = case constraints of
                    [] -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            fmt
                (F.string % " quantity " % stext % " " % int % " " % "\"" % stext % "\"" % stext )
                sig (showNumber scaling) fract unit cst
        ContentBds bt -> case bt of
            BdsWithAddress -> "bds"
            BdsAt mAddr -> case mAddr of
                Nothing -> "bds ?"
                Just (BdsAddr addr) -> fmt ("bds " % hex) addr

instance MkBlock Variation where
    mkBlock = \case
        Element n rule -> do
            fmt ("element " % int) n
            indent $ case rule of
                ContextFree cont -> mkBlock cont
                Dependent name lst -> do
                    fmt ("case " % stext) (showPath name)
                    indent $ forM_ lst $ \(x,a) -> do
                        fmt (int % ":") x
                        indent $ mkBlock a

        Group lst -> do
            line "group"
            indent $ mapM_ mkBlock lst

        Extended et n1 n2 lst -> do
            fmt ("extended " % F.string % int % " " % int) fx n1 n2
            indent $ mapM_ mkBlock lst
          where
            fx = case et of
                ExtendedRegular -> ""
                ExtendedNoTrailingFx -> "no-trailing-fx "

        Repetitive rep variation -> do
            case rep of
                RepetitiveRegular n -> fmt ("repetitive " % int) n
                RepetitiveFx -> fmt "repetitive fx"
            indent $ mkBlock variation

        Explicit -> "explicit"

        Compound mFspecSize lst -> do
            case mFspecSize of
                Nothing -> "compound"
                Just n -> fmt ("compound " % int) n
            indent $ forM_ lst $ \case
                Nothing -> "-"
                Just item -> mkBlock item

instance MkBlock Item where
    mkBlock = \case
        Spare n -> fmt ("spare " % int) n
        Item name title variation doc -> do
            fmt (stext % " \"" % stext % "\"") name title
            indent $ dumpText "definition" (docDefinition doc)
            indent $ dumpText "description" (docDescription doc)
            indent $ do
                mkBlock variation
                dumpText "remark" (docRemark doc)
      where
        dumpText title = \case
            Nothing -> mempty
            Just t -> do
                line title
                indent $ forM_ (T.lines t) $ \i -> do
                    fmt stext i

instance MkBlock Uap where
    mkBlock = \case
        Uap lst -> do
            line "uap"
            indent $ dumpList lst
        Uaps variations msel -> do
            line $ "uaps"
            indent $ do
                line "variations"
                forM_ variations $ \(name, lst) -> do
                    indent $ do
                        fmt stext name
                        indent $ dumpList lst
                case msel of
                    Nothing -> mempty
                    Just sel -> do
                        fmt ("case " % stext) (showPath $ selItem sel)
                        indent $ forM_ (selTable sel) $ \(x, uapName) -> do
                            fmt (int % ": " % stext) x uapName
      where
        dumpList lst = forM_ lst $ \case
            Nothing -> line "-"
            Just item -> fmt stext item

instance MkBlock Basic where
    mkBlock basic = do
        fmt ("asterix " % left 3 '0' % " \"" % stext % "\"") cat title
        fmt ("edition " % int % "." % int) ed1 ed2
        fmt ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
        case basPreamble basic of
            Nothing -> mempty
            Just preamble -> do
                line "preamble"
                indent $ forM_ (T.lines preamble) $ \i -> do
                    fmt stext i
        emptyLine
        line "items"
        emptyLine
        indent $ blocksLn (mkBlock <$> basCatalogue basic)
        emptyLine
        mkBlock $ basUap basic
      where
        cat = basCategory basic
        title = basTitle basic
        edition = basEdition basic
        ed1 = editionMajor edition
        ed2 = editionMinor edition
        (Date year month day) = basDate basic

instance MkBlock Expansion where
    mkBlock x = do
        fmt ("ref " % left 3 '0' % " \"" % stext % "\"") cat title
        fmt ("edition " % int % "." % int) ed1 ed2
        fmt ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
        emptyLine
        mkBlock $ expVariation x
      where
        cat = expCategory x
        title = expTitle x
        edition = expEdition x
        ed1 = editionMajor edition
        ed2 = editionMinor edition
        (Date year month day) = expDate x

instance MkBlock Asterix where
    mkBlock = \case
        AsterixBasic basic -> mkBlock basic
        AsterixExpansion expansion -> mkBlock expansion

-- | Parse from Text

type Parser = Parsec Void Text

-- | Space consumer helper function.
smartSkip :: Parser () -> Parser ()
smartSkip skipSpace = L.space
    skipSpace
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

-- | Skip whitespace with newline.
scn :: Parser ()
scn = smartSkip space1

-- | Skip whitespace without newline.
sc :: Parser ()
sc = smartSkip (void $ some (MC.char ' ' <|> MC.char '\t'))

stringLiteral :: Parser String
stringLiteral = MC.char '"' >> manyTill L.charLiteral (MC.char '"')

pInt :: Parser Int
pInt = L.decimal -- TODO: add support for [INT, CHR, HEX, OCT]

-- | Parse with first successfull parser.
tryOne :: [Parser a] -> Parser a
tryOne [] = fail "empty list"
tryOne [x] = x
tryOne (x:xs) = try x <|> tryOne xs

-- | Parse 'asterix category'.
pCat :: Text -> Parser Int
pCat prefix = do
    MC.string prefix >> sc
    (a,b,c) <- (,,) <$> digitChar <*> digitChar <*> digitChar
    return (read [a,b,c])

-- | Parse 'edition'.
pEdition :: Parser Edition
pEdition = do
    MC.string "edition" >> sc
    (a,_,b) <- (,,) <$> L.decimal <*> MC.char '.' <*> L.decimal
    return $ Edition a b

-- | Parse 'date'.
pDate :: Parser Date
pDate = do
    MC.string "date" >> sc
    Date <$> L.decimal <*> (MC.char '-' *> L.decimal ) <*> (MC.char '-' *> L.decimal )

-- | Parse to the end of line.
pLine :: Parser String
pLine = many printChar <* newline

skipEmptyLines :: Parser ()
skipEmptyLines = do
    peek <- lookAhead pLine
    case null peek of
        True -> pLine >> skipEmptyLines
        False -> return ()

-- | Parse text block after a header 'hdr'.
blockAfter :: Text -> Parser String
blockAfter hdr = do
    i <- L.indentLevel
    void $ MC.string hdr >> many (MC.char ' ') >> newline
    skipEmptyLines
    unlines . leftIndent . dropEmpty <$> consumeUntilIndent i
  where
    getIndent :: String -> Pos
    getIndent = mkPos . succ . length . takeWhile (== ' ')

    consumeUntilIndent :: Pos -> Parser [String]
    consumeUntilIndent i = atEnd >>= \case
        True -> return []
        False -> do
            peek <- lookAhead pLine
            case null peek || getIndent peek > i of
                False -> return []
                True -> (:) <$> pLine <*> consumeUntilIndent i

    -- drop empty lines at the end of a list
    dropEmpty :: [String] -> [String]
    dropEmpty [] = []
    dropEmpty lst
        | null (last lst) = dropEmpty (init lst)
        | otherwise = lst

    leftIndent lst = fmap (drop n) lst where
        n = pred $ unPos i
        i = minimum $ fmap getIndent $ filter (not . null) lst

-- | Parse 'preamble'.
pPreamble :: Parser Text
pPreamble = T.pack <$> blockAfter "preamble"

-- | Parse a list of chunks (all chunks have the same initial indent level).
parseList :: Parser h -> (Parser () -> Parser a) -> Parser (h, [a])
parseList parseHeader parseChunk = do
    i0 <- L.indentLevel
    hdr <- parseHeader
    i1 <- lookAhead (scn >> L.indentLevel)
    when (i1 <= i0) $ fail "indent required"
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i1) || end)
    values <- some (try (L.indentGuard scn EQ i1) >> parseChunk sc')
    return (hdr, values)

pNumber :: Parser Number
pNumber = tryOne
    [ NumberQ <$> L.signed space pRatio
    , NumberR <$> L.signed space pReal
    , NumberZ <$> L.signed space L.decimal
    ]
  where
    pRatio = do
        a <- L.decimal
        void $ MC.string "/"
        b <- L.decimal
        return $ (a Data.Ratio.% b)
    pReal = do
        a <- some digitChar
        void $ MC.string "."
        b <- some digitChar
        return $ fst . head $ readFloat $ a <> "." <> b

-- | Parser valid name.
pName :: Parser Name
pName = T.pack <$> some (alphaNumChar <|> MC.char '_')

-- | Parse name in the form "a/b/c...".
pPaths :: Parser [Name]
pPaths = do
    x <- pName
    rest <- many (MC.char '/' >> pName)
    return $ x:rest

-- | Parse Signed.
pSigned :: Parser Signed
pSigned = tryOne
    [ MC.string "unsigned" >> pure Unsigned
    , MC.string "signed" >> pure Signed
    ]

parseRow :: Parser () -> Parser (Int, Text)
parseRow _ = (,)
    <$> (pInt <* (MC.char ':' >> sc))
    <*> (T.strip . T.pack <$> pLine)

pConstrain :: Parser Constrain
pConstrain = tryOne
    [ MC.string "==" >> sc >> (EqualTo <$> pNumber)
    , MC.string "/=" >> sc >> (NotEqualTo <$> pNumber)
    , MC.string ">=" >> sc >> (GreaterThanOrEqualTo <$> pNumber)
    , MC.string ">" >> sc >> (GreaterThan <$> pNumber)
    , MC.string "<=" >> sc >> (LessThanOrEqualTo <$> pNumber)
    , MC.string "<" >> sc >> (LessThan <$> pNumber)
    ]

pContent :: Parser Content
pContent = tryOne
    [ MC.string "raw" >> pure ContentRaw
    , (ContentTable . snd <$> parseList (MC.string "table") parseRow)
    , do
        MC.string "string" >> sc
        ContentString <$> tryOne
            [ MC.string "ascii" >> pure StringAscii
            , MC.string "icao" >> pure StringICAO
            , MC.string "octal" >> pure StringOctal
            ]
    , ContentInteger
        <$> pSigned <* (sc >> MC.string "integer")
        <*> (many (sc >> pConstrain))
    , ContentQuantity
        <$> pSigned <* (sc >> MC.string "quantity" >> sc)
        <*> pNumber <* sc
        <*> L.decimal <* sc
        <*> (T.pack <$> stringLiteral)
        <*> (many (sc >> pConstrain))
    , ContentBds <$> tryOne
        [ MC.string "bds" >> sc >> MC.string "?" >> pure (BdsAt Nothing)
        , MC.string "bds" >> sc >> BdsAt <$> do
            (a,b) <- (,) <$> hexDigitChar <*> hexDigitChar
            return $ Just $ BdsAddr $ read $ "0x" ++ a:b:[]
        , MC.string "bds" >> pure BdsWithAddress
        ]
    ]

-- | Parse (fixed) type.
pElement :: Parser () -> Parser Variation
pElement sc' = do
    MC.string "element" >> sc'
    n <- L.decimal
    rule <- sc' >> tryOne
        [ ContextFree <$> pContent
        , pDependent
        ]
    return $ Element n rule
  where
    pDependent = do
        (h,lst) <- parseList pHeader pCase
        return $ Dependent h lst
      where
        pHeader = MC.string "case" >> sc >> pPaths
        pCase _ = (,) <$> (pInt <* (MC.char ':' >> (try scn <|> sc))) <*> pContent

-- | Parse group of nested items.
pGroup :: Parser Variation
pGroup = Group . snd <$> parseList (MC.string "group") pItem

-- | Parse 'extended' item.
pExtended :: Parser Variation
pExtended = do
    ((et, n1,n2), lst) <- parseList parseHeader pItem
    return $ Extended et n1 n2 lst
  where
    parseHeader = do
        MC.string "extended" >> sc
        et <-
            (try (MC.string "no-trailing-fx" <* sc) >> pure ExtendedNoTrailingFx)
            <|> pure ExtendedRegular
        n1 <- L.decimal <* sc
        n2 <- L.decimal
        return (et, n1, n2)

-- | Parse 'repetitive' item.
pRepetitive :: Parser Variation
pRepetitive = do
    rt <- try pRepFx <|> pRepRegular
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i) || end)
    scn >> Repetitive <$> pure rt <*> pVariation sc'
  where
    pRepFx = (MC.string "repetitive" >> sc >> MC.string "fx" >> pure RepetitiveFx)
    pRepRegular = fmap RepetitiveRegular (MC.string "repetitive" >> sc >> L.decimal)

-- | Parse 'explicit' item.
pExplicit :: Parser Variation
pExplicit = MC.string "explicit" *> pure Explicit

-- | Parse 'compound' item.
pCompound :: Parser Variation
pCompound = do
    (a,b) <- parseList pHeader pListElement
    return $ Compound a b
  where
    pListElement sc' = try pDash <|> (Just <$> pItem sc')
    pDash = MC.char '-' >> pure Nothing
    pHeader =
        try (Just <$> (MC.string "compound" >> sc >> L.decimal))
        <|> (MC.string "compound" >> pure Nothing)

-- | Parse 'element'.
pVariation :: Parser () -> Parser Variation
pVariation sc' = tryOne
    [ pElement sc'
    , pGroup
    , pExtended
    , pRepetitive
    , pExplicit
    , pCompound
    ]

-- | Parser valid uap name.
pUapName :: Parser Name
pUapName = T.pack <$> some (alphaNumChar <|> MC.char '-')

-- | Parse spare or regular item.
pItem :: Parser () -> Parser Item
pItem sc' = try pSpare <|> pRegular
  where
    pSpare = MC.string "spare" >> sc' >> fmap Spare L.decimal
    pRegular = do
        name <- pName <* sc'
        title <- (T.pack <$> stringLiteral)
        definition <- optional . try $ do
            sc'
            (T.pack <$> blockAfter "definition")
        description <- optional . try $ do
            sc'
            (T.pack <$> blockAfter "description")
        variation <- sc' >> pVariation sc'
        remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
        let doc = Documentation definition description remark
        return $ Item name title variation doc

-- | Parse toplevel items.
pItems :: Parser [Item]
pItems = snd <$> parseList (MC.string "items") pItem

-- | Parse 'UAP'.
pUap :: Parser Uap
pUap = uaps <|> uap
  where
    parseOne _sc' = do
        result <- (MC.char '-' >> return Nothing)
                <|> (fmap Just pName)
        scn
        pure result

    uap = Uap . snd <$> parseList (MC.string "uap") parseOne

    pVariations = do
        (_, lst) <- parseList (MC.string "variations") (\_ -> parseList pUapName parseOne)
        pure lst

    pSelector = do
        (name, lst) <- parseList pHeader pCase
        pure $ UapSelector name lst
      where
        pHeader = MC.string "case" >> sc >> pPaths
        pCase _ = (,)
            <$> (pInt <* (MC.char ':' >> sc))
            <*> fmap T.pack pLine

    uaps = do
        i0 <- L.indentLevel
        MC.string "uaps" >> scn
        vars <- do
            i1 <- lookAhead L.indentLevel
            guard $ i1 > i0
            pVariations
        cs <- optional $ try $ do
            i1 <- lookAhead L.indentLevel
            guard $ i1 > i0
            pSelector
        pure $ Uaps vars cs

-- | Parse basic category description.
pBasic :: Parser Basic
pBasic = Basic
    <$> pCat "asterix"
    <*> (scn >> (T.pack <$> stringLiteral))
    <*> (scn >> pEdition)
    <*> (scn >> pDate)
    <*> optional (try (scn >> pPreamble))
    <*> (scn >> pItems)
    <*> (scn >> pUap)

-- | Parse extension
pExtension :: Parser Expansion
pExtension = Expansion
    <$> pCat "ref"
    <*> (scn >> (T.pack <$> stringLiteral))
    <*> (scn >> pEdition)
    <*> (scn >> pDate)
    <*> (scn >> (pCompound <* scn))

-- | Parse asterix.
pAsterix :: Parser Asterix
pAsterix
    = try (AsterixBasic <$> pBasic)
  <|> (AsterixExpansion <$> pExtension)

-- | Syntax implementation
syntax :: Syntax
syntax = Syntax
    { syntaxDescription = "Compact asterix syntax."
    , syntaxEncoder = Just encoder
    , syntaxDecoder = Just decoder
    }
  where
    encoder = encodeUtf8 . TL.toStrict . BL.toLazyText . renderBlockM 4 . mkBlock
    decoder filename s = first errorBundlePretty $
        parse (pAsterix <* eof) filename (decodeUtf8 s)

