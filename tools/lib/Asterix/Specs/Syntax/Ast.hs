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
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

import           Asterix.Specs.Types
import           Asterix.Specs.Common
import           Asterix.Indent

-- | Dump to Text

class IsBlock a where
    mkBlock :: a -> Block Text

instance IsBlock Content where
    mkBlock = \case
        ContentRaw -> "raw"
        ContentTable lst -> mconcat
            [ "table"
            , indent $ mconcat $ do
                (key, value) <- lst
                pure $ line $ sformat (int % ": " % stext) key value
            ]
        ContentString st -> line $ "string " <> case st of
            StringAscii -> "ascii"
            StringICAO -> "icao"
            StringOctal -> "octal"
        ContentInteger signed constraints ->
            let sig = toLower <$> show signed
                cst = case constraints of
                    [] -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            in line $ sformat (F.string % " integer" % stext) sig cst
        ContentQuantity signed scaling fract unit constraints ->
            let sig = toLower <$> show signed
                cst = case constraints of
                    [] -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            in line $ sformat
                (F.string % " quantity " % stext % " " % int % " " % "\"" % stext % "\"" % stext )
                sig (showNumber scaling) fract unit cst
        ContentBds bt -> line $ "bds" <> case bt of
            BdsWithAddress -> ""
            BdsAt mAddr -> case mAddr of
                Nothing -> " ?"
                Just (BdsAddr addr) -> sformat (" " % hex) addr

instance IsBlock Variation where
    mkBlock = \case
        Element n rule -> mconcat
            [ line $ sformat ("element " % int) n
            , indent $ case rule of
                ContextFree cont -> mkBlock cont
                Dependent name lst -> mconcat
                    [ line $ sformat ("case " % stext) (showPath name)
                    , indent $ mconcat $ do
                        (x,a) <- lst
                        pure $ mconcat
                            [ line $ sformat (int % ":") x
                            , indent $ mkBlock a
                            ]
                    ]
            ]

        Group lst -> mconcat
            [ "group"
            , indent $ mconcat $ fmap mkBlock lst
            ]

        Extended et n1 n2 lst -> mconcat
            [ line $ sformat ("extended " % F.string % int % " " % int) fx n1 n2
            , indent $ mconcat $ fmap mkBlock lst
            ]
          where
            fx = case et of
                ExtendedRegular -> ""
                ExtendedNoTrailingFx -> "no-trailing-fx "

        Repetitive rep variation -> mconcat
            [ line $ sformat ("repetitive " % int) rep
            , indent $ mkBlock variation
            ]

        Explicit -> "explicit"

        Compound mFspecSize lst -> mconcat
            [ case mFspecSize of
                Nothing -> "compound"
                Just n -> line $ sformat ("compound " % int) n
            , indent $ mconcat $ do
                mItem <- lst
                pure $ case mItem of
                    Nothing -> "-"
                    Just item -> mkBlock item
            ]

instance IsBlock Item where
    mkBlock = \case
        Spare n -> line $ sformat ("spare " % int) n
        Item name title variation doc -> mconcat
            [ line $ sformat (stext % " \"" % stext % "\"") name title
            , indent $ dumpText "definition" (docDefinition doc)
            , indent $ dumpText "description" (docDescription doc)
            , indent $ mconcat
                [ mkBlock variation
                , dumpText "remark" (docRemark doc)
                ]
            ]
      where
        dumpText title = \case
            Nothing -> mempty
            Just t -> mconcat
                [ line title
                , indent $ mconcat [line i | i <- T.lines t]
                ]

instance IsBlock Uap where
    mkBlock = \case
        Uap lst -> mconcat
            [ "uap"
            , indent $ dumpList lst
            ]
        Uaps variations -> mconcat
            [ "uaps"
            , mconcat $ do
                (name, lst) <- variations
                pure $ indent $ mconcat
                    [ line $ name
                    , indent $ dumpList lst
                    ]
            ]
      where
        dumpList lst = mconcat $ do
            item <- lst
            pure $ line $ maybe "-" id item

instance IsBlock Basic where
    mkBlock basic = mconcat
        [ line $ sformat ("asterix " % left 3 '0' % " \"" % stext % "\"") cat title
        , line $ sformat ("edition " % int % "." % int) ed1 ed2
        , line $ sformat ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
        , case basPreamble basic of
            Nothing -> mempty
            Just preamble -> mconcat
                [ "preamble"
                , indent $ mconcat [line i | i <- T.lines preamble]
                ]
        , emptyLine
        , "items"
        , emptyLine
        , indent $ blocksLn (mkBlock <$> basCatalogue basic)
        , emptyLine
        , mkBlock $ basUap basic
        ]
      where
        cat = basCategory basic
        title = basTitle basic
        edition = basEdition basic
        ed1 = editionMajor edition
        ed2 = editionMinor edition
        (Date year month day) = basDate basic

instance IsBlock Expansion where
    mkBlock x = mconcat
        [ line $ sformat ("ref " % left 3 '0' % " \"" % stext % "\"") cat title
        , line $ sformat ("edition " % int % "." % int) ed1 ed2
        , line $ sformat ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
        , emptyLine
        , mkBlock $ expVariation x
        ]
      where
        cat = expCategory x
        title = expTitle x
        edition = expEdition x
        ed1 = editionMajor edition
        ed2 = editionMinor edition
        (Date year month day) = expDate x

instance IsBlock Asterix where
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
    n <- MC.string "repetitive" >> sc >> L.decimal
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i) || end)
    scn >> Repetitive <$> pure n <*> pVariation sc'

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
    parseOne _sc'
        = (MC.char '-' >> return Nothing)
      <|> (fmap Just pName)
    uap = Uap . snd <$> parseList (MC.string "uap") parseOne
    uaps = do
        (_, lst) <- parseList (MC.string "uaps") (\_ -> parseList pUapName parseOne)
        return $ Uaps lst

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
    <*> (scn >> pCompound)

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
    encoder = encodeUtf8 . renderBlock 4 . mkBlock
    decoder filename s = first errorBundlePretty $
        parse pAsterix filename (decodeUtf8 s)

