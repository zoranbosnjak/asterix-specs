{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- '.ast' syntax implementation

module Data.Asterix.Syntax.Ast (syntax) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Void
import           Data.Bool
import           Data.Bifunctor (first)
import qualified Data.Ratio
import           Formatting as F
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Asterix.Types
import           Data.Asterix.Common
import           Data.Asterix.Indent

-- | Dump to Text

-- | Dump variation.
dumpVariation :: Variation -> Accumulator ()
dumpVariation = \case
    Element n rule -> do
        tell $ sformat ("element " % int) n
        block $ do
            let dumpContent = \case
                    ContentRaw -> do
                        tell "raw"
                    ContentTable lst -> do
                        tell "table"
                        block $ forM_ lst $ \(key, value) -> do
                            tell $ sformat (int % ": " % stext) key value
                    ContentString st -> tell $ "string " <> case st of
                        StringAscii -> "ascii"
                        StringICAO -> "icao"
                        StringOctal -> "octal"
                    ContentInteger signed constraints -> do
                        let sig = toLower <$> show signed
                            cst = case constraints of
                                [] -> ""
                                lst -> " " <> T.intercalate " " (fmap showConstrain lst)
                        tell $ sformat (F.string % " integer" % stext) sig cst
                    ContentQuantity signed scaling fract unit constraints -> do
                        let sig = toLower <$> show signed
                            cst = case constraints of
                                [] -> ""
                                lst -> " " <> T.intercalate " " (fmap showConstrain lst)
                        tell $ sformat
                            (F.string % " quantity " % stext % " " % int % " " % F.string % stext )
                            sig (showNumber scaling) fract (show unit) cst
                    ContentBds bt -> tell $ "bds" <> case bt of
                        BdsWithAddress -> ""
                        BdsAt mAddr -> case mAddr of
                            Nothing -> " ?"
                            Just (BdsAddr addr) -> sformat (" " % hex) addr
            case rule of
                ContextFree cont -> dumpContent cont
                Dependent name lst -> do
                    tell $ sformat ("case " % stext) (showPath name)
                    block $ forM_ lst $ \(x,a) -> do
                        tell $ sformat (int % ":") x
                        block $ dumpContent a

    Group lst -> do
        tell "group"
        block $ mapM_ dumpItem lst

    Extended n1 n2 lst -> do
        tell $ sformat ("extended " % int % " " % int) n1 n2
        block $ mapM_ dumpItem lst

    Repetitive rep variation -> do
        tell $ sformat ("repetitive " % int) rep
        block $ dumpVariation variation

    Explicit -> do
        tell "explicit"

    Compound mFspecSize lst -> do
        case mFspecSize of
            Nothing -> tell "compound"
            Just n -> tell $ sformat ("compound " % int) n
        block $ forM_ lst $ \case
            Nothing -> tell "-"
            Just item -> dumpItem item

dumpItem :: Item -> Accumulator ()
dumpItem = \case
    Spare n -> tell $ sformat ("spare " % int) n
    Item name title variation doc -> do
        tell $ sformat (stext % " " % F.string) name (show title)
        block $ dumpText "definition" (docDefinition doc)
        block $ dumpText "description" (docDescription doc)
        block $ do
            dumpVariation variation
            dumpText "remark" (docRemark doc)
  where
    dumpText title = \case
        Nothing -> return ()
        Just t -> do
            tell title
            block $ tell t

-- | Encode Uap.
dumpUap :: Uap -> Accumulator ()
dumpUap = \case
    Uap lst -> do
        tell "uap"
        dumpList lst
    Uaps variations -> do
        tell "uaps"
        block $ forM_ variations $ \(name, lst) -> do
            tell name
            dumpList lst
  where
    dumpList lst = block $ forM_ lst $ \item -> do
        tell $ maybe "-" id item

-- | Encode asterix basic category description.
dumpBasic :: Basic -> Accumulator ()
dumpBasic basic = do
    tell $ sformat ("asterix " % left 3 '0' % " \"" % stext % "\"") cat title
    tell $ sformat ("edition " % int % "." % int) ed1 ed2
    tell $ sformat ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
    case basPreamble basic of
        Nothing -> return ()
        Just preamble -> do
            tell "preamble"
            block $ do
                tell preamble

    tell ""
    tell "items"
    block $ forM_ (basCatalogue basic) $ \item -> do
        tell ""
        dumpItem item

    tell ""
    dumpUap $ basUap basic
  where
    cat = basCategory basic
    title = basTitle basic
    edition = basEdition basic
    ed1 = editionMajor edition
    ed2 = editionMinor edition
    (Date year month day) = basDate basic

-- | Encode expansion
dumpExpansion :: Expansion -> Accumulator ()
dumpExpansion x = do
    tell $ sformat ("ref " % left 3 '0' % " \"" % stext % "\"") cat title
    tell $ sformat ("edition " % int % "." % int) ed1 ed2
    tell $ sformat ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
    tell ""
    dumpVariation $ expVariation x
  where
    cat = expCategory x
    title = expTitle x
    edition = expEdition x
    ed1 = editionMajor edition
    ed2 = editionMinor edition
    (Date year month day) = expDate x

-- | Encode asterix description.
dumpAsterix :: Asterix -> Accumulator ()
dumpAsterix = \case
    AsterixBasic basic -> dumpBasic basic
    AsterixExpansion expansion -> dumpExpansion expansion

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
    [ NumberR <$> (L.signed space L.float)
    , NumberQ <$> pRatio
    , NumberZ <$> (L.signed space L.decimal)
    ]
  where
    pRatio = do
        a <- L.decimal
        void $ MC.string "/"
        b <- L.decimal
        return $ (a Data.Ratio.% b)

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
    ((n1,n2), lst) <- parseList parseHeader pItem
    return $ Extended n1 n2 lst
  where
    parseHeader = do
        MC.string "extended" >> sc
        n1 <- L.decimal <* sc
        n2 <- L.decimal
        return (n1, n2)

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
    encoder = encodeUtf8 . renderBuffer 4 . snd . flip execState (0,[]) . dumpAsterix
    decoder filename s = first errorBundlePretty $
        parse pAsterix filename (decodeUtf8 s)

