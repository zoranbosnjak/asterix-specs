{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module:      Data.Asterix.Syntax.Ast
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines '.ast' syntax.
--

module Data.Asterix.Syntax.Ast (syntax) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Void
import           Data.Bool
import           Data.Bifunctor (first)
import qualified Data.Ratio
import           Numeric
import           Formatting as F
import           Data.Char (toLower)
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Asterix.Types
import           Data.Asterix.Common

-- | Dump to Text

type Accumulator = State (Int, Text)

-- | Dump some line of text to accumulator.
tell :: Text -> Accumulator ()
tell s = modify $ \(indent, buffer) ->
    let spaces = T.pack $ Prelude.take (4*indent) $ repeat ' '
        lst = [ T.stripEnd (spaces <> x) <> "\n" | x <- T.lines s]
        line = case s of
            "" -> T.pack "\n"
            _ -> mconcat lst
    in (indent, buffer <> line)

-- | Indented block.
block :: Accumulator a -> Accumulator a
block act = do
    modify $ \(a,b) -> (succ a, b)
    result <- act
    modify $ \(a,b) -> (pred a, b)
    return result

showNumber :: Number -> Text
showNumber = \case
    NumberZ i -> sformat (int) i
    NumberQ q -> sformat (int % "/" % int)
        (Data.Ratio.numerator q) (Data.Ratio.denominator q)
    NumberR r -> sformat (F.string) (showFFloat Nothing r "")

showConstrain :: Constrain -> Text
showConstrain = \case
    EqualTo num -> "== " <> showNumber num
    NotEqualTo num -> "/= " <> showNumber num
    GreaterThan num -> "> " <> showNumber num
    GreaterThanOrEqualTo num -> ">= " <> showNumber num
    LessThan num -> "< " <> showNumber num
    LessThanOrEqualTo num -> "<= " <> showNumber num

showName :: [Name] -> Text
showName = T.intercalate "/"

-- | Dump element.
dumpElement :: Element -> Accumulator ()
dumpElement = \case
    Fixed n rule -> do
        tell $ sformat ("fixed " % int) n
        block $ do
            let dumpContent = \case
                    ContentTable lst -> do
                        tell "table"
                        block $ forM_ lst $ \(key, value) -> do
                            tell $ sformat (int % ": " % stext) key value
                    ContentString st -> tell $ "string " <> case st of
                        StringAscii -> "ascii"
                        StringICAO -> "icao"
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
            case rule of
                Unspecified -> tell "raw"
                ContextFree cont -> dumpContent cont
                Dependent name lst -> do
                    tell $ sformat ("case " % stext) (showName name)
                    block $ forM_ lst $ \(x,a) -> do
                        tell $ sformat (int % ":") x
                        block $ dumpContent a

    Group lst -> do
        tell "subitems"
        block $ mapM_ dumpMaybeSubitem lst

    Extended n1 n2 lst -> do
        tell $ sformat ("extended " % int % " " % int) n1 n2
        block $ mapM_ dumpMaybeSubitem lst

    Repetitive rep element -> do
        tell $ sformat ("repetitive " % int) rep
        block $ dumpElement element

    Explicit -> do
        tell "explicit"

    Compound lst -> do
        tell "compound"
        block $ forM_ lst $ \case
            Nothing -> tell "-"
            Just subitem -> dumpMaybeSubitem subitem

    _ -> error "unexpected element"
  where
    dumpMaybeSubitem = \case
        Spare n -> tell $ sformat ("spare " % int) n
        Subitem name title mDesc element mRemark -> do
            tell $ sformat (stext % " " % F.string) name (show title)
            case mDesc of
                Nothing -> return ()
                Just desc -> block $ do
                    tell "description"
                    block $ tell desc
            block $ do
                dumpElement element
                case mRemark of
                    Nothing -> return ()
                    Just remark -> do
                        tell "remark"
                        block $ tell remark

-- | Dump toplevel item.
dumpItem :: Item -> Accumulator ()
dumpItem (Item rule definition si) = case si of
    Spare _ -> error "unexpected toplevel spare item"
    Subitem name title _mDesc element mRemark -> do
        tell $ sformat (stext % " " % F.string) name (show title)
        block $ do
            case rule of
                Unspecified -> tell "unspecified"
                ContextFree r -> tell $ sformat (F.string) (toLower <$> show r)
                Dependent _x _lst -> do
                    tell "case ..."
                    block $ return ()   -- TODO

            tell "definition"
            block $ tell definition

            dumpElement element

            case mRemark of
                Nothing -> return ()
                Just remark -> do
                    tell "remark"
                    block $ tell remark

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

-- | Encode asterix category description.
dumpAsterix :: Asterix -> Accumulator ()
dumpAsterix asterix = do
    tell $ sformat ("asterix " % left 3 '0' % " \"" % stext % "\"") cat title
    tell $ sformat ("edition " % int % "." % int) ed1 ed2
    tell $ sformat ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
    case astPreamble asterix of
        Nothing -> return ()
        Just preamble -> do
            tell "preamble"
            block $ do
                tell preamble
    tell ""

    tell "items"
    block $ forM_ (astCatalogue asterix) $ \item -> do
        tell ""
        dumpItem item

    tell ""
    dumpUap $ astUap asterix
  where
    cat = astCategory asterix
    title = astTitle asterix
    edition = astEdition asterix
    ed1 = editionMajor edition
    ed2 = editionMinor edition
    (Date year month day) = astDate asterix

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
pCat :: Parser Word8
pCat = do
    MC.string "asterix" >> sc
    (a,b,c) <- (,,) <$> digitChar <*> digitChar <*> digitChar
    return (read [a,b,c] :: Word8)

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
    [ (ContentTable . snd <$> parseList (MC.string "table") parseRow)
    , do
        MC.string "string" >> sc
        ContentString <$> tryOne
            [ MC.string "ascii" >> pure StringAscii
            , MC.string "icao" >> pure StringICAO
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
    ]

-- | Parse (fixed) type.
pFixed :: Parser () -> Parser Element
pFixed sc' = do
    MC.string "fixed" >> sc'
    n <- L.decimal
    rule <- sc' >> tryOne
        [ ContextFree <$> pContent
        , MC.string "raw" >> pure Unspecified
        , pDependent
        ]
    return $ Fixed n rule
  where
    pDependent = do
        (h,lst) <- parseList pHeader pCase
        return $ Dependent h lst
      where
        pHeader = MC.string "case" >> sc >> pPaths
        pCase _ = (,) <$> (pInt <* (MC.char ':' >> (try scn <|> sc))) <*> pContent

-- | Parse group of nested subitems.
pGroup :: Parser Element
pGroup = Group . snd <$> parseList (MC.string "subitems") pSubItem

-- | Parse 'extended' subitem.
pExtended :: Parser Element
pExtended = do
    ((n1,n2), lst) <- parseList parseHeader pSubItem
    return $ Extended n1 n2 lst
  where
    parseHeader = do
        MC.string "extended" >> sc
        n1 <- L.decimal <* sc
        n2 <- L.decimal
        return (n1, n2)

-- | Parse 'repetitive' subitem.
pRepetitive :: Parser Element
pRepetitive = do
    n <- MC.string "repetitive" >> sc >> L.decimal
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i) || end)
    scn >> Repetitive <$> pure n <*> pElement sc'

-- | Parse 'explicit' subitem.
pExplicit :: Parser Element
pExplicit = MC.string "explicit" *> pure Explicit

-- | Parse 'compound' subitem.
pCompound :: Parser Element
pCompound = Compound . snd <$> parseList (MC.string "compound") pListElement
  where
    pListElement sc' = try pDash <|> (Just <$> pSubItem sc')
    pDash = MC.char '-' >> pure Nothing

-- | Parse 'element'.
pElement :: Parser () -> Parser Element
pElement sc' = tryOne
    [ pFixed sc'
    , pGroup
    , pExtended
    , pRepetitive
    , pExplicit
    , pCompound
    --, pRFC: TODO
    ]

-- | Parser valid uap name.
pUapName :: Parser Name
pUapName = T.pack <$> some (alphaNumChar <|> MC.char '-')

-- | Parse spare or regular 'subitem'.
pSubItem :: Parser () -> Parser Subitem
pSubItem sc' = try pSpare <|> pRegular
  where
    pRegular = do
        name <- pName <* sc'
        title <- (T.pack <$> stringLiteral)
        description <- optional . try $ do
            sc'
            (T.pack <$> blockAfter "description")
        element <- sc' >> pElement sc'
        remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
        return $ Subitem name title description element remark
    pSpare = MC.string "spare" >> sc' >> fmap Spare L.decimal

-- | Parse toplevel item.
pItem :: Parser () -> Parser Item
pItem sc' = do
    name <- pName <* sc'
    title <- (T.pack <$> stringLiteral) <* sc'
    encoding <- do
        let pEncoding = tryOne
                [ MC.string "mandatory" >> sc' >> pure Mandatory
                , MC.string "optional" >> sc' >> pure Optional
                , MC.string "absent" >> sc' >> pure Absent
                ]
        tryOne
            [ ContextFree <$> pEncoding
            , MC.string "unspecified" >> sc' >> pure Unspecified
            , do
                let pCase _ = (,) <$> (pInt <* (MC.char ':' >> sc)) <*> pEncoding
                    pHeader = MC.string "case" >> sc >> pPaths
                (h,lst) <- parseList pHeader pCase
                return $ Dependent h lst
            ]
    definition <- (T.pack <$> blockAfter "definition") <* sc'
    element <- pElement sc'
    remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
    return $ Item
        encoding
        definition
        (Subitem name title Nothing element remark)

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

-- | Parse asterix category description.
pAsterix :: Parser Asterix
pAsterix = Asterix
    <$> pCat
    <*> (scn >> (T.pack <$> stringLiteral))
    <*> (scn >> pEdition)
    <*> (scn >> pDate)
    <*> optional (try (scn >> pPreamble))
    <*> (scn >> pItems)
    <*> (scn >> pUap)

-- | Syntax implementation
syntax :: Syntax
syntax = Syntax
    { syntaxDescription = "Compact asterix syntax."
    , encodeAsterix = Just encoder
    , decodeAsterix = Just decoder
    }
  where
    encoder = encodeUtf8 . snd . flip execState (0,"") . dumpAsterix
    decoder filename s = first errorBundlePretty $
        parse pAsterix filename (decodeUtf8 s)

