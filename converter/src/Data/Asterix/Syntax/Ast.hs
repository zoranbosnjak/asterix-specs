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
import           Data.Void
import           Data.Bool
import           Data.Bifunctor (first)
import           Data.Ratio ((%))
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Asterix.Types
import           Data.Asterix.Common

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
sc = smartSkip (void $ some (char ' ' <|> char '\t'))

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

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
    string "asterix" >> sc
    (a,b,c) <- (,,) <$> digitChar <*> digitChar <*> digitChar
    return (read [a,b,c] :: Word8)

-- | Parse 'edition'.
pEdition :: Parser Edition
pEdition = do
    string "edition" >> sc
    (a,_,b) <- (,,) <$> L.decimal <*> char '.' <*> L.decimal
    return $ Edition a b

-- | Parse 'date'.
pDate :: Parser Date
pDate = do
    string "date" >> sc
    Date <$> L.decimal <*> (char '-' *> L.decimal ) <*> (char '-' *> L.decimal )

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
    void $ string hdr >> many (char ' ') >> newline
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
        void $ string "/"
        b <- L.decimal
        return $ (a % b)

-- | Parser valid name.
pName :: Parser Name
pName = T.pack <$> some (alphaNumChar <|> char '_')

-- | Parse name in the form "a/b/c...".
pPaths :: Parser [Name]
pPaths = do
    x <- pName
    rest <- many (char '/' >> pName)
    return $ x:rest

-- | Parse Signed.
pSigned :: Parser Signed
pSigned = tryOne
    [ string "unsigned" >> pure Unsigned
    , string "signed" >> pure Signed
    ]

parseRow :: Parser () -> Parser (Int, Text)
parseRow _ = (,)
    <$> (pInt <* (char ':' >> sc))
    <*> (T.strip . T.pack <$> pLine)

pConstrain :: Parser Constrain
pConstrain = tryOne
    [ string "==" >> sc >> (EqualTo <$> pNumber)
    , string "/=" >> sc >> (NotEqualTo <$> pNumber)
    , string ">=" >> sc >> (GreaterThanOrEqualTo <$> pNumber)
    , string ">" >> sc >> (GreaterThan <$> pNumber)
    , string "<=" >> sc >> (LessThanOrEqualTo <$> pNumber)
    , string "<" >> sc >> (LessThan <$> pNumber)
    ]

pContent :: Parser Content
pContent = tryOne
    [ (ContentTable . snd <$> parseList (string "table") parseRow)
    , do
        string "string" >> sc
        ContentString <$> tryOne
            [ string "ascii" >> pure StringAscii
            , string "icao" >> pure StringICAO
            ]
    , ContentInteger
        <$> pSigned <* (sc >> string "integer")
        <*> (many (sc >> pConstrain))
    , ContentQuantity
        <$> pSigned <* (sc >> string "quantity" >> sc)
        <*> pNumber <* sc
        <*> L.decimal <* sc
        <*> (T.pack <$> stringLiteral)
        <*> (many (sc >> pConstrain))
    ]

-- | Parse (fixed) type.
pFixed :: Parser () -> Parser Element
pFixed sc' = do
    string "fixed" >> sc'
    n <- L.decimal
    rule <- sc' >> tryOne
        [ ContextFree <$> pContent
        , string "raw" >> pure Unspecified
        , pDependent
        ]
    return $ Fixed n rule
  where
    pDependent = do
        (h,lst) <- parseList pHeader pCase
        return $ Dependent h lst
      where
        pHeader = string "case" >> sc >> pPaths
        pCase _ = (,) <$> (pInt <* (char ':' >> (try scn <|> sc))) <*> pContent

-- | Parse group of nested subitems.
pGroup :: Parser Element
pGroup = Group . snd <$> parseList (string "subitems") pSubItem

-- | Parse 'extended' subitem.
pExtended :: Parser Element
pExtended = do
    ((n1,n2), lst) <- parseList parseHeader pSubItem
    return $ Extended n1 n2 lst
  where
    parseHeader = do
        string "extended" >> sc
        n1 <- L.decimal <* sc
        n2 <- L.decimal
        return (n1, n2)

-- | Parse 'repetitive' subitem.
pRepetitive :: Parser Element
pRepetitive = do
    n <- string "repetitive" >> sc >> L.decimal
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i) || end)
    scn >> Repetitive <$> pure n <*> pElement sc'

-- | Parse 'explicit' subitem.
pExplicit :: Parser Element
pExplicit = string "explicit" *> pure Explicit

-- | Parse 'compound' subitem.
pCompound :: Parser Element
pCompound = Compound . snd <$> parseList (string "compound") pListElement
  where
    pListElement sc' = try pDash <|> (Just <$> pSubItem sc')
    pDash = char '-' >> pure Nothing

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
pUapName = T.pack <$> some (alphaNumChar <|> char '-')

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
    pSpare = string "spare" >> sc' >> fmap Spare L.decimal

-- | Parse toplevel item.
pItem :: Parser () -> Parser Item
pItem sc' = do
    name <- pName <* sc'
    title <- (T.pack <$> stringLiteral) <* sc'
    encoding <- do
        let pEncoding = tryOne
                [ string "mandatory" >> sc' >> pure Mandatory
                , string "optional" >> sc' >> pure Optional
                , string "absent" >> sc' >> pure Absent
                ]
        tryOne
            [ ContextFree <$> pEncoding
            , string "unspecified" >> sc' >> pure Unspecified
            , do
                let pCase _ = (,) <$> (pInt <* (char ':' >> sc)) <*> pEncoding
                    pHeader = string "case" >> sc >> pPaths
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
pItems = snd <$> parseList (string "items") pItem

-- | Parse 'UAP'.
pUap :: Parser Uap
pUap = uaps <|> uap
  where
    parseOne _sc'
        = (char '-' >> return Nothing)
      <|> (fmap Just pName)
    uap = Uap . snd <$> parseList (string "uap") parseOne
    uaps = do
        (_, lst) <- parseList (string "uaps") (\_ -> parseList pUapName parseOne)
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
    , encodeAsterix = Nothing       -- encoding not supported
    , decodeAsterix = Just decoder
    }
  where
    decoder filename s = first errorBundlePretty $
        parse pAsterix filename (decodeUtf8 s)

