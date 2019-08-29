{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

-- |
-- Module:      Data.Asterix.Parser
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines Asterix parsers.
--

module Data.Asterix.Parser where

import           Control.Monad
import           Data.Void
import           Data.Bool
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Asterix.Types

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

-- | Parse with first successfull parser.
tryOne :: [Parser a] -> Parser a
tryOne [] = fail "empty list"
tryOne [x] = x
tryOne (x:xs) = try x <|> tryOne xs

-- | Parse 'category'.
pCat :: Parser Word8
pCat = do
    string "category" >> sc
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
    (a,_,b) <- (,,) <$> L.decimal <*> char '-' <*> L.decimal
    return $ Date a b

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
    , NumberZ <$> (L.signed space L.decimal)
    ]

-- | Parse (fixed) type.
pFixed :: Parser () -> Parser ItemType
pFixed sc' = do
    string "item" >> sc'
    n <- L.decimal
    val <- sc' >> tryOne
        [ string "raw" >> pure Raw
        , string "signed" >> fmap Signed pQuantity
        , string "unsigned" >> fmap Unsigned pQuantity
        , (Table . snd <$> parseList (string "discrete") parseRow)
        , string "string" >> sc >> string "ascii" >> pure StringAscii
        , string "string" >> sc >> string "icao" >> pure StringICAO
        ]
    return $ Fixed n val
  where
    pQuantity = do
        scale <- try (sc' >> string "scale" >> sc' >> pNumber) <|> pure (NumberZ 1)
        fract <- try (sc' >> string "fractional" >> sc' >> (L.decimal <* sc')) <|> pure 0
        unit <- optional . try $
            ( sc' >> string "unit" >> sc' >> (T.pack <$> stringLiteral) )
        lo <- optional . try $ tryOne
            [ sc' >> string "ge" >> sc' >> (Including <$> pNumber)
            , sc' >> string "gt" >> sc' >> (Excluding <$> pNumber)
            ]
        hi <- optional . try $ tryOne
            [ sc' >> string "le" >> sc' >> (Including <$> pNumber)
            , sc' >> string "lt" >> sc' >> (Excluding <$> pNumber)
            ]
        return $ Quantity scale fract unit lo hi

    parseRow _ = do
        val <- L.decimal    -- TODO: add support for [INT, CHR, HEX, OCT]
        char ':' >> sc
        msg <- T.strip . T.pack <$> pLine
        return (val, msg)

-- | Parse group of nested items.
pGroup :: Parser ItemType
pGroup = do
    Group . snd <$> parseList (string "subitems") pItem

-- | Parse 'extended' item.
pExtended :: Parser ItemType
pExtended = do
    ((n1,n2), lst) <- parseList parseHeader pItem
    return $ Extended n1 n2 lst
  where
    parseHeader = do
        string "extended" >> sc
        n1 <- L.decimal <* sc
        n2 <- L.decimal
        return (n1, n2)

-- | Parse 'repetitive' item.
pRepetitive :: Parser ItemType
pRepetitive = do
    void $ string "repetitive"
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (return ()) ((j > i) || end)
    scn >> fmap Repetitive (pContent sc')

-- | Parse 'compound' item.
pCompound :: Parser ItemType
pCompound = Compound . snd <$> parseList (string "compound") pItem

-- | Parse item 'content'.
pContent :: Parser () -> Parser ItemType
pContent sc' = tryOne
    [ pFixed sc'
    , pGroup
    , pExtended
    , pRepetitive
    --, pExplicit
    , pCompound
    --, pRFC
    ]

-- | Parse spare or regular 'item'.
pItem :: Parser () -> Parser Item
pItem sc' = try pSpare <|> pRegular
  where
    pRegular = do
        name <- some alphaNumChar <* sc'
        title <- (T.pack <$> stringLiteral)
        description <- optional . try $ do
            sc'
            (T.pack <$> blockAfter "description")
        content <- sc' >> pContent sc'
        remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
        return $ Item name title description content remark
    pSpare = string "spare" >> sc' >> fmap Spare L.decimal

-- | Parse toplevel item.
pToplevelItem :: Parser () -> Parser Toplevel
pToplevelItem sc' = do
    name <- some alphaNumChar <* sc'
    title <- (T.pack <$> stringLiteral) <* sc'
    mandatory <-
        (string "mandatory" *> pure True)
        <|> (string "optional" *> pure False)
    sc'
    definition <- (T.pack <$> blockAfter "definition") <* sc'
    content <- pContent sc'
    remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
    return $ Toplevel
        mandatory
        definition
        (Item name title Nothing content remark)

-- | Parse toplevel items.
pToplevelItems :: Parser [Toplevel]
pToplevelItems = snd <$> parseList (string "items") pToplevelItem

-- | Parse 'UAP'.
pUap :: Parser Uap
pUap = uap {- <|> uaps -}
  where
    parseOne _sc'
        = (char '-' >> return Nothing)
      <|> (fmap Just (some alphaNumChar))
    uap = Uap . snd <$> parseList (string "uap") parseOne
    -- uaps = undefined -- TODO

-- | Parse category description.
pCategory :: Parser Category
pCategory = Category
    <$> pCat
    <*> (scn >> (T.pack <$> stringLiteral))
    <*> (scn >> pEdition)
    <*> (scn >> pDate)
    <*> optional (try (scn >> pPreamble))
    <*> (scn >> pToplevelItems)
    <*> (scn >> pUap)

