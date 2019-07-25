{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Word (Word8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Asterix.Types

type Parser = Parsec Void Text

-- | Parse 'cat'.
pCat :: Parser Word8
pCat = do
    string "category" >> space
    (a,b,c) <- (,,) <$> digitChar <*> digitChar <*> digitChar
    space
    return (read [a,b,c] :: Word8)

-- | Parse 'edition'.
pEdition :: Parser Edition
pEdition = do
    string "edition" >> space
    a <- L.decimal
    void $ char '.'
    b <- L.decimal
    space
    return $ Edition a b

-- | Parse 'date'.
pDate :: Parser Date
pDate = do
    string "date" >> space
    a <- L.decimal
    void $ char '-'
    b <- L.decimal
    space
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

-- | Get indent level of a string.
getIndent :: String -> Pos
getIndent = mkPos . succ . length . takeWhile (== ' ')

-- | Parse text block after a header 'hdr'.
blockAfter :: Text -> Parser String
blockAfter hdr = do
    i <- L.indentLevel
    void $ string hdr >> newline
    skipEmptyLines
    result <- unlines . leftIndent . dropEmpty <$> consumeUntilIndent i
    space
    return result
  where
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

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- | Parse first successfull parser.
tryOne :: [Parser a] -> Parser a
tryOne [] = fail "empty list"
tryOne [x] = x
tryOne (x:xs) = try x <|> tryOne xs

-- | Parse a list of chunks (all chunks have the same initial indent level).
parseList :: Parser h -> Parser a -> Parser (h, [a])
parseList parseHeader parseOne = do
    hdr <- parseHeader <* newline
    skipEmptyLines
    i <- getIndent <$> lookAhead pLine
    space
    values <- some $ do
        j <- L.indentLevel
        when  (j /= i) $ fail "unexpected indent"
        parseOne
    return (hdr, values)

pNumber :: Parser Number
pNumber = tryOne
    [ NumberR <$> ((L.signed space L.float) <* space)
    , NumberZ <$> ((L.signed space L.decimal) <* space)
    ]

-- | Parse (fixed) type.
pFixed :: Parser ItemType
pFixed = do
    string "fixed" >> space
    n <- L.decimal <* space
    val <- tryOne
        [ string "raw" >> space >> pure Raw
        , string "signed" >> space >> fmap Signed pQuantity
        , string "unsigned" >> space >> fmap Unsigned pQuantity
        , Table . snd <$> parseList (string "values") parseRow
        , string "string" >> pure Ascii
        ]
    return $ Fixed n val
  where
    pQuantity = do
        scale <- string "scale" >> space >> pNumber
        fract <- string "fract" >> space >> (L.decimal <* space)
        unit <- optional . try $ do
            string "unit" >> space
            (T.pack <$> stringLiteral) <* space
        lo <- optional $ tryOne
            [ string "ge" >> space >> (Including <$> pNumber)
            , string "gt" >> space >> (Excluding <$> pNumber)
            ]
        hi <- optional $ tryOne
            [ string "le" >> space >> (Including <$> pNumber)
            , string "lt" >> space >> (Excluding <$> pNumber)
            ]
        return $ Quantity scale fract unit lo hi
    parseRow = do
        val <- L.decimal
        char ':' >> space
        msg <- T.strip . T.pack <$> pLine
        space
        return (val, msg)

-- | Parse group of nested items.
pGroup :: Parser ItemType
pGroup = do
    Group . snd <$> parseList (string "items") pItem

-- | Parse 'extended' item.
pExtended :: Parser ItemType
pExtended = do
    ((n1,n2), lst) <- parseList parseHeader pItem
    return $ Extended n1 n2 lst
  where
    parseHeader = do
        string "extended" >> space
        n1 <- L.decimal <* space
        n2 <- L.decimal
        return (n1, n2)

-- | Parse 'compound' item.
pCompound :: Parser ItemType
pCompound = do
    (_, lst) <- parseList (string "compound") pItem
    return $ Compound lst

-- | Parse item 'content'.
pContent :: Parser ItemType
pContent = tryOne
    [ pFixed
    , pGroup
    , pExtended
    --, pRepetitive
    --, pExplicit
    , pCompound
    --, pRFC
    ]

-- | Parse spare or regular 'item'.
pItem :: Parser Item
pItem = try pSpare <|> do
    name <- some alphaNumChar <* space
    title <- (T.pack <$> stringLiteral) <* space
    description <- optional . try $ do
        (T.pack <$> blockAfter "description")
    content <- pContent
    return $ Item name title description content
  where
    pSpare = do
        string "spare" >> space
        fmap Spare L.decimal <* space

-- | Parse toplevel item at ident level 'p'.
pToplevelItem :: Parser Toplevel
pToplevelItem = do
    name <- some alphaNumChar <* space
    title <- (T.pack <$> stringLiteral) <* space
    mandatory <-
        (string "mandatory" *> pure True)
        <|> (string "optional" *> pure False)
    space
    definition <- (T.pack <$> blockAfter "definition") <* space
    content <- pContent
    remark <- optional . try $ do
        (T.pack <$> blockAfter "remark")
    return $ Toplevel
        mandatory
        definition
        (Item name title Nothing content)
        remark

-- | Parse toplevel items.
pToplevelItems :: Parser [Toplevel]
pToplevelItems = snd <$> parseList (string "items") pToplevelItem

-- | Parse 'UAP'.
pUap :: Parser Uap
pUap = uap {- <|> uaps -}
  where
    parseOne
        = (char '-' >> space >> return Nothing)
      <|> (fmap Just (some alphaNumChar <* space))
    uap = Uap . snd <$> parseList (string "uap") parseOne
    -- uaps = undefined -- TODO

-- | Parse category description.
pCategory :: Parser Category
pCategory = Category
    <$> pCat
    <*> (T.pack <$> stringLiteral) <* space
    <*> pEdition
    <*> pDate
    <*> (fmap Just (try pPreamble) <|> pure Nothing)
    <*> pToplevelItems
    <*> pUap

