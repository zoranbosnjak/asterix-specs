-- | '.ast' syntax implementation

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs.Syntax.Ast where

import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Bifunctor                 (first)
import           Data.Bool
import           Data.Char                      (toLower)
import           Data.Either
import           Data.List                      (intersperse)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Lazy.Builder         (Builder)
import           Data.Void
import           Formatting                     as F
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char           as MC
import qualified Text.Megaparsec.Char.Lexer     as L

import           Asterix.Specs.Indent
import           Asterix.Specs.Syntax           (Coder (..), showConstrain,
                                                 showNumber, showPath)
import           Asterix.Specs.Types

-- | Parse from Text

type Parser = Parsec Void Text

-- | Parse with first successfull parser.
tryOne :: [Parser a] -> Parser a
tryOne []     = fail "empty list"
tryOne [x]    = x
tryOne (x:xs) = try x <|> tryOne xs

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
pInt = L.decimal

-- | Parse to the end of line.
pLine :: Parser String
pLine = many printChar <* newline

skipEmptyLines :: Parser ()
skipEmptyLines = do
    peek <- lookAhead pLine
    case null peek of
        True  -> pLine >> skipEmptyLines
        False -> pure ()

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
        True -> pure []
        False -> do
            peek <- lookAhead pLine
            case null peek || getIndent peek > i of
                False -> pure []
                True  -> (:) <$> pLine <*> consumeUntilIndent i

    -- drop empty lines at the end of a list
    dropEmpty :: [String] -> [String]
    dropEmpty [] = []
    dropEmpty lst
        | null (last lst) = dropEmpty (init lst)
        | otherwise = lst

    leftIndent lst = fmap (drop n) lst where
        n = pred $ unPos i
        i = minimum (getIndent <$> filter (not . null) lst)

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
            bool (fail "unexpected indent") (pure ()) ((j > i1) || end)
    values <- some (try (L.indentGuard scn EQ i1) >> parseChunk sc')
    pure (hdr, values)

-- | Parser valid name.
pName :: Parser Text
pName = T.pack <$> some (alphaNumChar <|> MC.char '_')

pItemName :: Parser ItemName
pItemName = fmap ItemName pName

-- | Parse name in the form "a/b/c...".
pPaths :: Parser ItemPath
pPaths = fmap ItemPath lst where
    lst = (:) <$> pItemName <*> many (MC.char '/' >> pItemName)

pTuple :: Text -> Parser a -> Text -> Parser [a]
pTuple tOpen p tClose = do
    MC.string tOpen >> try sc
    result <- (:)
        <$> p
        <*> many (MC.char ',' >> try sc >> p)
    _ <- try sc >> MC.string tClose
    pure result

pPathsMulti :: Parser [ItemPath]
pPathsMulti = pTuple "(" pPaths ")"

pRule :: forall a. Parser () -> Parser a -> Parser (Rule a)
pRule sc' pX = sc' >> tryOne
    [ pDependent
    , ContextFree <$> pX
    ]
  where
    pHeader :: Parser [ItemPath]
    pHeader = do
        MC.string "case" >> sc
        tryOne
            [ fmap pure pPaths
            , pPathsMulti
            ]

    pCase :: Parser () -> Parser (Either a ([Int], a))
    pCase _sc' = do
        p <- tryOne
            [ MC.string "default" >> pure Nothing
            , Just <$> pTuple "(" pInt ")"
            , Just . pure <$> pInt
            ]
        MC.char ':' >> (try scn <|> sc)
        x <- pX
        case p of
            Nothing -> pure (Left x)
            Just n  -> pure (Right (n, x))

    pDependent :: Parser (Rule a)
    pDependent = do
        (items, eLst) <- parseList pHeader pCase
        dv <- case lefts eLst of
            []  -> fail "default value is expected"
            [x] -> pure x
            _   -> fail "only one default value is expected"
        pure $ Dependent items dv (rights eLst)

pNumber :: Parser Number
pNumber = makeExprParser pTerm operatorTable
  where
    symbol :: Text -> Parser Text
    symbol = L.symbol sc

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    pNumInt = NumInt <$> L.signed space L.decimal

    pNumPow = do
        a <- L.signed space L.decimal
        void $ MC.string "^"
        b <- L.signed space L.decimal
        pure $ NumPow a b

    pTerm = tryOne
        [ parens pNumber
        , pNumPow
        , pNumInt
        ]

    binary name f = InfixL (f <$ symbol name)

    operatorTable =
        [ [ binary "/" NumDiv
          ]
        ]

-- | Parse Signedness.
pSignedness :: Parser Signedness
pSignedness = tryOne
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
    , ContentTable . snd <$> parseList (MC.string "table") parseRow
    , do
        MC.string "string" >> sc
        ContentString <$> tryOne
            [ MC.string "ascii" >> pure StringAscii
            , MC.string "icao" >> pure StringICAO
            , MC.string "octal" >> pure StringOctal
            ]
    , ContentInteger
        <$> pSignedness <* (sc >> MC.string "integer")
        <*> many (sc >> pConstrain)
    , ContentQuantity
        <$> pSignedness <* (sc >> MC.string "quantity" >> sc)
        <*> pNumber <* sc
        <*> (Unit . T.pack <$> stringLiteral)
        <*> many (sc >> pConstrain)
    , ContentBds <$> tryOne
        [ MC.string "bds" >> sc >> MC.string "?" >> pure (BdsAt Nothing)
        , MC.string "bds" >> sc >> BdsAt <$> do
            (a,b) <- (,) <$> hexDigitChar <*> hexDigitChar
            pure $ Just $ BdsAddr $ read $ "0x" ++ [a, b]
        , MC.string "bds" >> pure BdsWithAddress
        ]
    ]

-- | Parse (fixed) type.
pElement :: Parser () -> Parser (Variation ())
pElement sc' = Element
    <$> (MC.string "element" >> sc' >> pure ())
    <*> fmap BitSize L.decimal
    <*> pRule sc' pContent

-- | Parse group of nested items.
pGroup :: Parser (Variation ())
pGroup = Group () . snd <$> parseList (MC.string "group") pItem

-- | Parse 'extended' item.
pExtended :: Parser (Variation ())
pExtended = do
    (_a, b) <- parseList (MC.string "extended") pListElement
    pure $ Extended b
  where
    pListElement sc' = tryOne
        [ MC.char '-' >> pure Nothing
        , Just <$> pItem sc'
        ]

-- | Parse 'repetitive' item.
pRepetitive :: Parser (Variation ())
pRepetitive = do
    rt <- tryOne
        [ pRepFx
        , pRepRegular
        ]
    i <- lookAhead (scn >> L.indentLevel)
    let sc' = do
            scn
            j <- L.indentLevel
            end <- atEnd
            bool (fail "unexpected indent") (pure ()) ((j > i) || end)
    scn >> (Repetitive rt <$> pVariation sc')
  where
    pRepFx = do
        void $ MC.string "repetitive" >> sc >> MC.string "fx"
        pure RepetitiveFx
    pRepRegular = RepetitiveRegular
        <$> (MC.string "repetitive" >> sc >> fmap ByteSize L.decimal)

-- | Parse 'explicit' item.
pExplicit :: Parser (Variation ())
pExplicit = Explicit <$> go where
    go = tryOne
        [ Just <$> (MC.string "explicit" >> sc >> pt)
        , MC.string "explicit" >> pure Nothing
        ]
    pt = tryOne
        [ MC.string "re" >> pure ReservedExpansion
        , MC.string "sp" >> pure SpecialPurpose
        ]

-- | Parse 'compound' item.
pCompound :: Parser (Variation ())
pCompound = do
    lst <- snd <$> parseList pHeader pListElement
    pure $ Compound lst
  where
    pHeader = MC.string "compound" >> sc
    pListElement sc' = try pDash <|> (Just <$> pNonSpare sc')
    pDash = MC.char '-' >> pure Nothing

-- | Parse 'element'.
pVariation :: Parser () -> Parser (Variation ())
pVariation sc' = tryOne
    [ pElement sc'
    , pGroup
    , pExtended
    , pRepetitive
    , pExplicit
    , pCompound
    ]

pNonSpare :: Parser () -> Parser (NonSpare ())
pNonSpare sc' = do
    name <- pItemName <* sc'
    title <- Title . T.pack <$> stringLiteral
    definition <- optional . try $ do
        sc'
        T.pack <$> blockAfter "definition"
    description <- optional . try $ do
        sc'
        T.pack <$> blockAfter "description"
    rule <- pRule sc' (pVariation sc')
    remark <- optional . try $ (sc' >> (T.pack <$> blockAfter "remark"))
    let doc = Documentation definition description remark
    pure $ NonSpare name title rule doc

-- | Parse spare or regular item.
pItem :: Parser () -> Parser (Item ())
pItem sc' = tryOne
    [ pSpare
    , fmap Item (pNonSpare sc')
    ]
  where
    pSpare = Spare
        <$> (MC.string "spare" >> sc' >> pure ())
        <*> fmap BitSize L.decimal

-- | Parse toplevel items.
pItems :: Parser [NonSpare ()]
pItems = snd <$> parseList (MC.string "items") pNonSpare

-- | Parser valid uap name.
pUapName :: Parser UapName
pUapName = UapName . T.pack <$> some (alphaNumChar <|> MC.char '-')

-- | Parse 'UAP'.
pUap :: Parser (Uap [UapItem ItemName])
pUap = tryOne
    [ uaps
    , uap
    ]
  where
    parseOne _sc' = do
        result <- tryOne
            [ MC.char '-' >> pure UapItemSpare
            , MC.string "rfs" >> pure UapItemRFS
            , fmap UapItem pItemName
            ]
        scn
        pure result

    uap = Uap . snd <$> parseList (MC.string "uap") parseOne

    pVariations = do
        (_, lst) <- parseList
            (MC.string "variations")
            (\_ -> parseList pUapName parseOne)
        pure lst

    pSelector = do
        (name, lst) <- parseList pHeader pCase
        pure $ UapSelector name lst
      where
        pHeader = MC.string "case" >> sc >> pPaths
        pCase _ = (,)
            <$> (pInt <* (MC.char ':' >> sc))
            <*> fmap (UapName . T.pack) pLine

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

-- | Parse 'asterix category'.
pCat :: Text -> Parser CatNum
pCat prefix = do
    MC.string prefix >> sc
    (a,b,c) <- (,,) <$> digitChar <*> digitChar <*> digitChar
    pure $ CatNum $ read [a,b,c]

-- | Parse 'edition'.
pEdition :: Parser Edition
pEdition = do
    MC.string "edition" >> sc
    (a,_,b) <- (,,) <$> L.decimal <*> MC.char '.' <*> L.decimal
    pure $ Edition a b

-- | Parse 'date'.
pDate :: Parser Date
pDate = do
    MC.string "date" >> sc
    Date <$> L.decimal <*> (MC.char '-' *> L.decimal ) <*> (MC.char '-' *> L.decimal )

-- | Parse 'preamble'.
pPreamble :: Parser Text
pPreamble = T.pack <$> blockAfter "preamble"

-- | Parse basic category description.
pBasic :: Parser Basic
pBasic = Basic
    <$> pCat "asterix"
    <*> (scn >> (Title . T.pack <$> stringLiteral))
    <*> (scn >> pEdition)
    <*> (scn >> pDate)
    <*> optional (try (scn >> pPreamble))
    <*> (scn >> pItems)
    <*> (scn >> (pUap <* scn))

-- | Parse 'compound' item for expansion
pCompoundExp :: Parser (Maybe ByteSize, [Maybe (NonSpare ())])
pCompoundExp = parseList pHeader pListElement
  where
    pHeader = try pFx
        <|> (MC.string "compound" >> sc >> fmap (Just . ByteSize) L.decimal)
    pListElement sc' = try pDash <|> (Just <$> pNonSpare sc')
    pDash = MC.char '-' >> pure Nothing
    pFx = MC.string "compound fx" >> pure Nothing

-- | Parse expansion
pExpansion :: Parser Expansion
pExpansion = do
    cat <- pCat "ref"
    title <- scn >> (Title . T.pack <$> stringLiteral)
    edition <- scn >> pEdition
    date <- scn >> pDate
    (fspecSize, items) <- scn >> pCompoundExp
    scn
    pure $ Expansion cat title edition date fspecSize items

-- | Parse asterix.
pAsterix :: Parser Asterix
pAsterix = tryOne
    [ AsterixBasic <$> pBasic
    , AsterixExpansion <$> pExpansion
    ]

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
            StringICAO  -> "icao"
            StringOctal -> "octal"
        ContentInteger signedness constraints -> do
            let sig = toLower <$> show signedness
                cst = case constraints of
                    []  -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            fmt (F.string % " integer" % stext) sig cst
        ContentQuantity signedness lsb (Unit unit) constraints -> do
            let sig = toLower <$> show signedness
                cst = case constraints of
                    []  -> ""
                    lst -> " " <> T.intercalate " " (fmap showConstrain lst)
            fmt
                (F.string % " quantity " % stext % " " % "\"" % stext % "\"" % stext )
                sig (showNumber lsb) unit cst
        ContentBds bt -> case bt of
            BdsWithAddress -> "bds"
            BdsAt mAddr -> case mAddr of
                Nothing             -> "bds ?"
                Just (BdsAddr addr) -> fmt ("bds " % hex) addr

instance MkBlock a => MkBlock (Rule a) where
    mkBlock = \case
        ContextFree value -> mkBlock value
        Dependent items dv lst -> do
            fmt ("case " % stext) $ case items of
                [item] -> showPath item
                _ -> "(" <> mconcat (intersperse ", " $ fmap showPath items) <> ")"
            indent $ forM_ lst $ \(values, a) -> do
                fmt (stext % ":") $ case values of
                    [value] -> sformat int value
                    _ -> sformat ("(" % stext % ")") $ mconcat
                        (intersperse ", " $ fmap (sformat int) values)
                indent $ mkBlock a
            indent $ do
                "default:"
                indent $ mkBlock dv

instance MkBlock (Variation ()) where
    mkBlock = \case
        Element () (BitSize n) rule -> do
            fmt ("element " % int) n
            indent $ mkBlock rule

        Group () lst -> do
            line "group"
            indent $ mapM_ mkBlock lst

        Extended lst -> do
            line "extended"
            indent $ forM_ lst $ \case
                Nothing -> "-"
                Just item -> mkBlock item

        Repetitive rep variation -> do
            case rep of
                RepetitiveRegular (ByteSize n) -> fmt ("repetitive " % int) n
                RepetitiveFx                   -> fmt "repetitive fx"
            indent $ mkBlock variation

        Explicit mt -> do
            case mt of
                Nothing -> "explicit"
                Just val -> case val of
                    ReservedExpansion -> "explicit re"
                    SpecialPurpose    -> "explicit sp"

        Compound lst -> do
            "compound"
            indent $ forM_ lst $ \case
                Nothing -> "-"
                Just item -> mkBlock item

instance MkBlock (NonSpare ()) where
    mkBlock (NonSpare (ItemName name) (Title title) rule doc) = do
            fmt (stext % " \"" % stext % "\"") name title
            indent $ dumpText "definition" (docDefinition doc)
            indent $ dumpText "description" (docDescription doc)
            indent $ do
                mkBlock rule
                dumpText "remark" (docRemark doc)
      where
        dumpText title' = \case
            Nothing -> mempty
            Just t -> do
                line title'
                indent $ forM_ (T.lines t) $ \i -> do
                    fmt stext i

instance MkBlock (Item ()) where
    mkBlock = \case
        Spare () (BitSize n) -> fmt ("spare " % int) n
        Item nsp -> mkBlock nsp

instance MkBlock (Uap [UapItem ItemName]) where
    mkBlock = \case
        Uap lst -> do
            line "uap"
            indent $ mapM_ dumpUapItem lst
        Uaps variations msel -> do
            line "uaps"
            indent $ do
                line "variations"
                forM_ variations $ \(UapName name, lst) -> do
                    indent $ do
                        fmt stext name
                        indent $ mapM_ dumpUapItem lst
                case msel of
                    Nothing -> mempty
                    Just sel -> do
                        fmt ("case " % stext) (showPath $ selItem sel)
                        indent $ forM_ (selTable sel) $ \(x, UapName uapName) -> do
                            fmt (int % ": " % stext) x uapName
      where
        dumpUapItem = \case
            UapItem (ItemName item) -> fmt stext item
            UapItemSpare -> "-"
            UapItemRFS -> "rfs"

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
        ""
        line "items"
        ""
        indent $ blocksLn (mkBlock <$> basCatalogue basic)
        ""
        mkBlock $ basUap basic
      where
        blocksLn lst = mconcat $ intersperse "" lst
        CatNum cat = basCategory basic
        Title title = basTitle basic
        edition = basEdition basic
        ed1 = editionMajor edition
        ed2 = editionMinor edition
        (Date year month day) = basDate basic

instance MkBlock Expansion where
    mkBlock (Expansion (CatNum cat) (Title title) edition date mn items) = do
        fmt ("ref " % left 3 '0' % " \"" % stext % "\"") cat title
        fmt ("edition " % int % "." % int) ed1 ed2
        fmt ("date " % int % "-" % left 2 '0' % "-" % left 2 '0') year month day
        ""
        case mn of
            Nothing                    -> "compound fx"
            Just (ByteSize fspecBytes) -> fmt ("compound " % int) fspecBytes
        indent $ forM_ items $ \case
            Nothing -> "-"
            Just item -> mkBlock item
      where
        Edition ed1 ed2 = edition
        Date year month day = date

instance MkBlock Asterix where
    mkBlock = \case
        AsterixBasic basic -> mkBlock basic
        AsterixExpansion expansion -> mkBlock expansion

coder :: Coder
coder = Coder
    { cDescription = "Human readable format"
    , cDecoder = Just decoder
    , cEncoder = Just encoder
    }
  where
    decoder filename s = first errorBundlePretty $ parse (pAsterix <* eof) filename s
    encoder = render "    " "\n" . mkBlock
