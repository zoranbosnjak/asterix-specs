{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Pandoc where

#ifndef NOPANDOC
import           Data.Coerce
import           Data.List           (intersperse)
import           Data.Scientific
import           Data.Text           (Text)
import           Formatting          as F
import           Text.Pandoc
import           Text.Pandoc.Builder as PB

import           Asterix.Specs

fromRST :: Text -> Blocks
fromRST s = fromList val
  where
    pureVal = readRST def s
    Pandoc _meta val = either (error . show) id (runPure pureVal)

class ToPandoc a where
    tp :: [Text] -> a -> Blocks

instance ToPandoc Constrain where
    tp _p = \case
        EqualTo n -> go "==" n
        NotEqualTo n -> go "!=" n
        GreaterThan n -> go ">" n
        GreaterThanOrEqualTo n -> go ">=" n
        LessThan n -> go "<" n
        LessThanOrEqualTo n -> go "<=" n
      where
        f :: Number -> Inlines
        f = math . sformat (scifmt Generic Nothing) . evalNumber

        go :: Text -> Number -> Blocks
        go s n = para (code s <> " " <> f n)

instance ToPandoc Content where
    tp p = \case
        ContentRaw -> para $ str "Raw Content"
        ContentTable lst ->
            para (str "Values:")
         <> blockQuote (definitionList $ do
                (i, t) <- lst
                pure (code (sformat int i) <> str ":", [plain $ str t])
            )
        ContentString t -> para $ str $ case t of
            StringAscii -> "Ascii string (8-bits per char)"
            StringICAO  -> "ICAO string (6-bits per char)"
            StringOctal -> "Octal string (3-bits per char)"
        ContentInteger sig cstr -> mconcat
            [ para $ str $ case sig of
                Signed   -> "Signed integer"
                Unsigned -> "Unsigned integer"
            , mconcat $ fmap (tp p) cstr
            ]
        ContentQuantity sig lsb (Unit unit) cstr -> mconcat
            [ para $ str $ case sig of
                Signed   -> "Signed quantity"
                Unsigned -> "Unsigned quantity"
            , let lsb' = sformat (scifmt Generic Nothing) (evalNumber lsb)
              in para (str "LSB = "
                <> math (showNumber lsb) <> " " <> str unit
                <> " ≈ " <> math lsb' <> " " <> str unit
            )
            , para $ str $ sformat ("unit: \"" % stext % "\"") unit
            , mconcat $ fmap (tp p) cstr
            ]
        ContentBds t -> para $ str $ case t of
            BdsWithAddress -> "BDS register with address"
            BdsAt Nothing -> "BDS register at unknown address"
            BdsAt (Just (BdsAddr x)) ->
                sformat ("BDS register at address " % int) x

instance ToPandoc a => ToPandoc (Rule a) where
    tp p = \case
        ContextFree a -> tp p a
        Dependent p' dv cases ->
            let f1 :: [ItemPath] -> Inlines
                f1 lst
                    = "("
                   <> mconcat (intersperse ", " (fmap f1' lst))
                   <> ")"

                f1' :: ItemPath -> Inlines
                f1' (ItemPath p'') = code $ mconcat
                    (intersperse "/" (fmap coerce p''))

                f2 :: ([Int], a) -> (Inlines, [Blocks])
                f2 (xs', t) =
                    let f = sformat int
                        xs = mconcat $ intersperse ", " (fmap f xs')
                    in ("(" <> code xs <> "):", [tp p t])
            in
                para (str "Depending on: " <> f1 p')
             <> blockQuote
                  ( definitionList (fmap f2 cases)
                 <> para (str "Default:")
                 <> blockQuote (tp p dv)
                  )

instance ToPandoc (Variation ()) where
    tp p = \case
        Element _ (BitSize n) rule -> blockQuote $ mconcat
            [ para $ str "Element"
            , para (str $ sformat ("bit size: " % int) n)
            , tp p rule
            ]
        Group _ lst -> mconcat
            [ para $ str "Group"
            , blockQuote $ mconcat $ fmap (tp p) lst
            ]
        Extended lst -> mconcat
            [ para $ str "Extended"
            , blockQuote $ mconcat $ fmap f lst
            ]
          where
            f = \case
                Nothing -> para $ emph "(FX) - extension bit"
                Just i -> tp p i
        Repetitive t var -> mconcat
            [ para $ str "Repetitive"
            , para $ str $ case t of
                RepetitiveRegular (ByteSize n) ->
                    sformat ("Regular, " % int % " byte(s) REP field size.") n
                RepetitiveFx -> "With FX extension bit."
            , tp p var
            ]
        Explicit mt -> para $ str $ case mt of
            Nothing                -> "Explicit"
            Just ReservedExpansion -> "Explicit (ReservedExpansion)"
            Just SpecialPurpose    -> "Explicit (SpecialPurpose)"
        Compound lst -> mconcat
            [ para $ str "Compound"
            , blockQuote $ mconcat $ fmap f lst
            ]
          where
            f = \case
                Nothing -> para $ emph "Spare"
                Just i -> tp p i

instance ToPandoc (Item ()) where
    tp p = \case
        Spare _o (BitSize n) -> para $ str $ sformat ("Spare bits: " % int) n
        Item x -> tp p x

instance ToPandoc (NonSpare ()) where
    tp p' (NonSpare name title' rule doc')
        = para (strong $ str title)
       <> blockQuote (mconcat
            [ case definition of
                Nothing -> mempty
                Just t  -> para $ str $ sformat ("definition: " % stext) t
            , case description of
                Nothing -> mempty
                Just t  -> para $ str $ sformat ("description: " % stext) t
            , tp p'' rule
            , maybe mempty fromRST mRemark
            ])
      where
        p'' = p' <> [coerce name]
        p = mconcat (intersperse "/" p'')
        title'' = coerce title'
        title
            | title'' == "" = p
            | otherwise = sformat (stext % " - " % stext) p title''
        Documentation definition description mRemark = doc'

fillToMultipleOf :: Int -> a -> [a] -> [a]
fillToMultipleOf n x lst = lst <> replicate cnt x
  where
    m = mod (length lst) n
    cnt
        | m == 0 = 0
        | otherwise = n - m

groupAt :: Int -> [a] -> [[a]]
groupAt n lst
    | length lst <= n = [lst]
    | otherwise =
        let (a, b) = splitAt n lst
        in a : groupAt n b

instance ToPandoc ([NonSpare ()], Uap [UapItem ItemName]) where
    tp p' (catalogue, uap) = case uap of
        Uap lst -> go lst
        Uaps lsts msel -> mconcat (
            [ para $ str "This category has multiple UAPs."
            , case msel of
                Nothing -> para $ str "UAP selector is not known"
                Just (UapSelector (ItemPath i) tab) ->
                    let s = sformat "UAP selection is based on the value of: "
                        p'' = mconcat (intersperse "/" (coerce i))
                        cs (val, UapName name)
                            = plain (code $ sformat int val)
                           <> plain (str name)
                    in
                        para (str s <> code p'')
                     <> bulletList (fmap cs tab)
            ] <> fmap f lsts)

      where
        f :: (UapName, [UapItem ItemName]) -> Blocks
        f (UapName uapName, lst) = header 3 (str uapName) <> go lst

        titles = do
            NonSpare (ItemName name) (Title title) _rule _doc <- catalogue
            pure (name, title)

        getTitle name = case lookup name titles of
            Nothing -> ""
            Just x  -> sformat (" - " % stext) x

        go :: [UapItem ItemName] -> Blocks
        go
            = bulletList
            . mconcat
            . fmap go1
            . groupAt 7
            . zip [1..]
            . fillToMultipleOf 7 UapItemSpare

        go1 :: [(Int, UapItem ItemName)] -> [Blocks]
        go1 lst
            = fmap go2 lst
           <> [ plain $ str "(FX) - Field extension indicator" ]

        go2 :: (Int, UapItem ItemName) -> Blocks
        go2 (i, x) = plain $ case x of
            UapItem (ItemName name) ->
                let p = mconcat (intersperse "/" (p' <> [name]))
                    title = getTitle name
                in
                    str (sformat (int % ": ") i)
                 <> code p
                 <> str title
            UapItemSpare -> emph $ str "Spare"
            UapItemRFS -> emph $ str "RFS indicator"

-- | Convert 'Asterix AST' to 'Pandoc AST'
toPandoc :: Asterix -> Pandoc
toPandoc = \case
    AsterixBasic val -> doc $
        header 1 (mkTitle "Asterix category" cat title)
        <> mkCategory cat
        <> mkEdition ed
        <> mkDate date
        <> header 2 "Preamble"
        <> maybe (para $ emph "None") fromRST mPreamble
        <> header 2 "Description of standard data items"
        <> mconcat (fmap (tp ["I" <> cat]) catalogue)
        <> header 2 "User Application Profile"
        <> tp ["I" <> cat] (catalogue, uap)
      where
        Basic cat' title ed date mPreamble catalogue uap = val
        cat = mkCat cat'
    AsterixExpansion val -> doc $
        header 1 (mkTitle "Asterix expansion" cat title)
        <> mkCategory cat
        <> mkEdition ed
        <> mkDate date
        <> para (strong "FSPEC byte size" <> ": " <> fspecSize mn)
        <> header 2 "Items"
        <> mconcat (fmap mkItem items)
      where
        Expansion cat' title ed date mn items = val
        fspecSize = \case
            Just n -> str (sformat int (coerce n :: Int))
            Nothing -> str "fx"
        cat = mkCat cat'
        mkItem = \case
            Nothing -> para $ emph "Spare"
            Just nsp -> tp [] nsp
  where
    mkTitle s cat title = str $
        sformat (stext % " " % stext % " - " % stext) s cat (coerce title)
    mkCat cat' = sformat (left 3 '0') (coerce cat' :: Int)
    mkCategory cat = para (strong (str "category") <> ": " <> str cat)
    mkEdition ed = para (strong (str "edition") <> ": " <> str
           (sformat (int % "." % int) (editionMajor ed) (editionMinor ed)))
    mkDate date = para (strong (str "date") <> ": " <> str
           (sformat (int % "-" % left 2 '0' % "-" % left 2 '0')
               (dateYear date) (dateMonth date) (dateDay date)))
#endif

