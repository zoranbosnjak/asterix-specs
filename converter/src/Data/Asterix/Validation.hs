
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- |
-- Module:      Data.Asterix.Validation
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines validators for asterix data types.
--

module Data.Asterix.Validation where

import           Control.Monad
import qualified Data.Text as T
import           Data.Maybe
import           Data.List

import           Data.Asterix
import           Data.Asterix.Common

type ValidationError = T.Text

-- A priori known size.
class Fixed a where
    size :: a -> Maybe Int

instance Fixed Variation where
    size (Element n _content) = Just n
    size (Group items) = sum <$> sequence (fmap size items)
    size _ = Nothing

instance Fixed Item where
    size (Spare n) = Just n
    size (Item _name _title variation _doc) = size variation

isFixed :: Fixed a => a -> Bool
isFixed = isJust . size

isCapital :: T.Text -> Bool
isCapital w
    | elem w exceptions = True
    | T.head w == '(' || T.last w == ')' = True
    | otherwise = elem (T.head w) (['A'..'Z'] <> ['0'..'9'])
  where
    exceptions =
        [ "of", "in", "by", "to", "from", "the", "for", "and", "or"
        , "with", "which", "is", "as", "/", "=", "on", "a"
        ]

-- Bit alignment property.
class IsAligned a where
    isAligned :: a -> Bool

instance IsAligned RegisterSize where
    isAligned n = (n `mod` 8) == 0

instance IsAligned Variation where
    isAligned (Element n _content) = isAligned n
    isAligned (Group lst) = check notAlignedParts where
        notAlignedParts = filter (not . isAligned) lst
        check items = case size (Group items) of
            Nothing -> False
            Just n -> isAligned n
    isAligned (Extended n1 n2 lst) = and
        [ isAligned n1
        , isAligned n2
        , loop 0 fxPositions lst
        ]
      where
        fxPositions = tail (sum <$> inits (n1:repeat n2))
        loop _n _fx [] = True
        loop _n [] _items = False
        loop n (fx:fxs) (item:items) = case size item of
            Nothing -> False
            Just n' -> case compare (n+n'+1) fx of
                LT -> loop (n+n') (fx:fxs) items
                EQ -> loop (n+n'+1) fxs items
                GT -> False
    isAligned (Repetitive repSize variation) = eachAligned || sumAligned
      where
        eachAligned = and
            [ isAligned repSize
            , isAligned variation
            ]
        sumAligned = case size variation of
            Nothing -> False
            Just n -> isAligned (repSize + n)
    isAligned Explicit = True
    isAligned (Compound mFspecSize lst) = and
        [ maybe True isAligned mFspecSize
        , all check lst
        ]
      where
        check Nothing = True
        check (Just item) = isAligned item

instance IsAligned Item where
    isAligned (Spare n) = isAligned n
    isAligned (Item _name _title variation _doc) = isAligned variation

-- Validations
class Validate a where
    validate :: Bool -> a -> [ValidationError]

isValid :: Validate a => Bool -> a -> Bool
isValid warnings = null . validate warnings

instance Validate a => Validate [a] where
    validate warnings lst = join (fmap (validate warnings) lst)

instance Validate a => Validate (Maybe a) where
    validate _warnings Nothing = []
    validate warnings (Just a) = validate warnings a

reportWhen :: Bool -> ValidationError -> [ValidationError]
reportWhen False _ = []
reportWhen True msg = [msg]

reportUnless :: Bool -> ValidationError -> [ValidationError]
reportUnless = reportWhen . not

instance Validate (RegisterSize, Content) where
    validate warnings (n, ContentTable lst) = join
        [ reportWhen (keys /= nub keys) "duplicated keys"
        , reportWhen (any T.null values) "empty value"
        , reportWhen (sizeCheck == GT) "table too big"
        , join $ do
            guard warnings
            val <- values
            return $ join $ do
                w <- take 1 $ T.words val
                guard $ not $ isCapital w
                return ["Expecting capitalized word -> " <> val <> " -> " <> w]
        , join $ do
            guard warnings
            val <- values
            return $ reportWhen (val /= "" && T.last val == '.') $
                "Unexpected dot at the end of table entry -> " <> val
        ]
      where
        keys = fst <$> lst
        values = snd <$> lst
        sizeCheck = compare (length keys) (2 ^ n)
    validate _ _ = []

instance Validate (RegisterSize, a) => Validate (RegisterSize, Rule a) where
    validate _warnings (_n, Unspecified) = []
    validate warnings (n, ContextFree a) = validate warnings (n,a)
    validate warnings (n, Dependent _someItem rules) = join
        [ reportWhen (keys /= nub keys) "duplicated keys"
        , join $ do
            rule <- fmap snd rules
            return $ validate warnings (n, rule)
        ]
      where
        keys = fst <$> rules

duplicatedNames :: [Item] -> Bool
duplicatedNames items = names /= nub names
  where
    names = items >>= \case
        Spare _ -> []
        Item name _title _variation _doc -> [name]

instance Validate Variation where
    validate warnings (Element n content) = join
        [ reportUnless (n > 0) "element size"
        , validate warnings (n, content)
        ]
    validate warnings x@(Group items) = join
        [ reportUnless (isAligned x) "bit alignment"
        , validate warnings items
        , reportWhen (warnings && length items <= 1) "single item in a group"
        , reportWhen (duplicatedNames items) "duplicated names"
        ]
    validate warnings x@(Extended _n1 _n2 items) = join
        [ reportUnless (isAligned x) "bit alignment"
        , join $ do
            item <- items
            return $ maybe ["item size not fixed"] (const []) (size item)
        , reportWhen (duplicatedNames items) "duplicated names"
        , validate warnings items
        ]
    validate warnings (Repetitive m variation) = join
        [ reportUnless (m > 0) "REP size"
        , reportUnless (isAligned m) "REP alignment"
        , reportUnless (isAligned variation) "variation alignment"
        , validate warnings variation
        ]
    validate _warnings Explicit = []
    validate warnings x@(Compound _mFspecSize items) = join
        [ reportUnless (isAligned x) "alignment error"
        , validate warnings items
        , let items' = catMaybes items
          in reportWhen (duplicatedNames items') "duplicated names"
        ]

instance Validate Item where
    validate _warnings (Spare n) = reportUnless (n > 0) "size error"
    validate warnings (Item name title variation _doc) = join
        -- check item name length
        [ reportWhen (T.length name > 15) (name <> ":Item name too long")
        -- item name valid characters
        , do
            c <- T.unpack name
            guard $ not $ elem c (['A'..'Z'] <> ['0'..'9'])
            return $ name <> ":Invalid character " <> T.pack (show c)
        -- capitalized title
        , do
            guard warnings
            w <- T.words title
            guard $ not $ isCapital w
            return $ name <> ":Title not capitalized -> " <> title <> " -> " <> w
        -- no dot at the end of title
        , reportWhen (warnings && title /= "" && T.last title == '.') $
            name <> ":Unexpected dot at the end of title -> " <> title
        -- check variation
        , validateVariation
        ]
      where
        validateVariation = do
            err <- validate warnings variation
            return (name <> ":" <> err)

instance Validate Basic where
    validate warnings basic = join
        [ validateCat
        , join (validate warnings <$> basCatalogue basic)
        , allItemsDefined
        , validateUap
        , join (validateDepItem <$> basCatalogue basic)
        ]
      where
        validateCat :: [ValidationError]
        validateCat = reportUnless (basCategory basic `elem` [0..255])
            "category number out of range"

        allItemsDefined :: [ValidationError]
        allItemsDefined = join [requiredNotDefined, definedNotRequired, noDups]
          where
            required :: [Name]
            required = catMaybes $ case basUap basic of
                Uap lst -> lst
                Uaps lst -> nub $ join $ fmap snd lst

            defined :: [Name]
            defined = basCatalogue basic >>= \case
                Spare _n -> []
                Item name _title _variation _doc -> [name]

            requiredNotDefined :: [ValidationError]
            requiredNotDefined = do
                x <- required
                guard $ x `notElem` defined
                return (T.pack (show x) <> " required, but not defined.")

            definedNotRequired :: [ValidationError]
            definedNotRequired = do
                x <- defined
                guard $ x `notElem` required
                return (T.pack (show x) <> " defined, but not required.")

            noDups :: [ValidationError]
            noDups = reportWhen (not $ null dups)
                ("duplicate names: " <> T.pack (show dups))
              where
                dups = defined \\ (nub defined)

        validateUap :: [ValidationError]
        validateUap = case basUap basic of
            Uap lst -> validateList lst
            Uaps lst -> join
                [ do
                    let dupNames = nub x /= x where x = fst <$> lst
                    reportWhen dupNames "duplicated UAP names"
                , do
                    (uapName, lst') <- lst
                    do
                        x <- validateList lst'
                        return (uapName <> ":" <> x)
                ]
          where
            validateList lst =
                let x = catMaybes lst
                in reportWhen (nub x /= x) "duplicated items in UAP"

        validateDepItem :: Item -> [ValidationError]
        validateDepItem (Spare _n) = []
        validateDepItem (Item name title variation doc) = case variation of
            Element _n rule -> case rule of
                Unspecified -> []
                ContextFree _ -> []
                Dependent someItemName rules -> case findItemByName basic someItemName of
                    Nothing -> [showPath someItemName <> " not defined"]
                    Just someItem -> case size someItem of
                        Nothing -> [showPath someItemName <> " unknown size"]
                        Just m ->
                            let ln = compare (length rules) (2 ^ m)
                            in reportWhen (ln == GT) (showPath [name] <> " too many variations")
            Group items ->
                (join $ fmap validateDepItem items)
                ++ (join $ fmap validateNestedName items)
              where
                validateNestedName = \case
                    Spare _ -> []
                    Item subName _title _var _doc -> do
                        let n = T.length name
                            subName' = T.take n subName
                        guard $ subName /= name
                        guard $ subName' == name
                        [showPath [name, subName]
                            <> ": name repetition, suggesting -> "
                            <> showPath [name, T.drop n subName]]

            Extended _n1 _n2 items -> join $ fmap validateDepItem items
            Repetitive _n variation' -> validateDepItem (Item name title variation' doc)
            Explicit -> []
            Compound _mFspecSize lst -> join (fmap validateDepItem $ catMaybes lst)

instance Validate Expansion where
    validate warnings x = join
        [ validate warnings $ expVariation x
        ]

instance Validate Asterix where
    validate warnings = \case
        AsterixBasic x -> validate warnings x
        AsterixExpansion x -> validate warnings x

