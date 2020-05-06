
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Data.Asterix.Validation
-- Copyright:   (c) 2019 Zoran Bošnjak
--              (c) 2019 Sloveniacontrol Ltd. (www.sloveniacontrol.si)
-- License:     GPL-3
-- Maintainer:  Zoran Bošnjak <zoran.bosnjak@sloveniacontrol.si>
--
-- This module defines validates asterix data types.
--

module Data.Asterix.Validation
( validate
) where

import           Control.Monad
import qualified Data.Text as T
import           Data.Maybe
import           Data.List

import           Data.Asterix

type ValidationError = T.Text

fixedSubitemSize :: Asterix -> [Name] -> Maybe RegisterSize
fixedSubitemSize asterix path = findSubitemByName asterix path >>= \case
    Spare _ -> Nothing
    Subitem _ _ _ element _ -> case element of
        Fixed size _ -> Just size
        _ -> Nothing

reportWhen :: Bool -> [Name] -> ValidationError -> [ValidationError]
reportWhen False _ _ = []
reportWhen True path msg = [mconcat [showPath path, " -> ", msg]]

validate :: Asterix -> [ValidationError]
validate asterix = join
    [ allItemsDefined
    , join (validateEncoding <$> astCatalogue asterix)
    , join $ (snd . validateSubitem asterix [] . itemSubitem <$> astCatalogue asterix)
    , validateUap
    ]
  where

    validateEncoding :: Item -> [ValidationError]
    validateEncoding item = case itemEncoding item of
        Unspecified -> []
        ContextFree encoding -> reportWhen (encoding == Absent) [itemName] "Item can not be 'absent'"
        Dependent someSubitem rules -> case fixedSubitemSize asterix someSubitem of
            Nothing -> reportWhen True [itemName] (showPath someSubitem <> " not defined")
            Just m ->
                let size = compare (length rules) (2 ^ m)
                in reportWhen (size == GT) [itemName] "too many encoding variations"
      where
        itemName = case itemSubitem item of
            Spare _ -> ""
            Subitem name _ _ _ _ -> name

    validateUap :: [ValidationError]
    validateUap = case astUap asterix of
        Uap lst -> validateList ["uap"] lst
        Uaps lst -> join
            [ reportWhen (length lst /= 2) ["uap"] "expecting 2 uap variations"
            , do
                let dupNames = nub x /= x where x = fst <$> lst
                reportWhen dupNames ["uap"] "duplicated names"
            , do
                (uapName, lst') <- lst
                validateList [uapName] lst'
            ]
      where
        validateList path lst =
            let x = catMaybes lst
            in reportWhen (nub x /= x) path "duplicated items"

    allItemsDefined :: [ValidationError]
    allItemsDefined = join [requiredNotDefined, definedNotRequired, noDups]
      where
        required :: [Name]
        required = catMaybes $ case astUap asterix of
            Uap lst -> lst
            Uaps lst -> nub $ join $ fmap snd lst

        defined :: [Name]
        defined = (itemSubitem <$> astCatalogue asterix) >>= \case
            Spare _ -> []
            Subitem name _ _ _ _ -> [name]

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
        noDups =
            let dups = defined \\ (nub defined)
            in reportWhen (not $ null dups) ["items"]
                ("duplicate names: " <> T.pack (show dups))

validateSubitem :: Asterix -> [Name] -> Subitem -> (Int, [ValidationError])
validateSubitem asterix path = \case
    Spare n -> (n, reportWhen (n <= 0) (path++["spare"]) "size")
    Subitem name _ _ element _ -> validateElement asterix (path++[name]) element

validateElement :: Asterix -> [Name] -> Element -> (Int, [ValidationError])
validateElement asterix path = \case
    Fixed n content -> (n, join
        [ reportWhen (n <= 0) path "size"
        , validateContent asterix path n content
        ])
    Group lst -> checkSubitems lst
    Extended n1 n2 lst -> loop (0, join [check n1, check n2]) fxPositions lst where
        check n = reportWhen (mod n 8 /= 0) path ("extended size: " <> T.pack (show n))
        fxPositions = tail (sum <$> inits (n1:repeat n2))
        loop (n,problems) fx = \case
            [] -> (n, join
                [ problems
                , reportWhen ((mod n 8) /= 0) path "not aligned"
                , reportWhen (dupNames lst) path "duplicated names"
                ])
            (i:is) ->
                let (a,b) = (head fx, tail fx)
                    (n', problems') = validateSubitem asterix path i
                in case compare (n+n'+1) a of
                    LT -> loop (n+n', problems'++problems) (a:b) is
                    EQ -> loop (n+n'+1, problems'++problems) b is
                    GT -> loop (n+n', problems' ++ problems ++ reportWhen True path "overflow") b is
    Repetitive rep subElement ->
        let (n, problems) = validateElement asterix path subElement
        in (8*rep+n, problems)
    Explicit -> (0, [])
    Compound lst -> checkSubitems $ catMaybes lst
    Rfs -> (0, [])
  where
    dupNames lst =
        let getName = \case
                Spare _ -> Nothing
                Subitem name' _ _ _ _ -> Just name'
            names = catMaybes (fmap getName lst)
        in names /= nub names
    checkSubitems lst =
        let result = fmap (validateSubitem asterix path) lst
            n = sum (fst <$> result)
        in (n, join
            [ mconcat (snd <$> result)
            , reportWhen (null lst) path "empty"
            , reportWhen ((mod n 8) /= 0) path "size reminder error"
            , reportWhen (dupNames lst) path "duplicated names"
            ])

validateContent :: Asterix -> [Name] -> Int -> Rule Content -> [ValidationError]
validateContent asterix path n = \case
    Unspecified -> []
    ContextFree rule -> validateRule rule
    Dependent someSubitem rules -> join
        [ case fixedSubitemSize asterix someSubitem of
            Nothing -> reportWhen True path (showPath someSubitem <> " not defined")
            Just m ->
                let size = compare (length rules) (2 ^ m)
                in reportWhen (size == GT) path "too many variations"
        , do
            let keys = fst <$> rules
            reportWhen (keys /= nub keys) path "duplicated keys"
        , join (validateRule . snd <$> rules )
        ]
  where
    validateRule = \case
        ContentTable lst ->
            let keys = fst <$> lst
                values = snd <$> lst
                size = compare (length keys) (2 ^ n)
            in join
                [ reportWhen (keys /= nub keys) path "duplicated keys"
                , reportWhen (size == GT) path "table too big"
                , reportWhen (any T.null values) path "empty value"
                ]
        _ -> []

