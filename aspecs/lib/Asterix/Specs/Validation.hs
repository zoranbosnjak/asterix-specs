-- | Validators for asterix data types.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterix.Specs.Validation where

import           Control.Monad
import           Control.Monad.Trans.Writer

import           Data.Foldable
import           Data.List                  (nub, (\\))
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Asterix.Specs

type ValidationError = T.Text

newtype ErrM a = ErrM { unErrM :: Writer [ValidationError] a }
    deriving (Functor, Applicative, Monad)

instance Semigroup a => Semigroup (ErrM a) where
    act1 <> act2 = (<>) <$> act1 <*> act2

instance Monoid a => Monoid (ErrM a) where
    mempty = pure mempty

instance a ~ () => IsString (ErrM a) where
    fromString s = ErrM $ do
        tell [fromString s]

runErrM :: ErrM a -> [ValidationError]
runErrM = execWriter . unErrM

class Validate a where
    validate :: a -> ErrM ()

throw :: Text -> ErrM ()
throw = ErrM . tell . pure

withPreffix :: Text -> ErrM () -> ErrM ()
withPreffix name act = forM_ (runErrM act) $ \err -> do
    throw $ name <> ":" <> err

isCapital :: T.Text -> Bool
isCapital w
    | w `elem` exceptions = True
    | otherwise = T.head w `notElem` ['a'..'z']
  where
    exceptions =
        [ "of", "in", "by", "to", "from", "the", "for", "and", "or"
        , "with", "which", "is", "as", "on", "a", "vs."
        ]

instance Validate (Signedness, Constrain) where
    validate = \case
        (Signed, _) -> pure ()
        (Unsigned, cstr) -> do
            when (isNegative $ getConstrainNumber cstr)
                "'unsigned' content, negative constrain"

instance Validate (BitSize, Content) where
    validate (_n, ContentRaw) = pure ()
    validate (BitSize n, ContentTable lst) = do
        when (keys /= nub keys) "duplicated keys"
        when (any T.null values) "empty value"
        when (sizeCheck == GT) "table too big"
        forM_ values $ \val -> do
            forM_ (take 1 $ T.words val) $ \w -> do
                unless (isCapital w) $ do
                    throw $ "Expecting capitalized word -> " <> val <> " -> " <> w
            when (val /= "" && T.last val == '.') $ do
                throw $ "Unexpected dot at the end of table entry -> " <> val
            when ('"' `elem` T.unpack val) $ do
                throw $ "Value contain quotes -> " <> val
      where
        keys = fst <$> lst
        values = snd <$> lst
        sizeCheck = compare (length keys) (2 ^ n)
    validate (BitSize stringSize, ContentString stringType) =
        when (stringSize `mod` charSize /= 0) $ do
            throw $
                T.pack $ show stringType <> " (" <> show charSize
                <> " bits per symbol) does not fit in parent element ("
                <> show stringSize <> " bits)"
      where
        charSize = case stringType of
            StringAscii -> 8
            StringICAO  -> 6
            StringOctal -> 3
    validate (_, ContentInteger sign lst) = forM_ lst $ \cstr -> do
        validate (sign, cstr)
    validate (_, ContentQuantity sign _k _unit lst) = forM_ lst $ \cstr -> do
        validate (sign, cstr)
    validate (BitSize n, ContentBds bt) = do
        let expected = case bt of
                BdsWithAddress -> 64
                BdsAt _mAddr   -> 56
        when (n /= expected) "unexpected BDS register length"
        case bt of
            BdsAt (Just (BdsAddr addr)) -> do
                when (addr < 0 || addr > 255) "BDS address out of range"
            _ -> pure ()

instance Validate (BitSize, Rule Content) where
    validate (n, ContextFree a) = validate (n, a)
    validate (n, Dependent _someItem dv lst) = do
        when (keys /= nub keys) "duplicated keys"
        validate (n, dv)
        forM_ (fmap snd lst) $ \rule -> do
            validate (n, rule)
      where
        keys = fst <$> lst

instance Validate (Rule (Variation a)) where
    validate = \case
        ContextFree x -> validate x
        Dependent _item dv lst -> do
            validate dv
            mapM_ (validate . snd) lst
            when (keys /= nub keys) "duplicated keys"
          where
            keys = fmap fst lst

itemNames :: [Item offset] -> [ItemName]
itemNames = mapMaybe f where
    f = \case
        Spare _ _ -> Nothing
        Item (NonSpare name _ _ _) -> Just name

instance Validate (Variation a) where
    validate (Element _ (BitSize n) rule) = do
        when (n <= 0) "element size"
        validate (BitSize n, rule)
    validate (Group _ lst) = do
        mapM_ validate lst
        when (length lst <= 1) "group requires more items"
        when (itemNames lst /= nub (itemNames lst)) "duplicated names"
    validate (Extended items) = do
        when (offset (Extended items) /= mempty) "alignment error"
        when (itemNames lst /= nub (itemNames lst)) "duplicated names"
        when (length items <= 1) "extended subitem list size"
        mapM_ validate lst
      where
        lst = catMaybes items
    validate (Repetitive rt variation) = case rt of
        RepetitiveRegular (ByteSize m) -> do
            when (m <= 0) "REP size"
            when (offset variation /= mempty) "variation alignment"
            validate variation
        RepetitiveFx -> validate variation
    validate (Explicit _) = pure ()
    validate (Compound items) = do
        when (offset (Compound items) /= mempty) "alignment error"
        mapM_ validate lst
        when (itemNames lst' /= nub (itemNames lst')) "duplicated names"
        when (isNothing $ last items) "last element in compound is empty"
      where
        lst = catMaybes items
        lst' = fmap Item lst

instance Validate (NonSpare a) where
    validate (NonSpare (ItemName name) (Title title) rule _doc) = withPreffix name $ do
        when (T.length name > 15) "Item name too long"
        forM_ (T.unpack name) $ \c -> do
            unless (c `elem` (['A'..'Z'] <> ['0'..'9'])) $ do
                throw ("Invalid character " <> T.pack (show c))
        forM_ (T.words title) $ \w -> do
            unless (isCapital w) $ do
                throw $ "Title not capitalized -> " <> title <> " -> " <> w
        when (title /= "" && T.last title == '.') $ do
            throw $ "Unexpected dot at the end of title -> " <> title
        when (T.strip title /= title) $ do
            throw $ "Title contain leading or trailing whitespaces -> " <> title
        when ('"' `elem` T.unpack title) $ do
            throw $ "Title contain quotes -> " <> title
        mapM_ validate rule

instance Validate (Item a) where
    validate (Spare _ (BitSize n)) = do
        when (n <= 0) "size error"
    validate (Item nsp) = validate nsp

instance Validate CatNum where
    validate (CatNum n) = do
        unless (n `elem` [0..255])
            "category number out of range"

instance Validate Edition where
    validate (Edition a b) = do
        when (a < 0 || b < 0) "editon negative number"

instance Validate Date where
    validate (Date a b c) = do
        when (a < 0 || b < 0 || c < 0) "date negative number"

catUapItems :: [UapItem a] -> [a]
catUapItems = mapMaybe f where
    f = \case
        UapItem name -> Just name
        _ -> Nothing

instance Validate ([NonSpare ()], Uap [UapItem ItemName]) where
    validate (catalogue, uap) = case uap of
        Uap lst -> validateList lst
        Uaps lst1 msel -> do
            do
                let x = fmap fst lst1
                    dupNames = nub x /= x
                when dupNames "duplicated UAP names"
            forM_ lst1 $ \(UapName uapName, lst2) -> withPreffix uapName $ do
                validateList lst2
            case msel of
                Nothing -> pure ()
                Just (UapSelector name table) -> do
                    case findItemByName catalogue name of
                        Nothing -> throw $ showPath name <> " not defined"
                        Just i -> case bitSize i of
                            Nothing -> throw $ showPath name <> " unknown size"
                            Just m -> do
                                let ln = compare (length table) (2 ^ m)
                                when (ln == GT) "too many variations"
                    forM_ (fmap snd table) $ \uapName -> do
                        unless (uapName `elem` fmap fst lst1) $ do
                            let UapName x = uapName
                            throw $ "unknown uap: " <> x
      where
        isUapSpare = \case
            UapItemSpare -> True
            _ -> False
        validateList lst = do
            do
                let x = catUapItems lst
                when (nub x /= x) "duplicated items in UAP"
            when (isUapSpare $ last lst)
                "spare at the end of UAP is redundant"

instance Validate Basic where
    validate (Basic cat _title edition date _preamble catalogue uap) = do
        validate cat
        validate edition
        validate date
        forM_ catalogue $ \item -> do
            when (offset item /= mempty) "top level alignment error"
            validate item
            validateDepItem item
        forM_ required $ \name -> do
            unless (name `elem` defined) $ do
                throw $ T.pack (show name) <> " required, but not defined."
        forM_ defined $ \name -> do
            unless (name `elem` required) $ do
                throw $ T.pack (show name) <> " defined, but not required."
        do
            let dups = defined \\ nub defined
            unless (null dups) $ do
                throw $ "duplicate names: " <> T.pack (show dups)
        validate (catalogue, uap)
      where
        required :: [ItemName]
        required = catUapItems $ case uap of
            Uap lst        -> lst
            Uaps lst _msel -> nub (snd =<< lst)

        defined :: [ItemName]
        defined = [name | NonSpare name _title _variation _doc <- catalogue]

        validateDepItem :: NonSpare () -> ErrM ()
        validateDepItem (NonSpare name title rule doc) = forM_ (toList rule) $ \case
            Element _ _n rule' -> case rule' of
                ContextFree _ -> pure ()
                Dependent items _dv lst -> do
                    when (null lst) "empty case list"
                    forM_ (fmap fst lst) $ \xs -> do
                        case length items == length xs of
                            False -> "Case vector size mismatch"
                            True -> forM_ (zip items xs) $ \(someItemName, x) -> do
                                case findItemByName catalogue someItemName of
                                    Nothing -> throw $ showPath someItemName <> " not defined"
                                    Just someItem -> case bitSize someItem of
                                        Nothing -> throw $ showPath someItemName <> " unknown size"
                                        Just m -> do
                                            when (x > (2^m)) $ do
                                                throw $ showPath (ItemPath [name]) <> " too many cases"
            Group _ items -> do
                forM_ items $ \case
                    Spare _ _ -> pure ()
                    Item item@(NonSpare subName _title _var _doc) -> do
                        validateDepItem item
                        let (ItemName name') = name
                            (ItemName subName') = subName
                            n = T.length name'
                            subName'' = T.take n subName'
                        when (subName' /= name' && subName'' == name') $ do
                            throw $
                                showPath (ItemPath [name, subName])
                                <> ": name repetition, suggesting -> "
                                <> showPath (ItemPath [name, ItemName (T.drop n subName')])
            Extended lst -> forM_ (catMaybes lst) $ \case
                Spare _ _ -> pure ()
                Item nsp -> validateDepItem nsp
            Repetitive _rt variation' -> validateDepItem
                (NonSpare name title (ContextFree variation') doc)
            Explicit _ -> pure ()
            Compound lst -> mapM_ validateDepItem (catMaybes lst)

instance Validate Expansion where
    validate (Expansion cat _title edition date mn items) = do
        validate cat
        validate edition
        validate date
        case mn of
            Nothing -> pure ()
            Just (ByteSize n) -> do
                when (length items > n*8) "insufficient fspec length"
        forM_ (catMaybes items) $ \item -> do
            when (offset item /= mempty) "top level alignment error"
            validate item

instance Validate Asterix where
    validate = \case
        AsterixBasic x -> validate x
        AsterixExpansion x -> validate x
