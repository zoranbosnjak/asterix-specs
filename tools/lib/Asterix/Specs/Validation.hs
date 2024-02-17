-- Validators for asterix data types.

module Asterix.Specs.Validation where

import           Control.Monad
import qualified Data.Text as T
import           Data.Maybe
import           Data.List (nub, (\\))

import           Asterix.Specs

type ValidationError = T.Text

isCapital :: T.Text -> Bool
isCapital w
    | elem w exceptions = True
    | otherwise = not $ elem (T.head w) ['a'..'z']
  where
    exceptions =
        [ "of", "in", "by", "to", "from", "the", "for", "and", "or"
        , "with", "which", "is", "as", "on", "a", "vs."
        ]

-- Bit alignment property.
class IsAligned a where
    isAligned :: a -> Bool

instance IsAligned RegisterSize where
    isAligned n = (n `mod` 8) == 0

instance IsAligned Variation where
    isAligned (Element n _content) = isAligned n
    isAligned (Group lst) = check notAlignedParts
      where
        notAlignedParts = filter (not . isAligned) lst
        check items = maybe False isAligned (bitSize $ Group items)
    isAligned (Extended lst) =
        isAligned (sum $ fmap count lst)
        && all (== 7) (fxBitOffset (0::Int) lst)
      where
        count Nothing = 1 -- FX bit
        count (Just i) = fromJust $ bitSize i -- fixed item
        fxBitOffset _ [] = []
        fxBitOffset ix (Nothing : xs) = (ix `mod` 8) : fxBitOffset (succ ix) xs
        fxBitOffset ix (Just x : xs) = fxBitOffset (ix + fromJust (bitSize x)) xs
    isAligned (Repetitive rt variation) = case rt of
        RepetitiveRegular repSize -> eachAligned repSize || sumAligned repSize
        RepetitiveFx -> case bitSize variation of
            Nothing -> False
            Just n -> isAligned (n + 1)
      where
        eachAligned repSize = and
            [ isAligned repSize
            , isAligned variation
            ]
        sumAligned repSize = case bitSize variation of
            Nothing -> False
            Just n -> isAligned (repSize + n)
    isAligned (Explicit _) = True
    isAligned RandomFieldSequencing = True
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

getConstrainNumber :: Constrain -> Number
getConstrainNumber = \case
    EqualTo val -> val
    NotEqualTo val -> val
    GreaterThan val -> val
    GreaterThanOrEqualTo val -> val
    LessThan val -> val
    LessThanOrEqualTo val -> val

isNegative :: Number -> Bool
isNegative = \case
    NumInt val -> val < 0
    NumDiv a b -> isNegative a /= isNegative b
    NumPow a _ -> a < 0

checkNonNegative :: Signedness -> [Constrain] -> [ValidationError]
checkNonNegative Signed _ = []
checkNonNegative Unsigned lst = join $ do
    constrain <- lst
    [reportWhen (isNegative $ getConstrainNumber constrain)
        "'unsigned' content, negative constrain"]

instance Validate (RegisterSize, Content) where
    validate _warnings (_n, ContentRaw) = []
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
        , join $ do
            guard warnings
            val <- values
            return $ reportWhen (elem '"' (T.unpack val)) $
                "Value contain quotes -> " <> val
        ]
      where
        keys = fst <$> lst
        values = snd <$> lst
        sizeCheck = compare (length keys) (2 ^ n)
    validate _warnings (stringSize, ContentString stringType) =
        reportWhen (stringSize `mod` charSize /= 0) $
            T.pack $ show stringType ++ " (" ++ show charSize
            ++ " bits per symbol) does not fit in parent element ("
            ++ show stringSize ++ " bits)"
      where
        charSize = case stringType of
            StringAscii -> 8
            StringICAO  -> 6
            StringOctal -> 3
    validate _warnings (_, ContentQuantity sign _k _unit cst) = do
        checkNonNegative sign cst
    validate _warnings (_, ContentInteger sign cst) =
        checkNonNegative sign cst
    validate _warnings (n, ContentBds bt) = do
        let expected = case bt of
                BdsWithAddress -> 64
                BdsAt _mAddr -> 56
        join
            [ reportWhen (n /= expected) "unexpected BDS register length"
            , case bt of
                BdsAt (Just (BdsAddr addr)) -> reportWhen (addr < 0 || addr > 255)
                    "BDS address out of range"
                _ -> []
            ]

instance Validate (RegisterSize, Rule) where
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
    validate warnings (Group items) = join
        [ validate warnings items
        , reportWhen (length items <= 1) "group requires more items"
        , reportWhen (duplicatedNames items) "duplicated names"
        ]
    validate warnings x@(Extended lst) = join
        [ reportUnless (isAligned x) "bit alignment"
        , join $ do
            item <- items
            return $ maybe ["item size not fixed"] (const []) (bitSize item)
        , reportWhen (duplicatedNames items) "duplicated names"
        , validate warnings items
        , reportWhen (length items <= 1) "extended subitem list size"
        ]
      where
        items = catMaybes lst
    validate warnings (Repetitive rt variation) = case rt of
        RepetitiveRegular m -> join
            [ reportUnless (m > 0) "REP size"
            , reportUnless (isAligned m) "REP alignment"
            , reportUnless (isAligned variation) "variation alignment"
            , validate warnings variation
            ]
        RepetitiveFx -> validate warnings variation
    validate _warnings (Explicit _) = []
    validate _warnings RandomFieldSequencing = []
    validate warnings x@(Compound mFspecSize items) = join
        [ reportUnless (isAligned x) "alignment error"
        , validate warnings items
        , let items' = catMaybes items
          in reportWhen (duplicatedNames items') "duplicated names"
        , reportWhen (isNothing $ last items) "last element in compound is empty"
        , reportWhen (any isSpare items) "unexpected spare item inside compound"
        , reportWhen (warnings && (length items <= 1))
            "compound item with just one element"
        , case mFspecSize of
            Nothing -> []
            Just n -> mconcat
                [ reportWhen (mod n 8 /= 0) "fspec not aligned"
                , reportWhen (n < length items) "insufficient fspec length"
                ]
        ]
      where
        isSpare (Just (Spare _)) = True
        isSpare _ = False

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
        -- title whitespaces
        , reportWhen (T.strip title /= title) $
            name <> ":Title contain leading or trailing whitespaces -> " <> title
        , reportWhen (warnings && elem '"' (T.unpack title)) $
            name <> ":Title contain quotes -> " <> title
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
        , join (topAlignment <$> basCatalogue basic)
        ]
      where
        topAlignment :: Item -> [ValidationError]
        topAlignment i = reportUnless (isAligned i) $ case i of
            Spare _ -> error "unexpected spare"
            Item name _title _var _doc -> "Top level item " <> name <> " alignment error."

        validateCat :: [ValidationError]
        validateCat = reportUnless (basCategory basic `elem` [0..255])
            "category number out of range"

        allItemsDefined :: [ValidationError]
        allItemsDefined = join [requiredNotDefined, definedNotRequired, noDups]
          where
            required :: [Name]
            required = catMaybes $ case basUap basic of
                Uap lst -> lst
                Uaps lst _msel -> nub $ join $ fmap snd lst

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
            Uaps lst msel -> join
                [ do
                    let dupNames = nub x /= x where x = fst <$> lst
                    reportWhen dupNames "duplicated UAP names"
                , do
                    (uapName, lst') <- lst
                    do
                        x <- validateList lst'
                        return (uapName <> ":" <> x)
                , case msel of
                    Nothing -> []
                    Just sel -> validateSelector lst sel
                ]
          where
            validateList lst = join
                [ let x = catMaybes lst
                  in reportWhen (nub x /= x) "duplicated items in UAP"
                , reportWhen (isNothing $ last lst) "spare at the end of UAP is redundant"
                ]
            validateSelector lst (UapSelector name table) = join
                [ case findItemByName basic name of
                    Nothing -> [showPath name <> " not defined"]
                    Just i -> case bitSize i of
                        Nothing -> [showPath name <> " unknown size"]
                        Just m ->
                            let ln = compare (length table) (2 ^ m)
                            in reportWhen (ln == GT) ("too many variations")
                , do
                    uap <- fmap snd table
                    reportWhen (uap `notElem` (fmap fst lst)) ("unknown uap: " <> uap)
                ]

        validateDepItem :: Item -> [ValidationError]
        validateDepItem (Spare _n) = []
        validateDepItem (Item name title variation doc) = case variation of
            Element _n rule -> case rule of
                ContextFree _ -> []
                Dependent someItemName rules -> case findItemByName basic someItemName of
                    Nothing -> [showPath someItemName <> " not defined"]
                    Just someItem -> case bitSize someItem of
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

            Extended lst -> join $ fmap validateDepItem (catMaybes lst)
            Repetitive _rt variation' -> validateDepItem (Item name title variation' doc)
            Explicit _ -> []
            RandomFieldSequencing -> []
            Compound _mFspecSize lst -> join (fmap validateDepItem $ catMaybes lst)

instance Validate Expansion where
    validate warnings x = join
        [ validate warnings $ expVariation x
        , reportUnless (isAligned $ expVariation x) "Top level alignment error."
        ]

instance Validate Asterix where
    validate warnings = \case
        AsterixBasic x -> validate warnings x
        AsterixExpansion x -> validate warnings x
