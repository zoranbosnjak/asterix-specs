{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified GHC.IO.Encoding as E
import           System.Environment (getEnvironment)
import           System.Directory (listDirectory)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Hakyll

data Cat = Cat
    { catNumber :: String
    , catCats :: [String]
    , catRefs :: [String]
    } deriving (Show)

instance FromJSON Cat where
    parseJSON = withObject "Cat" $ \v -> Cat
        <$> v .: "category"
        <*> v .: "cats"
        <*> v .: "refs"

config :: Configuration
config = defaultConfiguration

getEnvVariableExpr :: String -> IO String
getEnvVariableExpr envKey = do
    env <- getEnvironment
    case lookup envKey env of
        Nothing -> error $ "Environment variable " ++ envKey ++ " not defined."
        Just value -> pure value

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8

    gitrev <- getEnvVariableExpr "SHORT_GITREV"
    gitdate <- getEnvVariableExpr "GIT_DATE"
    specs <- getEnvVariableExpr "SPECS"
    Just (manifest :: [Cat]) <- decodeFileStrict (specs <> "/" <> "manifest.json")
    syntax <- getEnvVariableExpr "SYNTAX"
    syntaxImages <- listDirectory $ syntax++"/syntax/png"
    aspecsVersion <- getEnvVariableExpr "ASPECS_VERSION"
    aspecsSha256Sum <- getEnvVariableExpr "ASPECS_SHA256"
    typesSimple <- readFile "types_simple.hs"

    -- extend defaultContext
    let ctx = defaultContext
            <> constField "gitrev" gitrev
            <> constField "gitdate" gitdate

    hakyllWith config $ do
        match "css/*" $ do
            route idRoute
            compile compressCssCompiler

        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- for all definitions
        forM_ manifest $ \(Cat n cats refs) -> do
            let lst = [ (x, "cats", "cat") | x <- cats]
                   ++ [ (x, "refs", "ref") | x <- refs]
            forM_ lst $ \(ed,a,b) -> do
                let dst = "specs/cat" ++ n ++ "/" ++ a ++ "/" ++ b ++ ed ++ "/definition."
                    src = specs ++ "/" ++ dst

                -- copy specs files in various formats
                forM_ ["ast", "txt", "json", "pandoc.native", "html", "pdf"] $ \fmt -> do
                    create [ fromFilePath (dst ++ fmt) ] $ do
                        route idRoute
                        compile $ do
                            unsafeCompiler (BSL.readFile (src ++ fmt)) >>= makeItem

        -- syntax images
        forM_ syntaxImages $ \img -> do
            create [ fromFilePath ("syntax/" ++ img) ] $ do
                route idRoute
                compile $ do
                    unsafeCompiler (BSL.readFile $ syntax ++ "/syntax/png/" ++ img) >>= makeItem

        -- templates
        let files =
                [ ("specs", ctx
                    <> listField "nums" catCtx (mapM makeItem manifest))
                , ("struct", ctx
                    <> constField "typesSimple" typesSimple)
                , ("syntax",
                    let imgCtx = field "img" (\(Item _ i) -> pure i)
                    in ctx
                        <> listField "images" imgCtx (mapM makeItem syntaxImages))
                , ("aspecs", ctx
                    <> constField "aspecsVersion" aspecsVersion
                    <> constField "aspecsSha256Sum" aspecsSha256Sum
                  )
                ]

        forM_ files $ \(name, ctx) -> do
            let n a b = fromFilePath (a <> name <> b)
            create [n "" ".md"] $ compile $ do
                makeItem ""
                    >>= loadAndApplyTemplate (n "templates/" ".md") ctx

            create [n "" ".html"] $ do
                route idRoute
                compile $ do
                    load $ n "" ".md"
                    >>= renderPandoc
                    >>= (\(Item _a b) -> pure (Item (n "" ".html") b))
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

catCtx :: Context Cat
catCtx = mconcat
    [ field "num" (\(Item _ i) -> pure (catNumber i))

    , boolField "hasCats" (\(Item _ i) -> catCats i /= [])
    , listFieldWith "cats" edCtx (\(Item _ i) -> mapM makeItem [(catNumber i, x) | x <- catCats i])

    , boolField "hasRefs" (\(Item _ i) -> catRefs i /= [])
    , listFieldWith "refs" edCtx (\(Item _ i) -> mapM makeItem [(catNumber i, x) | x <- catRefs i])
    ]
  where
    edCtx
        = field "n" (\(Item _ (n, _ed)) -> pure n)
       <> field "ed" (\(Item _ (_n, ed)) -> pure ed)
