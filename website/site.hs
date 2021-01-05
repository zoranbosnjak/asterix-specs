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

pandocCompileString :: Item String -> Compiler (Item String)
pandocCompileString content = do
    itemPandoc <- readPandocWith defaultHakyllReaderOptions content
    itemPandoc' <- traverse (return . id) itemPandoc
    return $ writePandocWith defaultHakyllWriterOptions itemPandoc'

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8

    gitrev <- getEnvVariableExpr "SHORT_GITREV"
    specs <- getEnvVariableExpr "SPECS"
    Just (manifest :: [Cat]) <- decodeFileStrict (specs <> "/" <> "manifest.json")
    syntax <- getEnvVariableExpr "SYNTAX"
    syntaxImages <- listDirectory $ syntax++"/syntax/png"

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
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- for all definitions
        forM_ manifest $ \(Cat n cats refs) -> do
            let lst = [ (x, "cats", "cat") | x <- cats]
                   ++ [ (x, "refs", "ref") | x <- refs]
            forM_ lst $ \(ed,a,b) -> do
                let dst = "specs/cat" ++ n ++ "/" ++ a ++ "/" ++ b ++ ed ++ "/definition."
                    src = specs ++ "/" ++ dst

                -- copy specs files in various formats
                forM_ ["ast", "txt", "json", "rst", "pdf"] $ \fmt -> do
                    create [ fromFilePath (dst ++ fmt) ] $ do
                        route idRoute
                        compile $ do
                            unsafeCompiler (BSL.readFile (src ++ fmt)) >>= makeItem

                -- create html version
                create [ fromFilePath (dst ++ "html") ] $ do
                    route idRoute
                    compile $ do
                        unsafeCompiler (readFile (src ++ "rst"))
                        >>= (\x -> pure (Item "definition.rst" x))
                        >>= renderPandoc
                        >>= (\(Item _a val) -> pure (Item (fromFilePath $ dst ++ "html") val))
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

        create ["specs.md"] $ do
            let ctx = defaultContext
                    <> constField "gitrev" gitrev
                    <> listField "nums" catCtx (mapM makeItem manifest)
            compile $ do
                makeItem ""
                    >>= loadAndApplyTemplate "templates/specs.md" ctx

        create ["specs.html"] $ do
            route idRoute
            compile $ do
                load "specs.md"
                >>= renderPandoc
                >>= (\(Item _a b) -> pure (Item "specs.html" b))
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- syntax images
        forM_ syntaxImages $ \img -> do
            create [ fromFilePath ("syntax/" ++ img) ] $ do
                route idRoute
                compile $ do
                    unsafeCompiler (BSL.readFile $ syntax ++ "/syntax/png/" ++ img) >>= makeItem

        create ["syntax.md"] $ do
            let imgCtx = field "img" (\(Item _ i) -> pure i)
                ctx = defaultContext
                   <> listField "images" imgCtx (mapM makeItem syntaxImages)
            compile $ do
                makeItem ""
                    >>= loadAndApplyTemplate "templates/syntax.md" ctx

        create ["syntax.html"] $ do
            route idRoute
            compile $ do
                load "syntax.md"
                >>= renderPandoc
                >>= (\(Item _a b) -> pure (Item "syntax.html" b))
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

catCtx :: Context Cat
catCtx = mconcat
    [ field "num" (\(Item _ i) -> pure (catNumber i))

    , boolField "hasCats" (\(Item _ i) -> (catCats i /= []))
    , listFieldWith "cats" edCtx (\(Item _ i) -> mapM makeItem [(catNumber i, x) | x <- catCats i])

    , boolField "hasRefs" (\(Item _ i) -> (catRefs i /= []))
    , listFieldWith "refs" edCtx (\(Item _ i) -> mapM makeItem [(catNumber i, x) | x <- catRefs i])
    ]
  where
    edCtx
        = field "n" (\(Item _ (n, _ed)) -> pure n)
       <> field "ed" (\(Item _ (_n, ed)) -> pure ed)

