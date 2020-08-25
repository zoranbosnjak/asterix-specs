{-# LANGUAGE OverloadedStrings #-}

import qualified GHC.IO.Encoding as E
import           System.Environment (getEnvironment)
import           Data.Aeson
import           Hakyll

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
    Just manifest <- decodeFileStrict (specs <> "/" <> "manifest.json")

    print (manifest :: Array)

    hakyllWith config $ do
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        create ["specs.md"] $ do
            let specsCtx = defaultContext
                    <> constField "gitrev" gitrev
                    <> listField "cats" defaultContext (pure [])
            compile $ do
                makeItem ""
                    >>= loadAndApplyTemplate "templates/specs.md" specsCtx

        create ["specs.html"] $ do
            route   idRoute
            compile $ do
                load "specs.md"
                >>= renderPandoc
                >>= (\(Item _a b) -> pure (Item "specs.html" b))
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

