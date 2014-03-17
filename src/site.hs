--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.FilePath (takeFileName)
import           Hakyll


--------------------------------------------------------------------------------
hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { providerDirectory = "src"
    }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "uc.md" $ do
        route   $ constRoute "index.html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate
                    "templates/default.html" defaultContext
                >>= relativizeUrls

    match "repo/*" $ do
        route   $ customRoute $ takeFileName . toFilePath
        compile   copyFileCompiler

    create [".nojekyll"] $ do
        route     idRoute
        compile $ makeItem ("" :: String)

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
