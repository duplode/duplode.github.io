--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid (mappend)
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

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
