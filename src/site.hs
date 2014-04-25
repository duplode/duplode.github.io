--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath (takeFileName)
import Data.Monoid ((<>))
import Hakyll
import Text.Highlighting.Kate (styleToCss, tango)
import Text.Pandoc.Options


--------------------------------------------------------------------------------
hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { providerDirectory = "src"
    }

ourPandocWriterOptions :: WriterOptions
ourPandocWriterOptions = defaultHakyllWriterOptions{ writerHtml5 = True }

tocPandocWriterOptions :: WriterOptions
tocPandocWriterOptions = ourPandocWriterOptions
    { writerTableOfContents = True
    , writerTemplate = "$toc$\n$body$"
    , writerStandalone = True
    }

processWithPandoc :: Item String -> Compiler (Item String)
processWithPandoc = processWithPandoc' False

processWithPandoc' :: Bool -> Item String -> Compiler (Item String)
processWithPandoc' withToc =
    return . renderPandocWith defaultHakyllReaderOptions
        (if withToc then tocPandocWriterOptions else ourPandocWriterOptions)

pandocCompilerOfOurs :: Compiler (Item String)
pandocCompilerOfOurs = pandocCompilerOfOurs' False

pandocCompilerOfOurs' :: Bool -> Compiler (Item String)
pandocCompilerOfOurs' withToc =
    pandocCompilerWith defaultHakyllReaderOptions $
        if withToc then tocPandocWriterOptions else ourPandocWriterOptions

--------------------------------------------------------------------------------
postItemCtx :: Context String
postItemCtx = dateField "date" "%B %e, %Y" <> defaultContext

licenseInfoCtx :: Context String
licenseInfoCtx = field "license-info" $ \it -> do
    lic <- itemIdentifier it `getMetadataField` "license"
    case lic of
        Just "CC-BY-SA" -> loadBody "fragments/cc-by-sa.html"
        _               -> return ""

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postItemCtx

baseCtx :: Context String
baseCtx = licenseInfoCtx <> defaultContext

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.md" $ do
        route $ setExtension ".html"
        compile $ do
            pandocCompilerOfOurs
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls

    let plainPosts = "posts/*.md"
        literatePosts = "posts/*.lhs"
        allPosts = plainPosts .||. literatePosts

    match "posts.html" $ do
        route $ idRoute
        compile $ do
            posts <- recentFirst =<< loadAll allPosts
            getResourceBody
                >>= applyAsTemplate
                    (listField "posts" postItemCtx (return posts))
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls

    match allPosts $ do
        route   $ setExtension "html"
        compile $ do
            pandocCompilerOfOurs
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls

    match "index.html" $ do
        route $ idRoute
        compile $ do
            barePosts <- recentFirst =<< loadAllSnapshots allPosts "content"
            getResourceBody
                >>= applyAsTemplate
                    (listField "post-teasers" teaserCtx (return barePosts))
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls


    match "repo/*" $ do
        route   $ customRoute $ takeFileName . toFilePath
        compile   copyFileCompiler

    create [".nojekyll"] $ do
        route     idRoute
        compile $ makeItem ("" :: String)

    create ["css/syntax.css"] $ do
        route   $ idRoute
        compile $ makeItem (compressCss . styleToCss $ tango)

    match "templates/*" $ compile templateCompiler

    match "fragments/*" $ compile getResourceBody


--------------------------------------------------------------------------------
