--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath (takeFileName)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
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
    renderPandocWith defaultHakyllReaderOptions
        (if withToc then tocPandocWriterOptions else ourPandocWriterOptions)

pandocCompilerOfOurs :: Compiler (Item String)
pandocCompilerOfOurs = pandocCompilerOfOurs' False

pandocCompilerOfOurs' :: Bool -> Compiler (Item String)
pandocCompilerOfOurs' withToc =
    pandocCompilerWith defaultHakyllReaderOptions $
        if withToc then tocPandocWriterOptions else ourPandocWriterOptions

rssConfig :: FeedConfiguration
rssConfig = FeedConfiguration
    { feedTitle = "The Life Monadic"
    , feedDescription = "Haskell amusements"
    , feedAuthorName = "Daniel Mlot"
    , feedAuthorEmail = "" -- Not used by RSS.
    , feedRoot = "http://duplode.github.io"
    }

--------------------------------------------------------------------------------
postDateCtx :: Context String
postDateCtx = dateField "date" "%B %e, %Y"

postItemCtx :: Context String
postItemCtx = postDateCtx <> baseCtx

licenseInfoCtx :: Context String
licenseInfoCtx = field "license-info" $ \it -> do
    lic <- itemIdentifier it `getMetadataField` "license"
    case lic of
        Just "CC-BY-SA" -> loadBody "fragments/cc-by-sa.html"
        _               -> return ""

redditCtx :: Context String
redditCtx = field "reddit-button" $ \it -> do
    redd <- itemIdentifier it `getMetadataField` "reddit"
    maybe (return "") (const $ loadBody "fragments/reddit.html") redd

ghCommentsCtx :: Context String
ghCommentsCtx = field "gh-comments-button" $ \it -> do
    mGhi <- itemIdentifier it `getMetadataField` "gh-issue"
    maybe (return "") commentFragment mGhi
    where
    issueBasePath = "https://github.com/duplode/duplode.github.io/issues/"
    commentFragment ghi = fmap itemBody $
        load "fragments/gh-comments.html"
        >>= applyAsTemplate (issueLinkCtx ghi)
    issueLink ghi = issueBasePath ++ ghi
    issueLinkCtx ghi = constField "gh-issue-link" $ issueLink ghi

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postItemCtx

postCtx :: Context String
postCtx = postDateCtx
    <> ghCommentsCtx <> licenseInfoCtx <> redditCtx
    <> baseCtx

baseCtx :: Context String
baseCtx = defaultContext

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "extras/**" $ do
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
        hiddenPosts = "posts/_*"
        allPosts = (plainPosts .||. literatePosts)
            .&&. complement hiddenPosts

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
                    "templates/post.html" postCtx
                >>= loadAndApplyTemplate
                    "templates/post-wrapper.html" postItemCtx
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls

    match "index.html" $ do
        route $ idRoute
        compile $ do
            barePosts <- fmap (take 6) . recentFirst
                =<< loadAllSnapshots allPosts "content"
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

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bodyField "description"
                    <> baseCtx

                processRssItem :: Item String -> Compiler (Item String)
                processRssItem it = do
                    let id' = itemIdentifier it
                    baseRoute <- getRoute id'
                    let toRssCtx = constField "reddit-alt"
                                (fromMaybe "" baseRoute)
                            <> postCtx
                    return it
                        >>= loadAndApplyTemplate
                            "templates/post.html" toRssCtx
                        >>= relativizeUrls

            barePosts <- mapM processRssItem
                =<< fmap (take 12) . recentFirst
                =<< loadAllSnapshots allPosts "content"
            renderRss rssConfig feedCtx barePosts

    match "templates/*" $ compile templateCompiler

    match "fragments/*" $ compile getResourceBody


--------------------------------------------------------------------------------
