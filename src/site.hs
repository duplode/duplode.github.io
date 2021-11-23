--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.FilePath (takeFileName)
import qualified GHC.IO.Encoding as E
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (filterM)
import Hakyll
import Skylighting (styleToCss, tango)
import Text.Pandoc.Options
import Text.Pandoc.Templates (compileTemplate)
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import qualified Text.HTML.TagSoup.Tree as TS

import qualified Scripts as Scr
import qualified IssueThread as Iss

--------------------------------------------------------------------------------
hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { providerDirectory = "src"
    , deploySite = Scr.deploy siteBuilders
    }
    where
    siteBuilders = Scr.SiteBuilders
        { Scr.theSiteRules = theSite
        , Scr.ghIssuesRules = ghIssues
        }

tocPandocWriterOptions :: Compiler WriterOptions
tocPandocWriterOptions = do
    tmpl <- either (const Nothing) Just
        <$> unsafeCompiler (compileTemplate "" tmplSpec)
    return defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = tmpl
        }
    where
    tmplSpec = T.unlines
        [ "<div id=\"contents\">"
        , "<p class=\"mini-header\">Contents</p>"
        , "$toc$"
        , "</div>"
        , "$body$"
        ]

pandocCompilerOfOurs :: Compiler (Item String)
pandocCompilerOfOurs = do
    ident <- getUnderlying
    mToc <- ident `getMetadataField` "toc"
    tocOpts <- case mToc of
        Nothing -> return defaultHakyllWriterOptions
        Just _ -> tocPandocWriterOptions
    pandocCompilerWith defaultHakyllReaderOptions tocOpts

suppressToc :: Item String -> Item String
suppressToc = fmap (withTagList (withTree suppressor))
    where
    withTree f = TS.flattenTree . f . TS.tagTree
    suppressor = TS.transformTree $ \t -> case t of
        TS.TagBranch "div" [("id", "contents")] _ -> []
        _ -> [t]


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

-- If the reddit metadata parameter is defined, link to a specific
-- comment thread. Otherwise, rely on reddit's automagic links to
-- submissions, which can be used to submit a post if that wasn't done
-- yet.
redditCtx :: Context String
redditCtx = field "reddit-button" $ \it -> do
    mRedd <- itemIdentifier it `getMetadataField` "reddit"
    redditFragment it mRedd
    where
    redditFixedBasePath = "https://reddit.com/r/haskell/comments/"
    redditBasePath = "https://reddit.com/"
    -- Ideally this wouldn't be hardcoded.
    -- Note the inconsistent use of slashes.
    blogBasePath = "https://duplode.github.io"
    redditFragment it mRedd = fmap itemBody $ do
        frag <- load "fragments/reddit.html"
        -- Copied from the urlField source code.
        pageUrl <- fmap (maybe "" toUrl) . getRoute . itemIdentifier $ it
        let targetUrl = case mRedd of
                Just redd -> redditFixedBasePath ++ redd
                Nothing -> redditBasePath ++ blogBasePath ++ pageUrl
        applyAsTemplate (constField "reddit-link" targetUrl) frag

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

discourseCtx :: Context String
discourseCtx = field "discourse-button" $ \it -> do
    mDis <- itemIdentifier it `getMetadataField` "discourse"
    maybe (return "") discourseFragment mDis
    where
    discourseBasePath = "https://discourse.haskell.org/t/"
    discourseFragment dis = fmap itemBody $
        load "fragments/discourse.html"
        >>= applyAsTemplate (discourseLinkCtx dis)
    discourseLink dis = discourseBasePath ++ dis
    discourseLinkCtx dis = constField "discourse-link" $ discourseLink dis

twitterCardImageCtx :: Context String
twitterCardImageCtx = field "twitter-image" $ \it -> do
    maybe "" (imageBasePath ++)
        <$> itemIdentifier it `getMetadataField` "twitter-card-image"
    where
    -- TODO: This *really* shouldn't be hardcoded
    imageBasePath = "https://duplode.github.io/images/"

twitterCardDescrCtx :: Context String
twitterCardDescrCtx = field "twitter-descr" $ \it -> do
    maybe "" id
        <$> itemIdentifier it `getMetadataField` "twitter-card-descr"

twitterCardCtx :: Context String
twitterCardCtx = twitterCardImageCtx <> twitterCardDescrCtx

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postItemCtx

postCtx :: Context String
postCtx = postDateCtx
    <> ghCommentsCtx <> licenseInfoCtx <> redditCtx <> discourseCtx
    <> baseCtx

baseCtx :: Context String
baseCtx = defaultContext <> twitterCardCtx

--------------------------------------------------------------------------------
plainPosts, literatePosts, hiddenPosts, allPosts :: Pattern
plainPosts = "posts/*.md"
literatePosts = "posts/*.lhs"
hiddenPosts = "posts/_*"
allPosts = (plainPosts .||. literatePosts)
    .&&. complement hiddenPosts

--------------------------------------------------------------------------------
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith hakyllConfig theSite

theSite :: Rules ()
theSite = do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon/*" $ do
        route   $ customRoute $ takeFileName . toFilePath
        compile   copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "extras/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "demos/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.md" $ do
        route $ setExtension ".html"
        compile $ do
            pandocCompilerOfOurs
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls

    match "posts.html" $ do
        route $ idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (allPosts .&&. hasNoVersion)
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
                =<< filterM (fmap isNothing
                        . (`getMetadataField` "retired") . itemIdentifier)
                -- The right snapshot to use is specified at teaserCtx.
                -- At this specific stage the item body isn't used at all.
                -- That's a reason for only using suppressToc later on.
                =<< loadAll (allPosts .&&. hasNoVersion)
            getResourceBody
                >>= applyAsTemplate
                    (listField "post-teasers" teaserCtx (return barePosts))
                >>= loadAndApplyTemplate
                    "templates/default.html" baseCtx
                >>= relativizeUrls
                >>= return . suppressToc


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
                        >>= return . suppressToc

            barePosts <- mapM processRssItem
                =<< fmap (take 12) . recentFirst
                =<< loadAllSnapshots (allPosts .&&. hasNoVersion) "content"
            renderRss rssConfig feedCtx barePosts

    match "templates/*" $ compile templateCompiler

    match "fragments/*" $ compile getResourceBody

-- Automated creation of comment threads as GitHub issues.
-- For testing, pass (plainPosts .||. literatePosts)
ghIssues :: Rules ()
ghIssues = Iss.ghIssues allPosts
--------------------------------------------------------------------------------
