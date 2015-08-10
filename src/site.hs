--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath (takeFileName, (-<.>), (</>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Control.Monad.State as St
import Data.Either (either)
import Data.List (sortBy)
import Data.Function ((&))
import Data.Ord (comparing)
import System.IO
import Control.Exception
import Data.String (fromString)
import Text.Read (readMaybe)
import Hakyll
import Text.Highlighting.Kate (styleToCss, tango)
import Text.Pandoc.Options
import qualified Github.Issues as G
import qualified Github.Auth as G

import qualified Scripts as Scr

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
plainPosts, literatePosts, hiddenPosts, allPosts :: Pattern
plainPosts = "posts/*.md"
literatePosts = "posts/*.lhs"
hiddenPosts = "posts/_*"
allPosts = (plainPosts .||. literatePosts)
    .&&. complement hiddenPosts

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig theSite

theSite :: Rules ()
theSite = do
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

-- TODO: Make these rules less ugly.
ghIssues :: Rules ()
ghIssues = do
    -- For testing purposes, switch to plainPosts .||. literatePosts
    let withIssues = allPosts

    match withIssues . version "gh-issue" $
        compile $ do
            ident <- getUnderlying
            let postPath = toFilePath ident
            title <- ident `getMetadataField'` "title"
            mIssue <- ident `getMetadataField` "gh-issue"
            case mIssue of
                Nothing -> makeItem ()
                Just issue -> case readMaybe issue :: Maybe Int of
                    Nothing -> error $ "ghIssues: Malformed issue for "
                        ++ postPath -- TODO: bail out more gracefully.
                    Just nIssue
                        | nIssue < 1 -> error $ "ghIssues: Issue < 1 for "
                            ++ postPath
                        | otherwise -> do
                            potIss <- makeItem $
                                Scr.PotentialIssue
                                    { Scr.potentialIssueNumber = nIssue
                                    , Scr.potentialIssueTitle = title
                                    , Scr.potentialIssuePath = postPath
                                    }
                            saveSnapshot "potential-issue" potIss
                            makeItem ()

    create ["virtual/gh-issues"] $
        compile $ do
            potIssues <- sortBy (comparing $
                    Scr.potentialIssueNumber . itemBody)
                <$> loadAllSnapshots withIssues "potential-issue"
            emLastIssue <- fmap (fmap listToMaybe) . unsafeCompiler $
                G.issuesForRepo "duplode" "duplode.github.io" [G.PerPage 1]
            unsafeCompiler $ emLastIssue & either
                (error . ("ghIssues: Last issue request failed: " ++) . show)
                (\mLastIssue -> do
                    let nLastIssue = fromMaybe 0 $
                            G.issueNumber <$> mLastIssue
                    auth <- G.GithubBasicAuth
                        <$> (putStrLn "GitHub username:"
                            *> fmap fromString getLine)
                        <*> fmap fromString getPassword
                    -- Creates GitHub issues for each potIss, but only if the
                    -- issue numbers are sensible (that is, they increase by
                    -- one, starting from the last existing issue number).
                    flip St.evalStateT nLastIssue $
                        St.forM_ potIssues $ \(Item ident potIss) -> do
                            curN <- St.get
                            let potN = Scr.potentialIssueNumber potIss
                                nextN = curN + 1
                                title = Scr.potentialIssueTitle potIss
                            eOldIssue <- St.liftIO $ G.issue
                                "duplode" "duplode.github.io" potN
                            eOldIssue & flip either
                                (St.liftIO . putStrLn
                                . (("Issue " ++ show potN ++ " for "
                                    ++ toFilePath ident ++ " already taken "
                                    ++ "by: ") ++)
                                . G.issueTitle)
                                (const $ do -- TODO: Analyse the error.
                                    St.unless (potN == nextN) $
                                        error ("ghIssues: Requested issue number for "
                                            ++ toFilePath ident ++ " is " ++ show potN
                                            ++ ", expected " ++ show nextN)
                                    eNewIss <- St.liftIO $ G.createIssue auth
                                        "duplode" "duplode.github.io"
                                        (G.newIssue title)
                                            -- TODO: Duplicates the post route.
                                            { G.newIssueBody = Just $
                                                "Comment thread for ["
                                                ++ title ++ "]("
                                                ++ relativizeUrlsWith
                                                    "https://duplode.github.io/"
                                                    (toFilePath ident -<.> "html")
                                                ++ ")."
                                            , G.newIssueLabels = Just
                                                ["comment-thread"]
                                            }
                                    eNewIss & either
                                        (error . (("ghIssues: Issue creation failed "
                                                ++ "for " ++ toFilePath ident ++ ": ")
                                            ++) . show)
                                        (\newIss -> let newN = G.issueNumber newIss
                                            in if newN /= potN
                                                then error $
                                                    "ghIssues: New issue number for "
                                                        ++ toFilePath ident ++ " is "
                                                        ++ show newN ++ ", expected "
                                                        ++ show potN
                                                else (St.liftIO . putStrLn $
                                                        "Created issue " ++ show newN
                                                        ++ " for " ++ toFilePath ident)
                                                    *> St.put newN)
                                    )
                    )
            makeItem ()

-- Qv. http://stackoverflow.com/q/4064378
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
--------------------------------------------------------------------------------
