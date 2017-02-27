{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module IssueThread
    ( ghIssues
    ) where

import Data.Function ((&))
import Data.Monoid ((<>))
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Data.Bifunctor (first, second)
import Text.Read
import Data.Vector ((!?))
import Data.String (fromString)
import System.IO
import System.FilePath ((-<.>))
import Control.Exception
import qualified Control.Monad.Except as Er
import qualified GHC.Exts as Ex (fromList)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Hakyll
import qualified GitHub.Endpoints.Issues as G
import qualified GitHub.Auth as G
import qualified GitHub.Data.Name as G
import qualified GitHub.Data.Id as G
import qualified GitHub.Data.Options as G
import qualified GitHub.Data.Request as G
import qualified GitHub.Request as G

ghIssues :: Pattern -> Rules ()
ghIssues withIssues = do

    match withIssues . version "gh-issue" $
        compile $ do
            ident <- getUnderlying
            let postPath = toFilePath ident
            title <- ident `getMetadataField'` "title"
            mIssue <- ident `getMetadataField` "gh-issue"
            case mIssue of
                Nothing -> makeItem ()
                Just issue -> parsePotentialIssueNumber issue & either
                    (\case -- TODO: bail out more gracefully.
                        MalformedIssueNumber -> Er.throwError
                            ["ghIssues: Malformed issue for " ++ postPath]
                        NegativeIssueNumber -> Er.throwError
                            ["ghIssues: Issue < 1 for " ++ postPath])
                    (\nIssue -> do
                        potIss <- makeItem $
                            PotentialIssue
                                { potentialIssueNumber = nIssue
                                , potentialIssueTitle = title
                                , potentialIssuePath = postPath
                                }
                        saveSnapshot "potential-issue" potIss
                        makeItem ())

    create ["virtual/gh-issues"] $
        compile $ do
            potIssues <- sortBy
                    (comparing $ potentialIssueNumber . itemBody)
                <$> loadAllSnapshots
                    (withIssues .&&. hasVersion "gh-issue") "potential-issue"
            emLastIssue <- unsafeCompiler retrieveLatestIssue
            emLastIssue & either
                (\err -> Er.throwError
                    [ "ghIssues: Last issue request failed: " ++ show err ])
                (\mLastIssue -> do
                    let nLastIssue = fromMaybe 0 $
                            G.issueNumber <$> mLastIssue
                    auth <- unsafeCompiler authGitHub
                    forM_ potIssues $
                        attemptCreatingTheIssue auth nLastIssue)
            makeItem ()

data PotentialIssue = PotentialIssue
    { potentialIssueNumber :: Int
    , potentialIssueTitle :: String
    , potentialIssuePath :: String
    } deriving (Show, Generic)

instance Binary PotentialIssue

data PotentialIssueError = MalformedIssueNumber | NegativeIssueNumber
    deriving (Eq, Show)

parsePotentialIssueNumber :: String -> Either PotentialIssueError Int
parsePotentialIssueNumber s = do
    nIssue <- first (const MalformedIssueNumber) $ readEither s
    when (nIssue < 1) $ Left NegativeIssueNumber
    return nIssue

authGitHub :: IO G.Auth
authGitHub = G.BasicAuth
    <$> (putStrLn "GitHub username:" *> fmap fromString getLine)
    <*> fmap fromString getPassword

createTheIssue :: G.Auth -> String -> Identifier
    -> IO (Either G.Error G.Issue)
createTheIssue auth title ident = G.createIssue auth
    "duplode" "duplode.github.io"
    (G.newIssue (fromString title))
        -- TODO: Duplicates the post route.
        { G.newIssueBody = Just . fromString $
            "Comment thread for ["
            ++ title ++ "]("
            ++ relativizeUrlsWith
                "https://duplode.github.io/"
                (toFilePath ident -<.> "html")
            ++ ")."
        , G.newIssueLabels = Just $
            Ex.fromList [G.N "comment-thread"]
        }

-- This relies on there being no gaps between issue numbers, and in
-- it being impossible to delete an issue.
retrieveLatestIssue :: IO (Either G.Error (Maybe G.Issue))
retrieveLatestIssue = do
    let eReqLatestIssue = G.issuesForRepoR "duplode" "duplode.github.io"
            (G.sortDescending <> G.sortByCreated <> G.stateAll) 1
    second (!? 0) <$> G.executeRequest' eReqLatestIssue

-- Currently unused.
retrieveOldIssue :: Int -> IO (Either G.Error G.Issue)
retrieveOldIssue n =  G.issue "duplode" "duplode.github.io" (G.Id n)

-- Creates a GitHub issue for a potential issue, but only if the issue
-- numbers are above that of the latest existing issue.
attemptCreatingTheIssue :: G.Auth -> Int -> Item PotentialIssue
    -> Compiler ()
attemptCreatingTheIssue auth lastN (Item ident potIss) = do
    let potN = potentialIssueNumber potIss
        title = potentialIssueTitle potIss
    if potN <= lastN
        then unsafeCompiler . putStrLn $
                "Issue " ++ show potN ++ " for "
                ++ toFilePath ident ++ " already taken."
        else do
            eNewIss <- unsafeCompiler $
                createTheIssue auth title ident
            eNewIss & either
                (\err -> Er.throwError
                    [ "ghIssues: Issue creation failed "
                    ++ "for " ++ toFilePath ident ++ ": "
                    ++ show err ])
                (\newIss -> let newN = G.issueNumber newIss
                    in if newN /= potN
                        then Er.throwError $
                            [ "ghIssues: Issue for "
                            ++ toFilePath ident
                            ++ " created with wrong number "
                            ++ show newN ++ ", expected "
                            ++ show potN ]
                        else unsafeCompiler . putStrLn $
                                "Created issue " ++ show newN
                                ++ " for " ++ toFilePath ident
                )

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
