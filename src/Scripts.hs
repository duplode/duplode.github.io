{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Scripts
    ( deploy
    , SiteBuilders(..)
    , PotentialIssue(..)
    , PotentialIssueError(..)
    ) where

import qualified Hakyll as H
import Control.Applicative
import System.Environment (withArgs)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Monad.Managed (with)
import Turtle

deploy :: SiteBuilders -> H.Configuration -> IO ExitCode
deploy builders conf =
    echoOK "Rebuilding and deploying site...\n"
    -- Automated issue-thread creation. Currently under testing.
    .&&. withArgs ["rebuild"]
        (liftA2 (>>) theSiteRules ghIssuesRules builders
            & H.hakyllWithExitCode conf)
    -- Switch lines to build without automated issue-thread creation.
    -- .&&. withArgs ["rebuild"] (theSiteRules builders & H.hakyllWithExitCode conf)
    .&&. shell "rsync -avc --delete --exclude '.git' _site/ ../master/" ""
    .&&. cd "../master"
        *> shell "git rev-parse --git-dir > /dev/null" ""
    .&&. ( fold (inshell "git status --porcelain" "") countLines
        >>= \case
            0 -> return ExitSuccess
            _ -> echoOK "\nAdding and committing the following changes:"
                .&&. shell "git status" ""
                .&&. shell "git add ." ""
                .&&. with (shell "git commit" "" & fork) wait )
    .&&. echoOK "\nPushing pending commits, if any."
    .&&. with (shell "git push gh master" "" & fork) wait

echoOK :: MonadIO io => Line -> io ExitCode
echoOK txt = echo txt *> pure ExitSuccess

data SiteBuilders = SiteBuilders
    { theSiteRules :: H.Rules ()
    , ghIssuesRules :: H.Rules ()
    }

data PotentialIssue = PotentialIssue
    { potentialIssueNumber :: Int
    , potentialIssueTitle :: String
    , potentialIssuePath :: String
    } deriving (Show, Generic)

instance Binary PotentialIssue

data PotentialIssueError = MalformedIssueNumber | NegativeIssueNumber
    deriving (Eq, Show)
