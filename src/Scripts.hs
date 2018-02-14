{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Scripts
    ( deploy
    , SiteBuilders(..)
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
    echoOK "Rebuilding and deploying site..."
    -- Builds the site and creates issue-threads.
    .&&. withArgs ["rebuild"]
       (liftA2 (>>) theSiteRules ghIssuesRules builders
           & H.hakyllWithExitCode conf)
    .&&. shell "rsync -avc --delete --exclude '.git' _site/ ../master/" ""
    .&&. cd "../master"
        *> shell "git rev-parse --git-dir > /dev/null" ""
    .&&. ( fold (inshell "git status --porcelain" "") countLines
        >>= \case
            0 -> return ExitSuccess
            _ -> echoOK "Adding and committing the following changes:"
                .&&. shell "git status" ""
                .&&. shell "git add ." ""
                .&&. with (shell "git commit" "" & fork) wait )
    .&&. echoOK "Pushing pending commits, if any."
    .&&. with (shell "git push gh master" "" & fork) wait

echoOK :: MonadIO io => Line -> io ExitCode
echoOK txt = echo txt *> pure ExitSuccess

data SiteBuilders = SiteBuilders
    { theSiteRules :: H.Rules ()
    , ghIssuesRules :: H.Rules ()
    }

