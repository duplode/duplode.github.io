{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Scripts
    ( deploy
    ) where

import qualified Hakyll as H
import System.Environment (withArgs)
import Control.Monad.Managed (with)
import Turtle

deploy :: H.Rules a -> H.Configuration -> IO ExitCode
deploy theSite conf =
    echoOK "Rebuilding and deploying site...\n"
    .&&. withArgs ["rebuild"] (H.hakyllWithExitCode conf theSite)
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

echoOK :: MonadIO io => Text -> io ExitCode
echoOK txt = echo txt *> pure ExitSuccess
