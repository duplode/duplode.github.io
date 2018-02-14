#!/bin/bash
stack build --ghc-options -threaded
cp $(stack path --local-install-root)/bin/duplode-github-io .
