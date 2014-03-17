#!/bin/bash
cabal configure
cabal build
cp dist/build/duplode-github-io/duplode-github-io .
