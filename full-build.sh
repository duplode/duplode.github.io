#!/bin/bash

# Stack-centric script. Note that stack.yaml is currently outdated.
# Last compatible revision: 9b38390ac20ef03020392796ed97d77f7fb61f98

./build-hs.sh
cabal new-run duplode-github-io -- rebuild
