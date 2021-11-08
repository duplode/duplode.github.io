#!/bin/bash

# Note that stack.yaml is currently outdated.
# Last Stack-compatible revision: 9b38390ac20ef03020392796ed97d77f7fb61f98

./build-hs.sh
cabal v2-run duplode-github-io -- rebuild
