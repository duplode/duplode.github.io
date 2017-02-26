#!/bin/bash
stack build
cp $(stack path --local-install-root)/bin/duplode-github-io .
