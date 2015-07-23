#!/bin/bash
stack build # With stack 0.2, add the option --copy-bins.
# This is horrible, but will improve with stack 0.2.
cp .stack-work/install/x86_64-linux/nightly-2015-07-19/7.10.1/bin/duplode-github-io .
