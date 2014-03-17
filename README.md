duplode.github.io
=================

Sources branch for duplode.github.io.

Build instructions
------------------

The build scripts assume two different repositories within one parent
directory are used for the `master` and `sources` branch:

    duplode.github.io
        duplode.github.io/master
            duplode.github.io/master/.git
        duplode.github.io/sources
            duplode.github.io/sources/.git

The site is generated using [Hakyll](http://jaspervdj.be/hakyll/).

Build commands and scripts (all should be ran from the `sources` root
directory):

    # Installs Hakyll and other Haskell dependencies.
    cabal install --only-dependencies

    # Compiles the Hakyll executable and copies it to the sources root.
    ./build-hs.sh

    # Compiles the Hakyll executable and rebuilds the site from scratch.
    # Use it after changes in the executable.
    ./full-build.sh

    # After compiling the Hakyll executable, builds the site incrementally.
    # See the Hakyll documentation for other commands.
    ./duplode-github-io build

    # Transfers the built site to the master root using rsync.
    # Afterwards, you should commit the updates at the master clone.
    ./synchronise.sh

Repository layout
-----------------

The initial repository layout can be achieved as follows (procedure based on
[this tutorial by Chris Jacob](https://gist.github.com/chrisjacob/833223)):

    mkdir -p duplode.github.io/master
    cd duplode.github.io/master
    git init
    touch .gitignore
    git add .
    git commit -m "Initial commit."
    git remote add gh git@github.com:duplode/duplode.github.io
    git push gh master

    cd ..
    git clone -o unrelated master sources
    cd sources
    git checkout --orphan sources
    git commit -m "Initial commit (sources)."
    git branch -d master
    git push gh sources
