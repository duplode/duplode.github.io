---
title: Casual Hacking With stack, Reloaded
published: 2017-02-26T10:00:00Z
license: CC-BY-SA
gh-issue: 9
---

It has been quite a while since
I [wrote](/posts/casual-hacking-with-stack.html) about how to use stack
for casual play outside of the context of a conventional Haskell
project. In the meantime, stack has gained a feature called the *global
project* which in many cases makes it possible to do quick experiments
with essentially no setup, while still taking advantage of the
infrastructure provided through stack.

<div></div><!--more-->

The global project consists of a `stack.yaml` file and an associated
`.stack-work` directory, which are kept in `~/.stack/global-project`
and are used by stack whenever there is no other `stack.yaml` lying
around. The `stack.yaml` of the global project specifies a resolver,
just like any other `stack.yaml`. If said resolver is a snapshot you
use elsewhere, you get access to all packages you have installed from
that snapshot with zero configuration.

```bash
$ pwd
/home/duplode
$ ls -lrt | grep stack.yaml
$ stack ghci
Configuring GHCi with the following packages: 
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/duplode/.ghci
Loaded GHCi configuration from /tmp/ghci22741/ghci-script
GHCi> import Control.Lens
GHCi> (1,2) ^. _1
1
```

By the way, this also holds for the
stack-powered [Intero](http://commercialhaskell.github.io/intero/)
Emacs mode, which makes it possible to simply open a new `*.hs` file
anywhere and immediately start hacking away.

What about packages you didn't install beforehand? They are no problem,
thanks to the `--package` option of `stack ghci`, which allows
installing snapshot packages at a whim.

```bash
$ stack ghci --package fmlist
fmlist-0.9: download
fmlist-0.9: configure
fmlist-0.9: build
fmlist-0.9: copy/register
Configuring GHCi with the following packages: 
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/duplode/.ghci
Loaded GHCi configuration from /tmp/ghci22828/ghci-script
GHCi> import qualified Data.FMList as FM
GHCi> FM.foldMapA (\x -> show <$> [0..x]) [0..3]
["0000","0001","0002","0003","0010","0011","0012","0013","0020","0021",
"0022","0023","0100","0101","0102","0103","0110","0111","0112","0113",
"0120","0121","0122","0123"]
```

One caveat is that `--package` won't install packages outside of the
snapshot on its own, so you have to add them to the `extra-deps` field
of the global project's `stack.yaml` beforehand, just like you would do
for an actual project. If you need several non-Stackage packages, you
may find it convenient to create a throwaway project for the sole
purpose of letting `stack solver` figure out the necessary `extra-deps`
for you.

```bash
$ mkdir throwaway
$ stack new throwaway --resolver lts-7.14 # Same resolver of the global project.
# ...
Writing configuration to file: throwaway/stack.yaml
All done.
$ cd throwaway
$ vi throwaway.cabal # Let's add reactive-banana to the dependencies.
$ stack solver
# ...
Successfully determined a build plan with 2 external dependencies.

The following changes will be made to stack.yaml:
* Dependencies to be added
    extra-deps:
    - pqueue-1.3.2
    - reactive-banana-1.1.0.1

To automatically update stack.yaml, rerun with '--update-config'
$ vi ~/.stack/global-project/stack.yaml # Add the packages to the extra-deps.
$ cd ..
$ rm -rf throwaway/
$ stack ghci --package reactive-banana
pqueue-1.3.2: configure
pqueue-1.3.2: build
pqueue-1.3.2: copy/register
reactive-banana-1.1.0.1: configure
reactive-banana-1.1.0.1: build
reactive-banana-1.1.0.1: copy/register
Completed 2 action(s).
Configuring GHCi with the following packages: 
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/duplode/.ghci
Loaded GHCi configuration from /tmp/ghci23103/ghci-script
GHCi> import Reactive.Banana
GHCi> :t stepper
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
```

Support for running `stack solver` directly with the global
project
[is on the horizon](https://github.com/commercialhaskell/stack/issues/2656).

There are also interesting possibilities if you need to compile your
throwaway code. That might be useful, for instance, if you ever feel
like testing a hypothesis with
a [criterion](http://www.serpentine.com/criterion/tutorial.html)
benchmark).  While there is a `stack ghc` command, if you don't need
GHC profiles then taking advantage of `--ghci-options` to enable
`-fobject-code` for `stack ghci` can be a more pleasant alternative.

```bash
$ stack ghci --ghci-options "-O2 -fobject-code"
Configuring GHCi with the following packages: 
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/duplode/.ghci
Loaded GHCi configuration from /tmp/ghci23628/ghci-script
GHCi> :l Foo.hs 
[1 of 1] Compiling Foo              ( Foo.hs, /home/duplode/.stack/global-project/.stack-work/odir/Foo.o )
Ok, modules loaded: Foo (/home/duplode/.stack/global-project/.stack-work/odir/Foo.o).
GHCi> :main
A random number for you: 2045528912275320075
```

A nice little thing about this approach is that the build artifacts are
kept in the global project's `.stack-work`, which means they won't
pollute whichever other directory you happen to be at. `-fobject-code`
means you can't write definitions directly on the GHCi prompt; however,
that is not much of a nuisance, given that you are compiling the code
anyway, and that the source file is just a `:!vim Foo.hs` away.

While in these notes I have focused on seat-of-the-pants
experimentation, stack also provides tools for running Haskell code
with minimal configuration in a more controlled manner. I specially
recommend having a look at
the
[*script interpreter* section of the stack User Guide](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter).
