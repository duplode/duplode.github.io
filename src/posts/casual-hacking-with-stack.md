---
title: Casual Hacking With stack
published: 2015-07-22T23:30:00-03:00
license: CC-BY-SA
reddit: 3e9xkn
gh-issue: 6
retired: true
---

*2017 update: This post was written shortly after the initial release
of stack. While the workflow suggested here remains potentially
useful, later versions of stack offer more immediate support for
working outside the boundaries of a conventional Haskell project. For
more on that,
see
[Casual Hacking With stack, Reloaded](/posts/casual-hacking-with-stack-reloaded.html).*

Sandboxes are exceptionally helpful not just for working in long-term
Haskell projects, but also for casual experiments. While playing around,
we tend to install all sorts of packages in a carefree way, which
increases a lot the risk of entering cabal hell. While vanilla
cabal-install sandboxes prevent such a disaster, using them
systematically for experiments mean that, unless you are meticulous, you
will end up either with dozens of .hs files in a single sandbox or with
dozens of copies of the libraries strewn across your home directory.
And no one likes to be meticulous while playing around. In that context,
stack, the recently released alternative to cabal-install, can prevent
trouble with installing packages in a way more manageable than through
ad-hoc sandboxes. In this post, I will suggest a few ways of using stack
that may be convenient for experiments. I have been using stack for only
a few days, therefore suggestions are most welcome!

<div></div><!--more-->

I won't dwell on the motivation and philosophy behind stack [^why].
Suffice it to say that, at least in the less exotic workflows, there is
a centralised package database somewhere in `~/.stack` with packages
pulled from a [Stackage](https://www.stackage.org/) snapshot (and
therefore known to be compatible with each other), which is supplemented
by a per-project database (that is, just like cabal sandboxes) for
packages not in Stackage (from Hackage or anywhere else). As that sounds
like a great way to avoid headaches, we will stick to this arrangement,
with only minor adjustments.

[^why]: For that, see
[Why is stack not cabal?](https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal),
written by a member of its development team.

Once you have installed stack [^install], you can create a new
environment for experiments with `stack new`:

[^install]: For installation guidance, see the
[GitHub project wiki](https://github.com/commercialhaskell/stack/wiki/Downloads).
Installing stack is easy, and there are many ways to do it (I simply
got it from Hackage with `cabal install stack`).

```
$ mkdir -p Development/haskell/playground
$ cd Development/haskell/playground
$ stack new --prefer-nightly
```

The `--prefer-nightly` option makes stack use a nightly snapshot of
Stackage, as opposed to a long term support one. As we are just playing
around, it makes sense to pick as recent as possible packages from the
nightly instead of the LTS. (Moreover, I use Arch Linux, which already
has GHC 7.10 and `base` 4.8, while the current LTS snapshot assumes
`base` 4.7.) If this is the first time you use stack, it will pick the
latest nightly; otherwise it will default to whatever nightly you
already have in `~/.stack`.

`stack new` creates a neat default project structure for you [^init]:

[^init]: To create an environment for an existing project, with its own
structure and cabal file, you would use `stack init` instead.

```
$ ls -R
.:
app  LICENSE  new-template.cabal  Setup.hs  src  stack.yaml  test

./app:
Main.hs

./src:
Lib.hs

./test:
Spec.hs
```

Of particular interest is the `stack.yaml` file, which holds the
settings for the local stack environment. We will talk more about it
soon.

``` yaml
flags: {}
packages:
- '.'
extra-deps: []
resolver: nightly-2015-07-19
```

As for the default `new-template.cabal` file, you can use its
`build-depends` section to keep track of what you are installing. That
will make `stack build` (the command which builds the current project
without installing it) to download and install any dependencies you add
to the cabal file automatically. Besides that, having the installed
packages noted down may prove useful in case you need to reproduce your
configuration elsewhere [^ghc-pkg]. If your experiments become a real
project, you can clean up the `build-depends` without losing track of
the packages you installed for testing purposes by moving their entries
to a second cabal file, kept in a subdirectory:

[^ghc-pkg]: In any case, you can also use `stack exec -- ghc-pkg list`
to see all packages installed from the snapshot you are currently using.
That, however, will be far messier than the `build-depends` list, as it
will include indirect dependencies as well.

```
$ mkdir xp
$ cp new-template.cabal xp/xp.cabal
$ cp LICENSE xp # Too lazy to delete the lines from the cabal file.
$ cd xp
$ vi Dummy.hs # module Dummy where <END OF FILE>
$ vi xp.cabal # Adjust accordingly, and list your extra deps.
```

You also need to tell stack about this fake subproject. All it takes is
adding an entry for the subdirectory in `stack.yaml`:

``` yaml
packages:
- '.' # The default entry.
- 'xp'
```

With the initial setup done, we use `stack build` to compile the
projects:

```
$ stack build
new-template-0.1.0.0: configure
new-template-0.1.0.0: build
fmlist-0.9: download
fmlist-0.9: configure
fmlist-0.9: build
new-template-0.1.0.0: install
fmlist-0.9: install
xp-0.1.0.0: configure
xp-0.1.0.0: build
xp-0.1.0.0: install
Completed all 3 actions.
```

In this test run, I added `fmlist` as a dependency of the fake package
`xp`, and so it was automatically installed by stack. The output of
`stack build` goes to a `.stack-work` subdirectory.

With the packages built, we can use GHCi in the stack environment with
`stack ghci`. It loads the library source files of the current project
by default:

``` haskell
$ stack ghci
Configuring GHCi with the following packages: new-template, xp
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              (
/home/duplode/Development/haskell/playground/src/Lib.hs, interpreted )
[2 of 2] Compiling Dummy            (
/home/duplode/Development/haskell/playground/xp/Dummy.hs, interpreted )
Ok, modules loaded: Dummy, Lib.
*Lib> import qualified Data.FMList as F -- Which we have just installed.
*Lib F> -- We can also load executables specified in the cabal file.
*Lib F> :l Main
[1 of 2] Compiling Lib              (
/home/duplode/Development/haskell/playground/src/Lib.hs, interpreted )
[2 of 2] Compiling Main             (
/home/duplode/Development/haskell/playground/app/Main.hs, interpreted )
Ok, modules loaded: Lib, Main.
*Main F>
```

Dependencies not in Stackage have to be specified in `stack.yaml` as
well as in the cabal files, so that stack can manage them too.
Alternative sources of packages include source trees in subdirectories
of the project, Hackage and remote Git repositories [^git]:

[^git]: For the latter, see
[the project wiki](https://github.com/commercialhaskell/stack/wiki/Nonstandard-project-initialization).

``` yaml
flags: {}
packages:
- '.'
- 'xp'
- location: deps/acme-missiles-0.3 # Sources in a subdirectory.
  extra-dep: true # Mark as dep, i.e. not part of the project proper.
extra-deps:
- acme-safe-0.1.0.0 # From Hackage.
- acme-dont-1.1 # Also from Hackage, dependency of acme-safe.
resolver: nightly-2015-07-19
```

`stack build` will then install the extra dependencies to
`.stack-work/install`. You can use `stack solver` to chase the indirect
dependencies introduced by them. For instance, this is its output after
commenting the `acme-dont` line in the `stack.yaml` just above:

```
$ stack solver --no-modify-stack-yaml
This command is not guaranteed to give you a perfect build plan
It's possible that even with the changes generated below, you will still
need to do some manual tweaking
Asking cabal to calculate a build plan, please wait
extra-deps:
- acme-dont-1.1
```

To conclude this tour, once you get bored of the initial Stackage
snapshot all it takes to switch it is changing the `resolver` field in
`stack.yaml` (with nightlies, that amounts to changing the date at the
end of the snapshot name). That will cause all dependencies to be
downloaded and built from the chosen snapshot when `stack build` is next
ran. As of now, the previous snapshot will remain in `~/.stack` unless
you go there and delete it manually; however, a command for removing
unused snapshots
[is in the plans](https://github.com/commercialhaskell/stack/issues/133).

I have not tested the sketch of a workflow presented here extensively,
yet what I have seen was enough to convince me stack can provide a
pleasant experience for casual experiments as well as full-fledged
projects. Happy hacking!

**Update:** There is now a follow-up post about the other side of the
coin,
[Migrating a Project to stack](/posts/migrating-a-project-to-stack.html).
