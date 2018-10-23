---
title: Migrating a Project to stack
published: 2015-07-27T03:00:00-03:00
license: CC-BY-SA
reddit: 3fm8vp
gh-issue: 7
---

This post consists of notes on how I converted one of my Haskell
projects to stack. It provides a small illustration of how flexible
stack can be in accomodating project organisation quirks on the way
towards predictable builds.<!--more--> If you want to see the complete
results, here are links to the
[Bitbucket repository](https://bitbucket.org/duplode/stunts-cartography)
of Stunts Cartography, the example project I am using, and specifically
to the
[source tree immediately after the
migration](https://bitbucket.org/duplode/stunts-cartography/src/3eb07c44f6e2eed19591f765b14fc5fbe2b8f946).

The first decision to make when migrating a project is which Stackage
snapshot to pick. It had been a while since I last updated my project,
and building it with the latest versions of all its dependencies would
require a few adjustments. That being so, I chose to migrate to stack
before any further patches. Since one of the main dependencies was
`diagrams` 1.2, I went for [`lts-2.19`](
https://www.stackage.org/lts-2.19), the most recent LTS snapshot with
that version of `diagrams` [^why-lts].

[^why-lts]: As a broader point, it just seems polite to, when possible,
pick a LTS snapshot over than a nightly for a public project. It is
more likely that those interested in building your project already have
a specific LTS rather than an arbitrary nightly.

```
$ stack init --resolver lts-2.19
```

`stack init` creates a `stack.yaml` file based on an existing cabal file
in the current directory. The `--resolver` option can be used to pick a
specific snapshot.

One complicating factor in the conversion to stack was that two of the
extra dependencies, `threepenny-gui-0.5.0.0` (one major version behind
the current one) and `zip-conduit`, wouldn't build with the LTS snapshot
plus current Hackage without version bumps in their cabal files.
Fortunately, stack deals very well with situations like this, in which
minor changes to some dependency are needed. I simply forked the
dependencies on GitHub, pushed the version bumps to my forks and
referenced the commits in the *remote* GitHub repository in
`stack.yaml`. A typical entry for a Git commit in the `packages` section
looks like this:

``` yaml
- location:
    git: https://github.com/duplode/zip-conduit
    commit: 1eefc8bd91d5f38b760bce1fb8dd16d6e05a671d
  extra-dep: true
```

Keeping customised dependencies in public remote repositories is an
excellent solution. It enables users to build the package without
further intervention without requiring developers to clumsily bundle the
source tree of the dependencies with the project, or waiting for a pull
request to be accepted upstream and reach Hackage.

With the two tricky extra dependencies being offloaded to Git
repositories, the next step was using `stack solver` to figure out the
rest of them:

```
$ stack solver --modify-stack-yaml
This command is not guaranteed to give you a perfect build plan
It's possible that even with the changes generated below, you will still
need to do some manual tweaking
Asking cabal to calculate a build plan, please wait
extra-deps:
- parsec-permutation-0.1.2.0
- websockets-snap-0.9.2.0
Updated /home/duplode/Development/stunts/diagrams/stack.yaml
```

Here is the final `stack.yaml`:

``` yaml
flags:
  stunts-cartography:
    repldump2carto: true
packages:
- '.'
- location:
    git: https://github.com/duplode/zip-conduit
    commit: 1eefc8bd91d5f38b760bce1fb8dd16d6e05a671d
  extra-dep: true
- location:
    git: https://github.com/duplode/threepenny-gui
    commit: 2dd88e893f09e8e31378f542a9cd253cc009a2c5
  extra-dep: true
extra-deps:
- parsec-permutation-0.1.2.0
- websockets-snap-0.9.2.0
resolver: lts-2.19
```

`repldump2carto` is a flag defined in the cabal file. It is used to
build a secondary executable. Beyond demonstrating how the `flags`
section of `stack.yaml` works, I added it because `stack ghci` expects
all possible build targets to have been built [^subprojects].

[^subprojects]: That being so, a more natural arrangement would be
treating `repldump2carto` as a full-blown subproject by giving it its
own cabal file and adding it to the `packages` section. I would then be
able to load only the main project in GHCi with `stack ghci
stunts-cartography`.

As I have GHC 7.10.1 from my Linux distribution and the LTS 2.19
snapshot is made for GHC 7.8.4, I needed `stack setup` as an additional
step. That command locally installs (in `~/.stack`) the GHC version
required by the chosen snapshot.

That pretty much concludes the migration. All that is left is
demonstrating: `stack build` to compile the project...

```
$ stack build
JuicyPixels-3.2.5.2: configure
Boolean-0.2.3: download
# etc. (Note how deps from Git are handled seamlessly.)
threepenny-gui-0.5.0.0: configure
threepenny-gui-0.5.0.0: build
threepenny-gui-0.5.0.0: install
zip-conduit-0.2.2.2: configure
zip-conduit-0.2.2.2: build
zip-conduit-0.2.2.2: install
# etc.
stunts-cartography-0.4.0.3: configure
stunts-cartography-0.4.0.3: build
stunts-cartography-0.4.0.3: install
Completed all 64 actions.
```

... `stack ghci` to play with it in GHCi...

``` haskell
$ stack ghci
Configuring GHCi with the following packages: stunts-cartography
GHCi, version 7.8.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
-- etc.
Ok, modules loaded: GameState, Annotation, Types.Diagrams, Pics,
Pics.MM, Annotation.Flipbook, Annotation.LapTrace,
Annotation.LapTrace.Vec, Annotation.LapTrace.Parser.Simple,
Annotation.Parser, Types.CartoM, Parameters, Composition, Track,
Util.Misc, Pics.Palette, Output, Util.ByteString, Util.ZipConduit,
Replay, Paths, Util.Reactive.Threepenny, Util.Threepenny.Alertify,
Widgets.BoundedInput.
*GameState> :l src/Viewer.hs -- The Main module.
-- etc.
*Main> :main
Welcome to Stunts Cartography.
Open your web browser and navigate to localhost:10000 to begin.

Listening on http://127.0.0.1:10000/
[27/Jul/2015:00:55:11 -0300] Server.httpServe: START, binding to
[http://127.0.0.1:10000/]
```

... and looking at the build output in the depths of `.stack-work`:

```
$ .stack-work/dist/x86_64-linux/Cabal-1.18.1.5/build/sc-trk-viewer/sc-trk-viewer
Welcome to Stunts Cartography 0.4.0.3.
Open your web browser and navigate to localhost:10000 to begin.

Listening on http://127.0.0.1:10000/
[26/Jul/2015:20:02:54 -0300] Server.httpServe: START, binding to
[http://127.0.0.1:10000/]
```

With the upcoming stack 0.2 it will be possible to use `stack build
--copy-bins --local-bin-path <path>` to copy any executables built as
part of the project to a path. If the `--local-bin-path` option is
omitted, the default is `~/.local/bin`. (In fact, you can already copy
executables to `~/.local/bin` with stack 0.1.2 through `stack install`.
However, I don't want to overemphasise that command, as `stack install`
not being equivalent to `cabal install` can cause some confusion.)

Hopefully this report will give you an idea of what to expect when
migrating your projects to stack. Some details may appear a little
strange, given how familiar cabal-install workflows are, and some
features are still being shaped. All in all, however, stack works very
well already: it definitely makes setting up reliable builds easier.
The [stack repository at GitHub](https://github.com/commercialhaskell/stack),
and specially [the wiki therein](https://github.com/commercialhaskell/stack/wiki),
offers lots of helpful information, in case you need further details
and usage tips.
