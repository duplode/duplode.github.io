---
title: "Distributive Draft"
license: CC-BY-SA
published: 2021-11-30T22:00:00-03:00
gh-issue: 17
toc: true
---

[`Distributive`](
https://hackage.haskell.org/package/distributive-0.6.2.1/docs/Data-Distributive.html)
is a class that goes under many bynames: "dual to `Traversable`",
"representable functors", "isomorphic to functions", and so forth. In
this post, I will explore what these descriptions have to do with each
other. I will also use the opportunity to put down to paper a handful of
proofs on this matter that I always seem to end up deriving over and
over again.

<!-- more -->

Before we begin, here is my go-to self-contained example functor for
playing with `Distributive`: an homogeneous pair (or length-two vector,
if you will) type:

``` haskell
data Duo a = Duo { fstDuo :: a, sndDuo :: a }
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)
```

## Dualising Traversable

Out of the many possible starting points, let's begin our journey from
the purported duality. `Traversable` is:

``` haskell
sequenceA :: (Traversable g, Applicative f) => g (f a) -> f (g a)

-- Laws:

-- Identity:
-- sequenceA . fmap Identity = Identity

-- Composition:
-- sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA

-- Naturality (comes for free, by parametricity):
-- Given Applicative f, Applicative h, and t :: forall a. f a -> h a
-- such that t preserves pure and (<*>), we have:
-- sequenceA . fmap t = t . sequenceA
```

Dualising the type of `sequenceA` would give us:
[^duality]

[^duality]: To be precise, sequenceA is a natural transformation between
  **Hask** endofunctors, that is, Haskell `Functor`s (more specifically,
  from `Compose g f` to `Compose f g`). Its dual is a natural
  transformation between **Hask**<sup>op</sup> endofunctors.
  **Hask**<sup>op</sup>, the opposite category, has Haskell functions as
  arrows, but with source and target swapped, so an `a` to `b` arrow in
  it amounts to a `b -> a` Haskell function. **Hask**<sup>op</sup>
  endofunctors are the usual Haskell `Functor`s. That being so, if we
  dualise:

    ``` haskell
    g (f a) -> f (g a)
    ```

    We get (using `-<` for **Hask**<sup>op</sup> arrows):

    ``` haskell
    g (f a) -< f (g a)
    ```

    Which, in **Hask**, boils down to:

    ``` haskell
    f (g a) -> g (f a)
    ```

    For an approchable explanation of duality in Haskell, see Edward Z.
    Yang's classic [*Flipping arrows in coBurgerKing*](
    http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/).


``` haskell
-- We will figure out the appropriate constraint for f in a moment.
distribute :: (Distributive g, _ f) => f (g a) -> g (f a)
```

Now let's consider what implementing `distribute` might be like. In the
dual case of `sequenceA`, we know all implementations look pretty much
the same: separate the contents from the shape of the traversable structure,
combine the applicative effects of the contents in some specific order,
and recreate the original shape under the applicative layer:

``` haskell
instance Traversable Duo where
    sequenceA m = Duo <$> fstDuo m <*> sndDuo m
```

With `Traversable`, we push the traversable `g` shape down `f` the
applicative layer. Dually, `Distributive` asks us to pull the
distributive shape `g` out of the `f` layer. Importantly, this only
makes sense if there is only one possible shape for the functor `g`:
otherwise, we would have to figure out a way to combine any different
shapes that might lie under the `f` layer in `f (g a)`, which would end
up with us reinventing `Applicative` and `Traversable`. With a single
shape, though, we can recreate it on the outside, no questions asked:

``` haskell
instance Distributive Duo where
    distribute u = Duo (fstDuo <$> u) (sndDuo <$> u)
```

Note the `f` functorial layer gets duplicated as many times as necessary
to fill in the positions of the `g` shape. Ordinary Haskell values can
be duplicated at will, so nothing more than `Functor` is necessary for
`f`.  [^coapplicative]

[^coapplicative]: That is what the `Data.Distributive` documentation
  means with "Due to the lack of non-trivial comonoids in Haskell, we
  can restrict ourselves to requiring a `Functor` rather than some
  `Coapplicative` class".

``` haskell
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
```

Accordingly, the laws also get flipped:

``` haskell
-- Identity:
-- fmap runIdentity . distribute = runIdentity

-- Composition:
-- fmap getCompose . distribute = distribute . fmap distribute . getCompose

-- Naturality (comes for free, by parametricity):
-- Given Functor f, Functor h, and t :: forall a. f a -> h a
-- fmap t . distribute = distribute . t
```

The property of having a single shape appears to be intimately connected
to `Distributive`. Let's see if we can explore that property to tease
out some of the connections between distributive functors and other
concepts.

## Single-shapedness

One of the points in our discussion of `distribute` in the first section
is that it only made sense for functors with a single shape. That being
the case, though, it looks reasonable to try reconstructing
`Distributive` starting from single-shapedness. Let's see where such a
bet will take us: hopefully, it will help us teasing out some of the
connections between distributive functors and other concepts.

The first thing we need for the task at hand is pinning down the notion
of single-shapedness. Here is one possible approach: approach: if we
know that a functor `g` has only one possible shape, all there remains
to learn about some `u :: g a` value is which `a` values can be found in
each position of the shape. To discover that, we can use `forall x. g x
-> x` extractor functions, however many there might be.  That, however,
means there is an isomorphism between `g a` and the following type:

``` haskell
-- For Distributive g, isomorphic to g a:
(forall x. g x -> x) -> a
```

If every extractor function gives us a value, we can obtain all values
and reconstruct the distributive structure.

For the sake of convenience, I will introduce a type synonym:
[^impredicativetypes]

[^impredicativetypes]: As written, these examples are meant for GHC 9.2
  or above, with `ImpredicativeTypes` turned on. For earlier GHC
  versions, you'd probably want to use `newtype` instead of `type` in
  order to avoid needing that extension.

``` haskell
-- A Pos g extracts a value from some position:
type Pos g = forall x. g x -> x
```

One direction of the isomorphism is just (flipped) function application,
and doesn't impose any constraints on `g`:

``` haskell
evert :: g a -> Pos g -> a
evert u = \p -> p u
```

That being so, it should be possible to define `Distributive` in terms
of the other direction of the isomorphism. Let's give it a go:

``` haskell
class Functor g => Distributive g where
    revert :: (Pos g -> a) -> g a
```

The laws, then, would merely state that `evert` and `revert` are halves
of an isomorphism:

``` haskell
revert . evert = id  -- "Resident" direction: from g a and back
evert . revert = id  -- "Visitor" direction: to g a and back
```

`Duo` can be made an instance of this `Distributive` class:

``` haskell
instance Distributive Duo where
    revert e = Duo (e fstDuo) (e sndDuo)
```

Functions are one especially important instance of `Distributive`. With
respect to our tentative formulation, we might note that `Pos ((->) r) ~
forall x. (r -> x) -> x` is isomorphic to `r`, and so `Pos r -> a` must
be isomorphic to `r -> a`:

``` haskell
instance Distributive ((->) r) where
    revert e = \r -> e (\f -> f r)
```

We still have, of course, to check whether this newfangled
`Distributive` class actually corresponds to the traditional one.

## Putting the distribute in Distributive

There is something of a general strategy to make use of `revert`.
Schematically:

``` haskell
revert (\p -> _)
```

By filling the blank (that is, the body of the `Pos g -> a` function
supplied to `revert`), we specify the value at each position of the
result structure. For doing so, we have access to the extractor for the
position, `p`, which can be used to draw values from matching positions
in other `g`-shaped distributive structures.  The upshot is that many of
combinators for distributive functors can be written using `revert` in a
way that is very easy on the eyes.  Here are some examples:

``` haskell
-- Apply a function to the value at each position.
fmapD :: Distributive g => (a -> b) -> g a -> g b
fmapD f u = revert (\p -> f (p u))

-- Use the same value for every position.
pureD :: Distributive g => a -> g a
pureD a = revert (\_ -> a)

-- Pick a function and a value from matching positions, and apply one
-- to the other.
apD :: Distributive g => g (a -> b) -> g a -> g b
apD u v = revert (\p -> p u (p v))

-- Take the diagonal by choosing the same position twice.
joinD :: Distributive g => g (g a) -> g a
joinD u = revert (\p -> p (p u))

-- Pick values from matching positions and combine them.
mappendD :: (Distributive g, Semigroup m) => g a -> g a -> g a
mappendD u v = revert (\p -> p u <> p v)
```

Crucially, we can also obtain `distribute` in this manner:

``` haskell
-- Under the f layer, draw the value at the matching position.
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
distribute m = revert (\p -> p <$> m)
```

Is this `distribute` lawful, though? Happily, the answer is yes: if
`distribute` is defined in this manner, `revert . evert = id` is
equivalent to the identity law, while the composition law follows from
`evert . revert = id`. A proof of this result can be found in the
appendix to this post.

### Through the looking glass

The definition of `distribute` in terms of `revert` given just above can
be tweaked into the following pointfree form:

``` haskell
-- See the appendix for a proof.
distribute = revert . flap . fmap evert
```

Here, `flap` is a [reasonably familiar](
https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Functor-Fmap.html#v:flap),
combinator, [also known as `(??)`](
https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Lens.html#v:-63--63-),
which turns `f (r -> a)` into `a -> f r` by supplying a common argument
to all the functions. The name is a play on it being a generalisation of
`flip`:

``` haskell
flap :: Functor f => f (r -> a) -> r -> f a
flap m r = (\f -> f r) <$> m
```

It turns out that `flap` is `distribute` for functions. That being so,
the implementation of `distribute` offered here amounts to:

- Changing `g` into `(->) (Pos g)` through `fmap evert`;

- Distributing it, as a function, with `flap`; and

- Restoring the `g` layer on the outside with `revert`.

Now, remember the informal description of `sequenceA` from the beginning
of the post? We can state it in a very similar fashion:

- Extract, in a specific order, the `f a` contents of a `t (f a)`;

- Following that order, sequence the `f a` contents, combining the
  applicative effects; and

- Restore the original `t` shape under the combined `f` layer.

There are a few ways to make an actual implementation of `sequenceA` out
of that description. For the sake of simplicity, let's stick to the one
which extracts the contents into lists. Though a bit rough around the
edges, it suits our purposes just fine:

``` haskell
-- A hollowed-out shape, paired with a list of contents.
data Decomp t a = Decomp (t ()) [a]

detach :: (Functor t, Foldable t) => t a -> Decomp t a
detach u = Decomp (() <$ u) (toList u)

-- Given a compatible detach, implementing fill is enough to set up a
-- Traversable instance. Merely for the sake of illustration, below is
-- a definition in the other direction, in terms of the usual
-- Traversable class.
fill :: Traversable t => Decomp t a -> t a
fill (Decomp sh as) = snd (mapAccumL (\(a : as) _ -> (as, a)) as sh)

-- "Resident" direction of the isomorphism:
-- fill (detach u) = u
-- "Visitor" direction of the isomorphism:
-- Precondition: length sh = length as
-- detach (fill (Decomp sh as)) = Decomp sh as

-- The familiar sequenceA for lists.
sequenceList :: Applicative f => [f a] -> f [a]
sequenceList = foldr (liftA2 (:)) (pure [])

-- sequenceList, adapted to Decomp.
sequenceDecomp :: Applicative f => Decomp t (f a) -> f (Decomp t a)
sequenceDecomp (Decomp sh ms) = Decomp sh <$> sequenceList ms

-- sequenceA, in terms of detach and fill.
sequenceFill :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceFill = fmap fill . sequenceDecomp . detach
```

Here's the kicker, though: this definition of `sequenceA` perfectly
mirrors the pointfree definition of `distributive` we saw above! Here,
we have used an isomorphism to get a list, which is straightforward to
sequence, out of the traversable structure; there, we have used an
isomorphism to get a function, which is straightforward to distribute,
out of the distributive structure. Fittingly, proving the `revert`-based
`distribute` is lawful, as done in the appendix, can be done in a very
similar way to how the lawfulness of the `fill`-based `sequenceA` is
proven. [^shape-and-contents]

[^shape-and-contents]: For a detailed account of the shape-and-contents
  presentation of `Traversable`, see [this Stack Overflow answer I wrote
  a while ago](https://stackoverflow.com/a/61199603/2751851).

### The other way around

We have defined `distribute` using `revert`, but what about getting
`revert` out of `distribute`? Ideally, we'd like to have a definition in
the other direction to establish our `Distributive` is equivalent to the
conventional one. Encouragingly, there's a cute trick that can take us
most of the way:

``` haskell
-- Here, id :: g a -> g a
distribute id :: Distribute g => g (g a -> a)
```

In `distribute id`, each position in the `g` shape holds its
corresponding `g a -> a` extractor. We can use it to get something
that looks a lot like `revert`:

``` haskell
revertRank1 :: Distributive g => ((g b -> b) -> a) -> g a
revertRank1 e = e <$> distribute id
```

Exactly how close are we? One limitation of `revertRank1` that readily
shows up is that if we try to use for the style of definition we have
seen earlier, `revertRank1 (\p -> _)`, the lessened polymorphism of the
type of `p` will stop us from instantiating it at different types.  That
makes certain implementations more complicated than they might be, `apD`
being a good example if you feel like trying it out.

Unfortunately, `distribute` gives us no way to bring in the extra
polymorphism in the extractor type, and so we end up with `g b -> b`
instead of `Pos g ~ forall x. g x -> x`. What we'd really want to have
would be `revert id :: Distribute g => g (forall x. g x -> x)` instead
of `distribute id`, but of course that takes `revert` for granted. And
yet, the extractors made available by `distribute id` are the same ones
given by `revert id`, except for the difference in their types (a proof
of that can be found in the appendix).

Is our `Distributive` class more powerful than the one from
`Data.Distributive`, then? It certainly looks like so, given that
`distribute` doesn't give us a proper `revert`. It is tempting to argue
that the difference is trivial, only existing because the type system
can't give `distribute id` the stronger type it should rightfully have.
There is one caveat, though: there being a function with the type of
`distribute` for some functor is no guarantee that it can be decomposed
into `evert`, `flap` and `revert`. That being so, before using any
results based on `revert` it is prudent to show it actually exists for
the functor we are working with. [^select]

[^select]: One example of this kind of pitfall is [`Select`](
  https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Select.html).
  As I originally [learned from Sergei Winitzki](
  https://stackoverflow.com/a/39736535/2751851), `Select` has a not
  entirely implausible candidate for `distribute`:

    ``` haskell
    distributish :: Functor f => f (Select r a) -> Select r (f a)
    distributish m = select $ \k ->
        (\u -> u `runSelect ` (\a -> k (const a <$> m))) <$> m
    ```

  `distributish` does not arise out of any `revert`, rank-one or
  otherwhise. The decomposition is impossible because the implementation
  relies on a `k :: f a -> r` criterion
  function being eventually supplied by the caller, which goes against the
  entire idea of using polymorphic extractors known in advance. In any case,
  `Select r` is not actually distributive, as `distributish` only
  follows the identity law, and not the composition one.



## Appendix: proofs

TODO: Double-checking.

Let's begin by collecting the basic premises in a single place:

``` haskell
type Pos g = forall x. g x -> x

evert :: g a -> Pos g -> a
evert u = \p -> p u

-- Core assumption: every distributive has a revert.
revert :: Distributive g => (Pos g -> a) -> g a

-- Laws for revert:
-- evert and revert are halves of an isomorphism.
revert . evert = id  -- "Resident" direction: from g a and back
evert . revert = id  -- "Visitor" direction: to g a and back

-- distribute can be defined in terms of revert:
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
distribute m = revert (\p -> p <$> m)

-- Abbreviations
I = Identity
uI = runIdentity
C = Compose
uC = getCompose

-- Laws for distribute:

-- Identity
fmap uI . distribute = uI

-- Composition:
fmap uC . distribute = distribute . fmap distribute . uC

-- Naturality law of distribute (ensured by parametricity):
-- Given Functor f, Functor h, and t :: forall a. f a -> h a
fmap t . distribute = distribute . t

-- Auxiliary definitions:

instance Distributive ((->) r) where
    revert e = \r -> e (\f -> f r)

flap :: Functor f => f (r -> a) -> r -> f a
flap m r = (\f -> f r) <$> m

-- fflip = distribute @_ @((->) _)
fflip :: Distributive g => (r -> g a) -> g (r -> a)
fflip m = revert (\p -> p . m)
```

### Pointfree definition of distribute

The following way to define `distribute` is better suited for the kind
of derivation we will go through shortly.

``` haskell
\m -> distribute m
\m -> revert (\p -> p <$> m)
\m -> revert (\p -> (\u -> p u) <$> m)
\m -> revert (\p -> (\u -> evert u p) <$> m)
\m -> revert (\p -> (\u -> (\s -> s p) (evert u)) <$> m)
\m -> revert (\p -> ((\s -> s p) . evert <$> m)
\m -> revert (\p -> (\s -> s p) <$> (evert <$> m))
\m -> revert (flap (evert <$> m))
revert . flap . fmap evert

--Therefore:
distribute = revert . flap . fmap evert
```

An useful consequence follows from this alternative definition:

``` haskell
-- Corollary:
-- If evert . revert = id, then:
distribute . fmap revert = revert . flap
evert . distribute = flap . fmap evert
```

### Properties of flap

Being part of our chosen definition of `distribute`, `flap` will play an
important role in the main proofs. Here are a few useful properties it
has:

`flap` is `distribute` for functions:

``` haskell
distribute @((->) _)
\m -> revert @((->) _) (\p -> p <$> m)
\m -> \r -> (\p -> p <$> m) (\f -> f r)
\m -> \r -> (\f -> f r) <$> m
flap

-- Verification of the laws:
-- Identity:
fmap uI . flap = uI  -- Goal
fmap uI . flap  -- LHS
fmap UI . (\m r -> (\f -> f r) <$> m)
\m r -> uI ((\f -> f r) <$> m)
\m r -> (\f -> f r) (uI m)
\m r -> uI m r
uI  -- LHS = RHS

-- Composition:
fmap uC . flap = flap . fmap flap . uC  -- Goal
fmap uC (flap m) = flap (fmap flap (uC m))  -- Goal, pointfully
flap (fmap flap (uC m))  -- RHS
(\m r -> (\f -> f r) <$> m) ((\m r -> (\f -> f r) <$> m) <$> uC m)
\r -> ((\f -> f r) . (\m r -> (\f -> f r) <$> m) <$> uC m)
\r -> ((\m -> (\f -> f r) <$> m) <$> uC m)
\r -> uC ((\f -> f r) <$> m)
uC . (\r -> ((\f -> f r) <$> m))
uC . flap m
fmap uC (flap m)  -- LHS = RHS
```

`flap` at `Identity` and `Compose`:

``` haskell
-- flap m r = (\f -> f r) <$> m
flap @I
\(I f) r -> (\f -> f r) <$> I f  -- m = I f
\(I f) r -> I (f r)
\m r -> I (uI m r)
(I .) . uI
fmap I . uI

flap @C
\(C w) r -> (\f -> f r) <$> C w  -- m = C w
\(C w) r -> C (fmap (\f -> f r) <$> w)
\m r -> C (fmap (\f -> f r) <$> uC m)
\m -> (C .) (\r -> fmap (\f -> f r) <$> uC m)
\m -> (C .) (\r -> (\w -> flap w r) <$> uC m)
\m -> (C .) (\r -> (\f -> f r) . flap <$> uC m)
\m -> (C .) (\r -> (\f -> f r) <$> (flap <$> uC m))
fmap C . flap . fmap flap . uC

-- Therefore:
flap @I = fmap I . uI
flap @C = fmap C . flap . fmap flap . uC
```

`flap . flap = id`:

``` haskell
-- flap m r = (\f -> f r) <$> m
flap @((->) _)
\m r -> (\f -> f r) . m
\m r -> (\f -> f r) . (\s -> m s)
\m r s -> m s r
flip

-- Therefore:
flap @((->) _) = flip

-- Corollary:
flap . flap = id
```

If `revert . evert = id`, then `flap` is injective:

``` haskell
-- Given:
revert . evert = id
-- Goal:
fflip . flap = id

fflip (flap m) = m  -- Goal, pointfully
fflip (flap u)  -- LHS
revert (\p -> p . flap m)
revert (\p -> p . (\r -> (\f -> f r) <$> m))
revert (\p -> \r -> p ((\f -> f r) <$> m))
-- p is a natural transformation
revert (\p -> (\r -> (\f -> f r) (p m)))
revert (\p -> (\r -> p m r))
revert (\p -> p m)
revert (evert m)
m  -- LHS = RHS, reversibly

-- fflip . flap = id iff revert . evert = id

-- Corollary:
-- If revert . evert = id, fflip is the left inverse of flap; therefore,
-- flap is injective.
```

### The identity law

And now, the main course.

The identity law holds if and only if `revert . evert = id`:

``` haskell
-- Given:
fmap uI . distribute = uI
-- Goal:
revert . evert = id

fmap uI . distribute = uI
fmap uI . revert . flap . fmap evert = uI
-- revert is a natural transformation
revert . fmap uI . flap . fmap evert = uI
-- flap @I = fmap I . uI
revert . fmap uI . fmap I . uI . fmap evert = uI
revert . uI . fmap evert = uI
-- evert is a natural transformation
revert . evert . uI = uI
revert . evert = id  -- Goal, reversibly
```

If the identity law holds, `distribute . distribute = id`:

``` haskell
distribute . distribute
revert . flap . fmap evert . revert . flap . fmap evert
-- revert is a natural transformation
revert . flap . revert . fmap evert . flap . fmap evert
-- Using the naturality law for flap, twice:
revert . flap . revert . flap . evert . fmap evert
revert . fmap revert . flap . flap . evert . fmap evert
revert . fmap revert . evert . fmap evert  -- flap . flap = id
-- evert is a natural transformation
revert . fmap revert . fmap evert . evert
revert . fmap (revert . evert) . evert

-- Therefore:
distributive . distribute = revert . fmap (revert . evert) . evert

-- Corollary:
-- Given revert . evert = id, or, equivalently,
-- given the identity law:
distribute . distribute = id
```

### The composition law

Given one direction of the isomorphism is equivalent to the identity
law, we would expect the other direction to have to do with the
composition law.

If `evert . revert = id`, then the composition law holds:

``` haskell
-- Given the composition law:
fmap uC . distribute = distribute . fmap distribute . uC
-- Goal: express the law in terms of evert and revert.

fmap uC . distribute = distribute . fmap distribute . uC
fmap uC . distibute  -- LHS
fmap uC . revert . flap . fmap evert
-- flap @C = fmap C . flap . fmap flap . uC
fmap uC . revert . fmap C . flap . fmap flap . uC . fmap evert
-- revert is a natural transformation
fmap uC . fmap C . revert . flap . fmap flap . uC . fmap evert
revert . flap . fmap flap . uC . fmap evert
-- uC is a natural transformation
revert . flap . fmap flap . fmap (fmap evert) . uC  -- On hold (LHS)
distribute . fmap distribute . uC  -- RHS
revert . flap . fmap evert . fmap revert
    . fmap flap . fmap (fmap evert) . uC
revert . flap . fmap (evert . revert)
    . fmap flap . fmap (fmap evert) . uC  -- On hold (RHS)
revert . flap
    . fmap flap . fmap (fmap evert) . uC
    = revert . flap . fmap (evert . revert)
        . fmap flap . fmap (fmap evert) . uC  -- LHS = RHS
revert . flap
    . fmap flap . fmap (fmap evert)
    = revert . flap . fmap (evert . revert)
        . fmap flap . fmap (fmap evert)  -- Goal

-- Lemma: The equation just above is equivalent to the composition law.

-- Now assume:
evert . revert = id
-- Goal:
revert . flap
    . fmap flap . fmap (fmap evert)
    = revert . flap . fmap (evert . revert)
        . fmap flap . fmap (fmap evert)

revert . flap . fmap (evert . revert)
    . fmap flap . fmap (fmap evert)  -- RHS
revert . flap
    . fmap flap . fmap (fmap evert)  -- LHS = RHS
```

### Uncertain

If the identity and composition laws both hold, then `evert . revert =
id`:

```
-- Given:
fmap uI . distribute = uI
fmap uC . distribute = distribute . fmap distribute . uC
-- Goal:
evert . revert = id

fmap uC . distribute = distribute . fmap distribute . uC
-- By the lemma of the previous proof:
revert . flap
    . fmap flap . fmap (fmap evert)
    = revert . flap . fmap (evert . revert)
        . fmap flap . fmap (fmap evert)
flap . revert . flap
    . fmap flap . fmap (fmap evert)
    = flap . revert . flap . fmap (evert . revert)
        . fmap flap . fmap (fmap evert)
flap . revert . flap
    . fmap flap . fmap (fmap evert)
    = fmap revert
        . fmap flap . fmap (fmap evert)
flap . revert . flap
    . fmap flap . fmap (fmap evert)
    = fmap distribute
-- ???
```

## Draft: alternative proofs

``` haskell
-- Given:
fmap uI . distribute = uI
-- Goal:
revert . evert = id

fmap uI . distribute = uI
fmap uI . revert . flap . fmap evert = uI
-- flap @I = fmap I . uI
fmap uI . revert . fmap I . uI . fmap evert = uI
-- Rearrange uI and I, which commute with anything
fmap uI . fmap I . revert . evert . uI = uI
revert . evert . uI = uI
-- uI is surjective
revert . evert = id  -- Goal
```


`distribute . distribute = id`:

``` haskell
distribute . distribute
revert . flap . fmap evert . revert . flap . fmap evert
-- revert is a natural transformation
revert . flap . revert . fmap evert . flap . fmap evert
-- Using the naturality law for flap, twice:
revert . flap . revert . flap . evert . fmap evert
revert . fmap revert . flap . flap . evert . fmap evert
revert . fmap revert . evert . fmap evert  -- flap . flap = id
-- evert is a natural transformation
revert . fmap revert . fmap evert . evert
revert . fmap (revert . evert) . evert

-- Therefore:
distributive . distribute = revert . fmap (revert . evert) . evert

-- Corollary:
-- Given revert . evert = id, or, equivalently,
-- given the identity law:
distribute . distribute = id
```

## Extra stuff


In particular, `distribute` for functions boils down to a reasonably
familiar combinator, also known as [`flap`](
https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Functor-Fmap.html#v:flap)
or [`(??)`](
https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Lens.html#v:-63--63-).
We will meet it again in a little while:


The definition of `distribute` above illustrates a broader strategy for
using `revert`. Schematically:




Compliance to the new laws can be readily verified:

``` haskell
-- "Resident" direction
revert (evert u) = u  -- Goal
revert (evert u)  -- LHS
revert (\p -> p u)
Duo ((\p -> p u) fstDuo) ((\p -> p u) sndDuo)
Duo (fstDuo u) (sndDuo u)
u  -- LHS = RHS

-- "Visitor" direction
evert (revert s) = s  -- Goal
-- For any p :: Pos Duo
evert (revert s) p = s p
evert (revert s) p  -- LHS
evert (Duo (s fstDuo) (s sndDuo)) p
(\q -> q (Duo (s fstDuo) (s sndDuo))) p
p (Duo (s fstDuo) (s sndDuo))
-- Case analysis
-- Case 1: p = fstDuo
s fstDuo  -- Switch to RHS
fstDuo (Duo (s fstDuo) (s sndDuo)) -- Switch to LHS
s fstDuo  -- LHS = RHS
-- Case 2: p = sndDuo
s sndDuo  -- Switch to RHS
sndDuo (Duo (s fstDuo) (s sndDuo)) -- Switch to LHS
s sndDuo  -- LHS = RHS
```

