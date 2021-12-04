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
shape, though, we can recreate it on the outside, no questions asked. As
for the `f` functorial layer, we can simply duplicate it as many times
as necessary to fill in the positions of the `g` shape:

``` haskell
instance Distributive Duo where
    distribute u = Duo (fstDuo <$> u) (sndDuo <$> u)
```

Ordinary Haskell values can be duplicated at will, so nothing more than
`Functor` is necessary for `f`. [^coapplicative]

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

We are betting on having a single shape being a defining
characteristic of distributive functors, so it should be worth it to
have a closer look at what that property means. Here is one possible
approach: if we know that a functor `g` has only one possible shape, all
there remains to learn about some `u :: g a` value is which `a` values
can be found in each position of the shape. To discover that, we can use
`forall x. g x -> x` extractor functions, however many there might be.
That, however, means there is an isomorphism between `g a` and the
following type:

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

And now, the unavoidable follow-up question: is this `distribute`
lawful? Happily, the answer is yes: given a `revert` which has `evert`
as its inverse on both sides, the definition above gives us a
`distribute` which follows the usual distributive laws. A full proof of
that result can be found in the appendix to this post. For now, I will
just comment on a striking aspect of said proof.

### Through the looking glass

For the sake of more comfortable equational reasoning, the proof in the
appendix uses a pointfree spelling of the `distribute` definition:

``` haskell
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
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
the implementation of `distribute` offered here amounts to changing `g`
into `(->) (Pos g)` through `fmap evert`, distributing it with `flap`,
and restoring the `g` layer on the outside with `revert`.

Now, a slight detour: remember the informal description of
`Traversable`, in terms of separating the contents from the shape and so
forth, that I mentioned at the beginning of the post? There are a few
ways to explicitly use it to define `Traversable`. Here is one that,
though a bit rough around the edges, conveys the idea well enough
without calling for advanced techniques. The key idea is expressing the
separation of shape and contents as an isomorphism:

``` haskell
data Decomp t a = Decomp (t ()) [a]

detach :: (Functor t, Foldable t) => t a -> Decomp t a
detach u = Decomp (() <$ u) (toList u)

-- Given a detach, this suffices to specify an instance of Traversable.
fill :: Traversable t => Decomp t a -> t a

-- Laws:

fill (detach u) = u
-- Assuming length sh = length as
detach (fill (Decomp sh as)) = Decomp sh as
```

(The equal length assumption, though rather unsightly, is not a
fundamental problem. In particular, it can be avoided with more
sophisticated encodings.)

Given `detach` and `fill`, we can implement a lawful `sequenceA`:

``` haskell
-- The familiar sequenceA for lists.
sequenceList :: Applicative f => [f a] -> f [a]
sequenceList = foldr (liftA2 (:)) (pure ())

-- sequenceList, adapted to Decomp.
sequenceDecomp :: Applicative f => Decomp t (f a) -> f (Decomp t a)
sequenceDecomp (Decomp sh ms) = Decomp sh <$> sequenceList ms

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA = fmap fill . sequenceDecomp . detach
```

Here's the kicker, though: this definiton of `sequenceA` perfectly
mirrors the pointfree definition of `distributive` we saw above! Here,
we used the `detach`/`fill` isomorphism to sequence it as a list; there,
we used the `evert`/`revert` isomorphism to distribute it as a function.
Accordingly, the proof in the appendix connecting the `evert`/`revert`
isomorphism to the distributive laws is essentially a flipped version of
the corresponding proof for `detach`/`fill` and the distributive laws.
[^shape-and-contents]

[^shape-and-contents]: For a detailed account of the shape-and-contents
  presentation of `Traversable`, see [this Stack Overflow answer I wrote
  a while ago](https://stackoverflow.com/a/61199603/2751851).

### The other way around

We have defined `distribute` using `revert`, but what about getting
`revert` out of `distribute`? There's a cute trick that can take us most
of the way:

``` haskell
-- Here, id :: g a -> g a
distribute id :: Distribute g => g (g a -> a)
```

`distribute id` is a distributive structure in which each position holds
its corresponding `g a -> a` extractor. We can use it to get something
that looks like `revert`:

``` haskell
revertRank1 :: Distributive g => ((g b -> b) -> a) -> g a
revertRank1 e = e <$> distribute id
```

`revertRank1`, however, is a pale shadow of the real thing. Setting up
the isomorphism with `flip ($)`, which is the rank-1 version of `evert`,
requires specialising the rank-1 `g b -> b` extractor type to `g a ->
a`, and doing so leaves us with something too weak for even defining
`distribute` as `\m -> revert (\p -> p <$> m)`.  Another salient
limitation is that the `g b -> b` extractors can't be instantiated at
multiple types at once, which makes certain implementations in terms of
`revertRank1` awkward, `apD` being a good example.

Unfortunately, `distribute` gives us no way to bring in the extra
polymorphism in the extractor type, and so we end up with `g a -> a`
instead of `Pos g ~ forall x. g x -> x`. What we'd really want to have
would be `revert id :: Distribute g => g (forall x. g x -> x)`, but of
course that takes `revert` for granted.

Is our `Distributive` class more powerful than the one from
`Data.Distributive`, then? It certainly looks like so, given that
`distribute` doesn't give us a proper `revert`. However, it can be
argued that the difference is a mere technicality, as the extractors in
`distribute id` are the same ones given by `revert id` (see the appendix
for proofs). Ultimately, we end up running against a type system
limitation, which makes it impossible to give `distribute id` the
stronger type it should rightfully have. From that point of view,
identifying our `Distributive` with the conventional one still makes
sense. [^unsafeCoerce]

[^unsafeCoerce]: If you are in the mood for dramatic measures, or just
  want to play with `revert` using the class from `Data.Distributive`,
  in this case it should be safe to force the issue by resorting to
  `unsafeCoerce` to introduce the extra polymorphism:

    ``` haskell
    revertCheat :: Distributive g => (Pos g -> a) -> g a
    revertCheat e =
        e <$> unsafeCoerce (distribute (id :: g Any -> g Any))
    ```

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

-- Other naturality properties (also ensured by parametricity):

fmap f . evert = evert . fmap f  -- (!)
fmap f . revert = revert . fmap f  -- (!)

-- Given p :: Pos g
p (f <$> u) = f (p u)

fmap (fmap f) . distribute = distribute . fmap (fmap f)

f . uI = uI . fmap f
fmap (fmap f) . uC = uC . fmap f

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

-- Corollary:
-- If evert . revert = id, then:
revert . flap = distribute . fmap revert
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

-- Corollary: flap has the naturality properties of distribute.
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
composition law. It doesn't stop there, though...

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
revert . flap . fmap flap . fmap (fmap evert) . uC  -- On hold
distribute . fmap distribute . uC  -- RHS
revert . flap . fmap evert . fmap revert
    . fmap flap . fmap (fmap evert) . uC
revert . flap . fmap (evert . revert)
    . fmap flap . fmap (fmap evert) . uC  -- On hold
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

``` haskell
-- Given
revert . evert = id
evert . revert = id
-- Goal
distribute . distribute = id

distribute . distribute
revert . flap . fmap evert . revert . flap . fmap evert
-- revert is a natural transformation
revert . flap . revert . fmap evert . flap . fmap evert
-- Using the distribute definition corollaries
distribute . fmap revert . revert . distribute . evert . fmap evert

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
