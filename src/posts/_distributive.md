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
"single-shaped", "isomorphic to functions", "representable functors",
"right adjoints", and so forth. In this post, I will explore what these
descriptions have to do with each other. I will also use the opportunity
to put down to paper a handful of proofs on this matter that I always
seem to end up deriving over and over again.

<!-- more -->

## The basic facts

In the initial part of this post, `Distributive` will be introduced
starting from connection to `Traversable`, and we will look at
at a few basic features of the class. In the process, we will prepare
the terrain for the bridge to `Representable` to be built.

Throughout the post, I will use the following homongeneous pair (or
length-two vector, if you will) type as a nice, self-contained running
example:

``` haskell
data Duo a = Duo { fstDuo :: a, sndDuo :: a }
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)
```

## Dualising Traversable

Let's begin our journey from the purported duality, then. `Traversable`
is:

``` haskell
sequenceA :: (Traversable g, Applicative f) => g (f a) -> f (g a)

-- Laws:

-- Identity:
-- sequenceA . fmap Identity = Identity

-- Composition:
-- sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA

-- Naturality (comes for free, by parametricity):
-- Given Applicative f, Applicative h, and t :: forall a. f a -> h a
-- such that t preserves pure and (<*>), that is:
-- t . pure = pure
-- t u <*> t v = t (u <*> v)
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

### Collecting results

Before we go on to further explore the general properties of
`distribute`, it makes sense to have a look at a handful of examples of
it being used, so that we have a clearer idea of what to expect from it.

In quite a few cases, `distribute` amounts to a transposition:

``` haskell
names :: [Duo String]
names =
    [ Duo "Alex" "Lifeson"
    , Duo "Geddy" "Lee"
    , Duo "Neil" "Peart"
    ]
```

``` haskell
ghci> distribute names
Duo {fstDuo = ["Alex","Geddy","Neil"], sndDuo = ["Lifeson","Lee","Peart"]}
```

In this first example, the distributive layer was given from the start
in `names`. Alternatively, we can use `\f -> distribute . fmap f` to
introduce it through some function `f`. This combination is available as
`collect`, which is a method of `Distribute`:

``` haskell
ghci> :t collect
collect
  :: (Distributive g, Functor f) => (a -> g b) -> f a -> g (f b)
```

For instance, an `f :: a -> Duo b` function can be thought of as a way
to generate two options, of type `b`, from an `a` value.  `collect f`,
then, allows us to generate and bring together all first options, and
all second options:

``` haskell
timesTables :: Duo [Integer]
timesTables = collect (\x -> Duo (7*x) (9*x)) [1..10]
```

``` haskell
ghci> timesTables
Duo {fstDuo = [7,14,21,28,35,42,49,56,63,70], sndDuo = [9,18,27,36,45,54,63,72,81,90]}
```

(A more satisfying version of this example can be written using infinite
lists as the distributive functor. See the [*streams*](
https://hackage.haskell.org/package/streams-3.3/docs/Data-Stream-Infinite.html)
package for an implementation ready to play with.)

`fmap` and `distribute` can also be combined in the opposite way, `\f ->
fmap f  . distribute`: instead of supplying a function to create a
distributive layer before distributing, we can supply a function to
eliminate the other functorial layer after distributing. That gives rise
to `cotraverse`: [^cotraverse]

[^cotraverse]: As the name suggests, `cotraverse` is dual to `traverse`.
  Omitting the constraints, the type of traverse is:

    ``` haskell
    (a -> f b) -> (g a -> f (g b))
    ```

    That dualises to (as before, `-<` indicates an arrow in
    **Hask**<sup>op</sup>):

    ``` haskell
    (a -< f b) -> (g a -< f (g b))
    ```

     Or, rendered in **Hask**:

     ``` haskell
     (f b -> a) -> (f (g b) -> g a)
     ```

    Accordingly, the traversable laws in terms of `traverse` dualise to
    become distributive laws in terms of `cotraverse`:

    ``` haskell
    cotraverse runIdentity = runIdentity
    cotraverse (g . fmap f . getCompose)
        = cotraverse g . fmap (cotraverse f) . getCompose
    cotraverse f . t = cotraverse (t . f)  -- t is a natural transf.
    ```

``` haskell
ghci> :t cotraverse
cotraverse
  :: (Distributive g, Functor f) => (f a -> b) -> f (g a) -> g b
```

One way to picture `cotraverse` is as a kind of zipping for distributive
structures, with the `f a -> b` function combining `a` values drawn from
matching positions:

``` haskell
powers :: [Duo Integer]
powers = (\x -> Duo (x^2) (x^3)) <$> [1..5]
```

``` haskell
ghci> cotraverse sum powers
Duo {fstDuo = 55, sndDuo = 225}
```

### Functions can be distributed

Functions give us a specially important example of a distributive
functor. Consider:

``` haskell
flap :: Functor f => f (r -> a) -> r -> f a
flap m r = ($ r) <$> m
```

`flap` is [a reasonably familiar](
https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Functor-Fmap.html#v:flap)
combinator, [also known as `(??)`](
https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Lens.html#v:-63--63-),
which turns `f (r -> a)` into `a -> f r` by supplying a common argument
to all the `r -> a` functions:

``` haskell
ghci> flap [reverse, take 4] "sandals"
["sladnas","sand"]
```

The name of the combinator is a play on it being a generalisation of
`flip`, as can be seen by specialising `f` to `(->) s`:

``` haskell
ghci> :t flap @((->) _)
flap @((->) _)
  :: Functor ((->) w) => (w -> (a -> b)) -> a -> w -> b
```

From our current point of view, when looking at `flap`'s type it
stands out how it pulls the function arrow (or, to be precise, the `(->)
r` functorial layer) out of `f`. If that sounds a lot like what
`distribute` usually does, it is because `flap` indeed is `distribute`
for functions. As the `Data.Distributive` source puts its:

``` haskell
instance Distributive ((->)e) where
  distribute a e = fmap ($e) a
  collect f q e = fmap (flip f e) q
```

If the `f` functor is a monad, `flap`/`distribute` converts [static
arrows](/posts/applicative-archery.html) (the ones we use with `(<*>)`)
into the equivalent Kleisli arrows (the ones we use with `(>>=)`). For
instance, `distribute` gives us a very direct implementation of
[`(<**>)`](
https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Applicative.html#v:-60--42--42--62-)
in terms of `Monad`: [^backAp]

``` haskell
backAp :: Monad m => m a -> m (a -> b) -> m b
backAp v u = v >>= distribute u
```

[^backAp]: It never hurts to emphasise that `(<**>)` is not `flip
  (<*>)`, and the `backAp` defined here is not `ap` from
  `Control.Monad`, as they differ in the order effects are sequenced.
  Here is an implementation of `ap` in a similar style:

    ``` haskell
    ap :: Monad m => m (a -> b) -> m a -> m b
    ap u v = u >>= flip fmap v
    ```

### Counting shapes

While comparing `distribute` and `traverse` a few subsections ago, I
mentioned that, at least as far as my description went, `distribute`
only makes sense for functors that have just a shape. This idea will
play an important role in what follows, and we will engage it more
closely. For now, it is good enough to understand "shape" as everything
in a `f a` functorial value other than the `a` values it holds. For
instance, here are two examples and two non-examples of single-shaped
functors:

- `Duo` only has a single shape:  every `Duo a` value holds exactly two
  `a` values, and nothing else.

- `((->) Integer)`, an example of a function functor, only has a single
  shape: every `Integer -> a` function has exactly one `a` value for
  each possible `Integer`, and, implementation details aside, this
  association fully specifies the function.

- `[]` does not have a single shape: a list might contain any number of
  `a` values; accordingly, every possible list length makes for a
  different shape.

- `(,) Integer` does not have a single shape: while a pair of type
  `(Integer, a)` contains exactly one `a` value, it also holds an
  arbitary `Integer` value, with each choice of `Integer` amounting to a
  different shape. (In particular, note that said `Integer` is not
  affected by `fmap`.)

## Natural magic

In the introduction to this post, I mentioned a laundry list of
properties of distributive functors. At first glance, not many of them
sound like they have anything to do with `distribute` and its unassuming
type:

``` haskell
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
```

Not unusually for cases like this, the connection has to do with
parametricty.  [^parametricity] A relevant example is provided by
contrasting the naturality law for `Distributive` we mentioned earlier
with its `Applicative` counterpart:

[^parametricity]: For an introduction to the core idea behind
  parametricity arguments and what they have to do with naturality, see
  my earlier post [*What does fmap
  preserve?*](/posts/what-does-fmap-preserve.html). Here we will be
  specially interested in using parametricity on type constructors, as
  discussed, for instance, by Janis Voigtländer's [*Free Theorems
  Involving Type Constructor Classes*](
  http://www.janis-voigtlaender.eu/Voi09b.html).

``` haskell
-- Naturality law for Applicative:
-- Given Applicative f, Applicative h, and t :: forall a. f a -> h a
-- such that t preserves pure and (<*>), that is:
-- t . pure = pure
-- t u <*> t v = t (u <*> v)
t . sequenceA = sequenceA . t

-- Naturality law for Distributive:
-- Given Functor f, Functor h, and t :: forall a. f a -> h a
fmap t . distribute = distribute . t
```

In the case of `sequenceA`, the property only holds if the natural
transformation `t` preserves `pure` and `(<*>)`, as otherwise applying
it to the applicative layer might interfere with `sequenceA`. With
`distribute`, though, there are no constraints other than `Functor` on
the `f` functor, and so the law can be used with any natural
transformation. That turns the naturality law into a much stronger
claim.

We can learn a lot about `Distributive` by taking advantage of
parametricity by means such as the naturality law. This section will
highlight some key results attainable in this way, results upon which
the rest of the post will build upon. For the sake of readability,
detailed justification of the claims here will be provided at the end of
the post, in an appendix.

### The law of extractors

Let's begin with the decisive result. I would say the property below,
which follows from the naturality and identity laws, expresses better
than anything else the essence of `Distributive`:

``` haskell
-- Law of extractors:
(\p -> p u) <$> distribute id = u
```

At the heart of this rather mind-bending statement we find `distribute
id`.  That is a combinator worthy of a name of its own, so I will from
now on call it `chart`:

``` haskell
chart :: Distributive g => g (g a -> a)
chart = distribute id
-- (\p -> p u) <$> chart = u
```

`chart` is a distributive structure that holds a number of extractor
functions of type `g a -> a`. The property says that if we pick any
other `u :: g a` structure and use `fmap` to feed it to all the
extractors in `chart`, we end up reconstructing `u`. In a more colourful
style, we might say the `chart` tells us how to navigate to any position
in the structure. [^extractors-chart]

[^extractors-chart]: An arguably better name for `chart` is simply
  `extractors`. I have opted for the metaphor-laden alternative here
  just so that I don't have to rely on formatting alone to distinguish
  between the `Distributive g => g (g a -> a)` combinator we are
  defining here and the plural of "extractor", which would have made
  some passages of this text very awkward.

I will call this property the *law of extractors*. It can be seen as an
alternative form of the identity law, as the two can be shown to be
equivalent with the help of the naturality law. A major selling point
for this alternative form is that it helps to make explicit two
important consequences:

- For every `u :: g a`, there is a function (namely, `\p -> p u`, or `($
  u)` for short) which can be used through `fmap` to change `chart` into
  `u`. Considering that `fmap` cannot change shapes, that is exactly
  what we would expect if all `g a` values had the same shape (more
  specifically, the shape of `chart`).

- `chart` holds all possible `g a -> a` extractors polymorphic in `a`,
  with every extractor occupying the position it extracts. That is what
  makes it possible for it to reconstruct any `u :: g a` by feeding it
  to the extractors. By the way, it is worth noting that `forall a.  g a
  -> a` extractors are essentially natural transformations from `g` to
  `Identity`, which is ultimately why the naturality properties of
  `distribute` matter so much here.

Below is a tiny example with `Duo` which illustrates these two
consequences:

``` haskell
chart @Duo
distribute @Duo id
Duo (fstDuo <$> id) (sndDuo <$> id)
Dup fstDuo sndDuo

($ Duo 1 2) <$> chart @Duo
($ Duo 1 2) <$> Duo fstDuo sndDuo
Duo (($ Duo 1 2) fstDuo) (($ Duo 1 2) sndDuo)
Duo 1 2
```

To put it in another way: `distribute` encodes information about the
shape of the distributive functor, and `chart` makes that information
explicit.

To further underline how `chart`, `distribute` and their respective laws
are on the same footing, here is a definition of `distribute` in terms
of `chart`:

``` haskell
distribute m = (\p -> p <$> m) <$> chart
```

The definition looks a lot like the law of extractors, except that,
instead of directly applying each extractor to `m`, we have to do it
to do it under the extra functorial layer by using `fmap`/`(<$>)`.

### Closing the Select loophole

You may have noticed that my comments about those two important
consequences of the law of extractors were a bit of a hedge:

- I described `fmap ($ u)` changing `chart` into `u` for any `u` as
  "exactly what we would expect if all `g a` values had the same shape",
  rather than saying outright that they all do have the same shape.

- I noted that `chart` "holds all" polymorphic extractors, rather than
  claiming it *is* the collection of all such extractors, arranged in a
  certain manner.

Strenghtening those claims in the way suggested above is not just a
matter of having a simpler, more aesthetically pleasing description of
distributive functors. In particular, making sure that `chart` only
holds polymorphic extractors is an important step on the way to
`Representable`, as we will discuss in a little while. Fortunately, if
we assume both the identity and composition laws hold, and employ a
bootload of naturality properties, we can conclude that all `chart`
extractors amount to natural transformations to `Identity`, which is
what we need to make the stronger claims. (The detailed argument on that
can be found in the appendix.)

Since this is a quite subtle point (after all, it is not obvious how
something with the type of `distribute id` could somehow produce
extractors that aren't fully polymorphic), I will allow myself a
digression to justify my cautiousness and illustrate what is at stake
here. Let's have a look at [`Select`](
https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Select.html),
the selection monad:

``` haskell
-- A paraphrased, non-transformer version of Select.
-- A `Select r a` value can be thought of as a way to choose an `a`
-- value based on some chosen criterion, expressed as an `a -> r`
-- function.
newtype Select r a = Select { runSelect :: (a -> r) -> a }
```

Does `Select r` have a single shape?

If we try to settle the matter by counting shapes, as in those four
examples from a while ago, things get confusing very quickly. The
strangeness comes from how `a`, the argument to the type constructor,
shows up to the left of a function arrow, which makes it hard to picture
what even is the shape, as something that can be considered apart from
the contents. Functors with no such occurrences, like our earlier
examples, are known as *strictly positive functors*. `Select r`, then,
is an example of a functor which is *not* strictly positive.
[^strictly-positive]

[^polarity]: Though it doesn't explicitly mention strict positivity,
  Michael Snoyman's [*Covariance and Contravariance*](
  https://www.fpcomplete.com/blog/2016/11/covariance-contravariance/)
  might be useful as an accessible explanation of why the side of the
  arrow on which type variables appear matters.  In particular, the
  `CallbackRunner` example in the "Positive and negative position"
  section towards the end is a `Functor` that isn't strictly positive.

We might try to cut through the befuddlement by pointing to the
following combinator: [^chartSelect]

[^chartSelect]: I originally realised it exists by looking into [a Stack
  Overflow answer by Sergei Winitzki](
  https://stackoverflow.com/a/39736535/2751851). I thank him for this
  post seeing the light of day: without considering `Select` as a
  counterexample, I don't think I would have managed to put the pieces
  together.

``` haskell
chartSelect :: Select r (Select r a -> a)
chartSelect = Select $ \k -> \u -> u `runSelect` \a -> k (const a)
```

`chartSelect` has the type an hypothetical `chart` for `Select r` would
have, and it turns out it actually follows the law of extractors.  Given
what we have said so far about `chart`, this sounds like a pretty good
argument in support of `Select r` indeed having a single shape, right?

Things aren't so simple, though. If `Select r` has a single shape, the
shape [is bigger on the inside](
https://tvtropes.org/pmwiki/pmwiki.php/Main/BiggerOnTheInside). If we
were to think of the shape of `Select r ~ (a -> r) -> a` as if it were
a garden-variety function functor, we would say it holds an `a`
result value for each possible `a -> r` function. The problem is that
the number of `a -> r` functions depends on our choice of `a`, and so
the shape, so to speak, adjusts itself when we specialise `Select r a`
one way or another. In particular, a specialised `chartSelect`,
like this one...

``` haskell
ghci> :t chartSelect @Bool @Integer
chartSelect @Bool @Integer
  :: Select Bool (Select Bool Integer -> Integer)
```

... has, as possible results, *all* `Select Bool Integer -> Integer`
extractors, and not just the ones we get by specialising the polymorphic
extractors of type `Select Bool a -> a`.
[^polymorphic-Select-extractors] Note this sort of thing doesn't happen
with strictly positive functors. If it did, specialising `chart @Duo` to
`Integer` would make things like `sum @Integer` appear alongside `fstDuo
@Integer` and `sndDuo @Integer`, which would be quite outrageous.

[^polymorphic-Select-extractors]: If we insist on keeping `a`
  polymorphic, the only possible extractors are of the form `\u -> u
  `runSelect` \_ -> b`, with the role of `Integer ->  Bool` criteria
  being played by constant functions which ignore the `Integer` that is
  ostensibly being tested.

    Looking at the matter from a different angle, the trick with `k` and
    `const a` in the definition of `chartSelect` is ultimately a way to
    make sure that, for any choice of `f :: a -> r`, ``(`runSelect` f)
    :: Select r a -> a`` shows up as a possible result. ``(`runSelect`
    f)``, in turn, generally isn't a natural transformation.

Given the way we have framed the discussion in terms of shapes and
polymorphic extractors, `Select` puts us in the uncomfortable position
of stretching those notions to the point where they might no longer be
reasonably applied. One way to keep things simple and intelligible might
be to, from the outset, only consider strictly positive functors.
[^containers] Given what we have learned so far about distributive
functors, though, we don't actually need additional restrictions. If
`extractSelect` makes non-polymorphic extractors available, we'd expect,
by the argument mentioned just before this digression, that the
`distribute` candidate we get from it...

[^containers]: In particular, that is how the theory of containers
  avoids this problem. See, for instance, the first few paragraphs of
  Abbott et. al., [*Containers: Constructing strictly positive types*](
  https://www.sciencedirect.com/science/article/pii/S0304397505003373).
  Do note that is a very theory-heavy article, and that there is no need
  at all to jump into it now in order to follow the discussion in this
  post.

``` haskell
nonDistribute :: Functor f => f (Select r a) -> Select r (f a)
nonDistribute m = Select $
    \k -> (\u -> u `runSelect` \a -> a <$ m) <$> m
```

... to be unlawful, and that is the case indeed. `nonDistribute` borrows
the shape of `m` so that it can turn the supplied criterion `k :: f a ->
r` into an `a -> r` function. That ultimately leads to a violation of
the composition law.

## It's all about the extractors

Let's now have a second look at the law of extractors:

``` haskell
(\p -> p u) <$> chart = u
```

Written in this way, the law draws our eyes to `chart`.  We can
rearrange it so that the focus is shifted to what is being done to `u`:

``` haskell
(\e -> e <$> chart) . (\u p -> p u) = id
```

This change of style brings to the foreground that `\u p -> p u`, also
known as `flip ($)`, when taken as a function *from* `g a`...

``` haskell
(\u p -> p u) :: g a -> (g a -> a) -> a
```

... has, if `g` is `Distributive`, a left inverse, namely, this function
*to* `g a`:

``` haskell
(\e -> e <$> chart) :: Distributive g => ((g a -> a) -> a) -> g a
```

To put it in another way, we can convert some `g a` into a `(g a -> a)
-> a` function and, given `Distributive g`, undo that recover the `g a`
from which the function was made. That is particularly interesting
because it suggests that, if we are lucky, there will be a way to show
the inverses above are actually full inverses, which would give us a
powerful characterisation of distributive functors through an
isomorphism between `g a` and `(g a -> a) -> a`.

There is, however, a complication to deal with. Given the `(g a -> a) ->
a` here is clearly meant to take an extractor and return a value, the
type `flip ($)` gives us is too permissive. The functions it produce
happily accept counterfeit extractors that rely on concrete element
types:

``` haskell
ghci> test = flip ($) (Duo 3 4)
ghci> test sum
7
```

Using a larger type than we need is bound to make things difficult if we
are looking for an isomorphism (isomorphic types, after all, must have
the same amount of possible values).

### A revamp

The good news, though, is that we have already established that, if `g`
is a lawful distributive functor, only the proper (polymorphic, natural)
extractors matter, as only those are made available by `chart`. That
being so, it makes sense to ensure the extractors are polymorphic by
replacing `(g a -> a) -> a` with the higher-rank type `(forall x. g x ->
x) -> a`, the inverses will only deal with proper, polymorphic
extractors. For that, however, we need to soup up our toolkit:

``` haskell
elide :: g a -> (forall x. g x -> x) -> a
elide u = \p -> p u

class Distributive g => Revealable g where
    reveal :: ((forall x. g x -> x) -> a) -> g a
```

I'd better justify the choice of names:

- `elide` takes a structure and hides the information about its shape.
  If we are given `elide u` for some `u :: g a`, the only thing about
  `u` it can tell us is what are the `a` values at specific positions,
  assuming we have the corresponding extractors to hand it. `elide`
  doesn't impose any constraints on `g`. Also, it is, in general, a
  lossy operation.  For instance, `elide` on `(Integer, a)` irreversibly
  removes access to the first component.

- If `g` is distributive, though, knowing the values at all positions is
  enough to reconstruct the structure, as there is only one possible
  shape. That being so, it becomes possible to implement `reveal`, a
  left inverse for `elide`. A class separate from `Distributive` is
  needed because there is no way to squeeze the needed higher-rank type
  out of `distribute`. [^impredicative-types] Here are a couple
  instances of it:

[^impredicative-types]: The definitions here are meant to be used with
  the `ImpredicativeTypes` GHC extension. If you want to try them on GHC
  9.0 or older, it is probably better to adapt them to use a `newtype`
  wrapper around `(forall x. g x -> x) -> a` like the one below, so that
  `ImpredicativeTypes` is not needed:

    ``` haskell
    newtype Elide g a = Elide { runElide :: (forall x. g x -> x) -> a }
        deriving Functor
    ```

``` haskell
instance Revealable Duo where
    reveal e = Duo (e fstDuo) (e sndDuo)

instance Revealable ((->) r) where
    reveal e = \r -> e ($ r)
```

We should be able to recover `distribute` from `reveal`:

``` haskell
distributeRev :: (Revealable g, Functor f) => f (g a) -> g (f a)
distributeRev m = reveal (\p -> p <$> m)
```

With `chart`, we are able to do even better:

``` haskell
chartRev :: Revealable g => g (forall x. g x -> x)
chartRev = reveal id
-- reveal e = e <$> chartRev
```

`chartRev` and `chart` hold the same extractors. `chartRev`, however,
has the more restrictive type `distribute` can't give us. In particular,
the upgraded types make it impossible to implement a `chartRev` for
`Select r`, which confirms we have indeed closed that loophole.

### The vaunted isomorphism

As far as laws for `Revealable` go, we already know we want `reveal` to
be the left inverse of `elide`:

``` haskell
reveal . elide = id
```

Is it reasonable to also require `elide . reveal = id`, thus giving us
an isomorphism?

Assuming `reveal` is the left inverse of `elide`, we know that `elide`
is injective (that is, it takes different arguments to different
results), and that `reveal` is surjective (that is, it can produce every
possible value in its result type). That being so, showing `reveal` is
injective suffices to prve it is also the right inverse of `elide`.
Happily, given all we have found so far, we can show that `reveal` will
be injective for any lawful distributive. Conversely, it can also be
shown that `reveal` and `elide` being full inverses suffices to give
rise to a lawful `distribute`. All in all, the isomorphism the
witness...

``` haskell
reveal . elide = id
elide . reveal = id
```

... is enough to characterise `Distributive`.

It is worth emphasising that `g a` being isomorphic to `(forall x. g x
-> x) -> a` directly tells us `g` is single-shaped: if all information
specific to some `g a` value can be obtained through the `forall x. g x
-> x` extractors, then the `g` shape must be fixed.

inverse of `elide`, it suffices to show that `reve` is surjective.
Let's have another look at its definition:

### Instances we know and love

### Through the looking glass


## Appendix: proofs and justifications

In what follows, we will take the signature of `distribute` to be...

``` haskell
distribute :: (Distributive g, Functor f) => f (g a) -> g (f a)
```

... so that, unless stated otherwise, `g` refers to the relevant
distributive functor, and `f` to the other functor involved in
`distribute`.

We will often rely on the identity and composition laws for
`distribute`, restated below for the sake of reference:

``` haskell
-- Identity
fmap runIdentity . distribute = runIdentity

-- Composition
fmap getCompose . distribute = distribute . fmap distribute . getCompose
```

Some fundamental `Distribute` instances:

``` haskell
instance Distribute Identity where
    distribute = Identity . fmap runIdentity

instance (Distributive g, Distributive h) => Distributive (Compose g h) where
    distribute = Compose . fmap distribute . distribute . fmap getCompose

flap :: Functor f => f (r -> a) -> r -> f a
flap m r = (\f -> f r) <$> m

instance Distributive ((->) r) where
    distribute = flap
```

### Essential naturality properties

First of all, there is the *naturality law*:

``` haskell
-- Functor h; t :: forall x. f x -> h x
fmap t . distribute = distribute . t
```

It works for any naural transformation `t`. `distribute` only demands
`Functor` of `f`, and so it has no means of doing anything to the `f`
functorial layer. That being so, any natural transformation affecting
`t` must be preserved by `distribute`. This is a free law, in the sense
that, thanks to parametricity, it holds for anything with the type of
`distribute`.

By similar reasoning, we can obtain what I will call the *second
naturality law*, which states that a lawful `distribute` preserves
natural transformations on the distributive functor:

``` haskell
-- Distributive h; t :: forall x. g x -> h x
t . distribute = distribute . fmap t
```

The obliviousness of `distribute` to the `f` functorial layer also means
that whether the property above holds cannot depened on what `f` is.
That being so, it suffices to consider what happens at, say, `f ~
Identity`. By the identity law, we know that:

``` haskell
distribute @_ @Identity = fmap Identity . runIdentity
```

Therefore:

``` haskell
-- Goal
t . distribute @_ @Identity = distribute @_ @Identity . fmap t
-- LHS
t . distribute @_ @Identity
-- g is lawful
t . fmap Identity . runIdentity
-- t is natural
fmap Identity . t . runIdentity
fmap Identity . runIdentity . fmap t
-- RHS
distribute @_ @Identity . fmap t
-- h is lawful
fmap Identity . runIdentity . fmap t  -- LHS = RHS
```

This property can be contrasted with the corresponding one for
`Traversable`:

``` haskell
-- Traversable h; t :: forall x. g x -> h x
-- toList . t = toList
fmap t . sequenceA = sequenceA . t
```

There, the `Applicative` constraint on the other functorial layer leads
to the precondition that `t` has to preserve `toList`, and requires a
significantly trickier proof.

Along the same lines, parametricity can be used to establish the
*uniqueness of `distribute`*. `distribute` cannot affect `f` nor `a`, so
any difference between implementations of `distribute` must only show up
in the distributive layer `g`, and do so regardless of what `f` and `a`
are. The identity law, however, ensures a lawful `distribute` cannot
affect the `g` layer either. That being so, any two functions with the
type of `distribute` that follow the identity law must agree on their
results.

### chart and the law of extractors

Consider some function `f :: r -> g a` for a `Distributive g`. By the
naturality law, we have:

``` haskell
-- Functor h; t :: forall x. (r -> x) -> h x
t <$> distribute f = distribute (t f)
```

In particular, given the following definition...

``` haskell
apply :: r -> (r -> a) -> Identity a
apply r f = Identity (f r)
```

... and some `u :: r`, `apply u` is a natural transformation from `((->)
r)` to `Identity`, and so we can substitute it into the naturality law:

``` haskell
apply u <$> distribute f = distribute (apply u f)
```

The `distribute` on the right-hand side has `Identity` as the other
functor. That being so, the identity law makes further simplifications
possible:

``` haskell
apply u <$> distribute f = distribute (apply u r)
-- Identity law
apply u <$> distribute f = fmap Identity (runIdentity (apply u f))
-- Applying fmap runIdentity on both sides
runIdentity . apply u <$> distribute f = runIdentity (apply u f)
-- Using the definition of apply
(\p -> p u) <$> distribute f = f u
```

In particular, if `f` is `id :: g a -> g a`:

``` haskell
-- Law of extractors
(\p -> p u) <$> distribute id = u
```

We define:

``` haskell
chart :: Distributive g => g (g a -> a)
chart = distribute id
```

And so:

``` haskell
(\p -> p u) <$> chart = u
```

We have obtained the law of extractors from the identity law. It is also
possible to go the other way, showing the naturality properties make the
laws equivalent:

``` haskell
(\p -> p u) <$> chart = u
(\p -> p u) <$> distribute id = u
Identity . (\p -> p u) <$> distribute id = Identity <$> u
-- apply defined as above
apply u <$> distribute id = Identity <$> u
-- Naturality law
distribute (apply u id) = Identity <$> u
distribute (Identity (id u)) = Identity <$> u
distribute (Identity u) = Identity <$> u
-- Applying fmap runIdentity to both sides
runIdentity <$> distribute (Identity u) = u
-- let m = Identity u
runIdentity <$> distribute m = runIdentity m  -- The identity law
```

`distribute` can be defined in terms of `chart`:

``` haskell
distribute m = (\p -> p <$> m) <$> chart
```

Given the uniqueness of `distribute`, all we need to confirm this is
indeed `distribute` is verifying the identity law holds:

``` haskell
distribute @Identity m
-- Assuming the proposed definition
(\p -> p <$> m) <$> chart
-- let u = runIdentity m
(\p -> p <$> Identity u) <$> chart
(\p -> Identity (p u)) <$> chart
Identity . (\p -> p u) <$> chart
-- Law of extractors
Identity <$> u
Identity <$> runIdentity m  -- As specified by the identity law
```

### Polymorphic extractors are natural

The derivation of the law of extractors illustrates how a polymorphic
extractors `p :: forall x. h x -> x` is, in essence, a natural
transformation from `h` to `Identity`. Using the naturality laws with
such a function would, in principle, require expressing it as `Identity
. p`. If we assume the identity law holds, though, we can get away
without actually mentioning `Identity`. Three examples:

``` haskell
-- p :: forall x. f x -> x; f :: a -> b

-- Identity . p is a natural transformation
Identity . p . fmap f = fmap f . Identity . p
Identity . p . fmap f =  Identity . f . p
-- Post-composing runIdentity on both sides
p . fmap f = f . p
```

``` haskell
-- p :: forall x. f x -> x

-- (First) naturality law:
fmap (Identity . p) . distribute = distribute . Identity . p
-- Identity law
fmap (Identity . p) . distribute
  = fmap Identity . runIdentity . Identity . p
fmap (Identity . p) . distribute = fmap Identity . p
-- Post-composing fmap runIdentity on both sides
fmap p . distribute = p
```

``` haskell
-- p :: forall x. g x -> x

-- Second naturality law:
Identity . p . distribute = distribute . fmap (Identity . p)
-- distribute @Identity
Identity . p . distribute
  = Identity . fmap runIdentity . fmap (Identity . p)
Identity . p . distribute = Identity . fmap p
-- Post-composing runIdentity on both sides
p . distribute = fmap p
```

In what follows, we will use such simplifications liberally to avoid
extra boilerplate.

### chart holds all polymorphic extractors

Another important property of `chart` is that it holds all polymorphic
extractors. A more precise way of stating that is:

``` haskell
-- p :: forall x. g x -> x
p chart = p
```

That is, any polymorphic extractor can extract itself from `chart`. This
property can be proved by applying the law of extractors to `chart`:

``` haskell
-- Law of extractors:
(\p -> p u) <$> chart = u
-- let u = chart
(\p -> p chart) <$> chart = chart
-- Apply p :: forall x. g x -> x to both sides
p ((\p -> p chart) <$> chart) = p chart
-- p is natural
(\p -> p chart) (p chart) = p chart
(p chart) chart = p chart
-- v :: g a; apply (\p -> p v) to both sides
(\p -> p v) ((p chart) chart) = (\p -> p v) (p chart)
-- (\p -> p v) is natural
(p chart) ((\p -> p v) <$> chart) = (\p -> p v) (p chart)
(p chart) ((\p -> p v) <$> chart) = p ((\p -> p v) <$> chart)
-- Law of extractors
(p chart) v = p v
-- v is arbitrary
p chart = p
```

## Sections from the original attempt

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



## Proofs from the original attempt

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

