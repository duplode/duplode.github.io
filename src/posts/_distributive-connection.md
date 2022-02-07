---
title: "Every Distributive is Representable"
license: CC-BY-SA
published: 2022-02-07T22:00:00-03:00
gh-issue: 17
twitter-card-descr: "Shedding light on the hidden link"
toc: true
---

"Every `Distributive` `Functor` is actually `Representable`", as the
documentation for [`Representable`](
https://hackage.haskell.org/package/adjunctions-4.4/docs/Data-Functor-Rep.html)
tells us straight away, and yet it is far from obvious why that should
be the case. At first glance, [`Distributive`](
https://hackage.haskell.org/package/distributive-0.6.2.1/docs/Data-Distributive.html),
the dual to `Traversable`, appears to have little if anything to do with
`Representable`, the class for functors isomorphic to functions. The
goal of this post is making this connection explicit. In the process, we
will tease out a fair amount of information about the two classes, and
also contemplate what makes it tricky to fully bridge the gap to
`Representable`.

<!-- more -->

## The basic facts

Over the course of this post, the focus will alternate between
`Distributive` and `Representable`. In this first section, we will
review the basic definitions and laws upon which we will build.
Following that, we will work on both ends, aiming at making the classes
meet in the middle.

### Distributive

Let's begin by jotting down a few basic facts about `Distributive`. Here
is a minimalistic definition of the class:

``` haskell
class Functor g => Distributive g where
    distribute :: Functor f => f (g a) -> g (f a)
```

(In what follows, when used as a placeholder name for a functor, `g`
will always stand for a distributive or representable functor, while `f`
will typically stand for the other functor involved in `distribute`.)

`distribute` is dual to `sequenceA`; accordingly, we will adopt the
duals of [the `Traversable` laws](
https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Traversable.html#g:20):
[^data-distributive-laws]

[^data-distributive-laws]: The `Data.Distributive` documentation, as of
  the version 0.6.2.1 of the *distributive* package, mentions a
  different set of properties in lieu of these laws, the crucial one
  being `distribute . distribute = id`. Though that is a viable
  approach, I feel that in the context of what this post aims at such a
  formulation raises more questions than they answer. (For instance,
  regarding `distribute .  distribute = id`: Are there two
  `Distributive` instances involved? If so, how are we supposed to check
  that an individual instance is lawful? Does that law correspond to
  anything from `Traversable`?) That being so, I have chosen to take a
  step back and regard the "dual to `Traversable`" formulation as the
  starting point.

* Identity:

  ``` haskell
  fmap runIdentity . distribute = runIdentity
  ```

* Composition:

  ``` haskell
  fmap getCompose . distribute = distribute . fmap distribute . getCompose
  ```

* Naturality (ensured by parametricity):

  ``` haskell
  -- For any natural transformation t
  -- t :: (Functor f1, Functor f2) => forall x. f1 x -> f2 x
  fmap t . distribute = distribute . t
  ```

  This naturality law is stronger than its `Traversable` counterpart.
  The `Applicative` constraint in `sequenceA` means only natural
  transformations between applicative functors that preserve `pure` and
  `(<*>)` are preserved by `distribute`.  In contrast, `distribute` is
  oblivious to any specifics of `f1` and `f2` functor, and so any
  natural transformation will do.

Homogeneous pairs are one example of a distributive functor:

``` haskell
data Duo a = Duo a a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

fstDuo, sndDuo :: Duo a -> a
fstDuo (Duo x _) = x
sndDuo (Duo _ y) = y

instance Distributive Duo where
    distribute m = Duo (fstDuo <$> m) (sndDuo <$> m)
```

`Duo` will be used in this post as a running example whenever a concrete
illustration of `Distributive` and adjacent classes is called for. For
the moment, here is a simple demonstration of `distribute @Duo` in
action. It illustrates the zip-like flavour of `distribute`, which is
shared by the closely related `collect` and `cotraverse` from
`Data.Distributive`:

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
Duo ["Alex","Geddy","Neil"] ["Lifeson","Lee","Peart"]
```

The function functor is a very important example of `Distributive`.
Consider the following combinator:

``` haskell
flap :: Functor f => f (r -> a) -> r -> f a
flap m = \r -> (\f -> f r) <$> m
```

It changes a `f (r -> a)` functorial value into a `r -> f a` function,
which feeds its argument to all of the available `r -> a` functions.
`flap` is a lawful implementation of `distribute`:

```
instance Distributive ((->) r) where
    distribute = flap
```

``` haskell
ghci> distribute [(*3), (+7), (^2)] 8
[24,15,64]
```

`flap` will be used in this post as a synonym for `distribute @((->) _)`
whenever convenient, or necessary to avoid circularity. [^flap]

[^flap]: The name `flap`, which I have borrowed from [*relude*](
  https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Functor-Fmap.html#v:flap),
  is a play on how `distribute @((->) _) @((->) _)` turns out to be
  `flip`.

### Representable

As for `Representable`, for our immediate purposes it suffices to
characterise it as a class for functors isomorphic to functions:

``` haskell
class Functor g => Representable g where
    type Rep g
    tabulate :: (Rep g -> a) -> g a
    index :: g a -> Rep g -> a
```

Here, `Rep g` is some concrete type such that `tabulate` and `index`
witness an isomorphism between `Rep g -> a` and `g a`. Accordingly, the
laws are:

* Home direction (from `g a` and back):

  ``` haskell
  tabulate . index = id
  ```

* Away direction (to `g a` and back):

  ``` haskell
  index . tabulate = id
  ```

`Duo` can be given a `Representable` instance: pick `Bool` (or any other
type with two inhabitants) as `Rep g`, and associate each possible value
with a component of the pair:

``` haskell
instance Representable Duo where
    type Rep Duo = Bool
    tabulate f = Duo (f False) (f True)
    index (Duo x y) = \case
        False -> x
        True -> y
```

In order to treat the two classes in an even-handed way, I have opted to
leave out the `Distributive g => Representable g` relationship that
exists in the `Data.Functor.Rep` version of `Representable` . In any
case, every representable is indeed distributive, with a default
definition of `distribute` which uses the isomorphism to delegate to
`flap` (that is, `distribute` for functions):

``` haskell
distributeRep :: (Representable g, Functor f) => f (g a) -> g (f a)
distributeRep = tabulate . flap . fmap index
```

The lawfulness of `distributeRep` follows from the lawfulness of `flap`.
[^lawfulness-of-distributeRep]

[^lawfulness-of-distributeRep]: Here is a proof of its lawfulness:

    ``` haskell
    -- Goal (identity law):
    fmap runIdentity . distributeRep = runIdentity
    fmap runIdentity . distributeRep  -- LHS
    fmap runIdentity . tabulate . flap . fmap index
    -- tabulate is natural
    tabulate . fmap runIdentity . flap . fmap index
    -- flap follows the identity law
    tabulate . runIdentity . fmap index
    tabulate . index . runIdentity
    -- tabulate . index = id
    runIdentity  -- LHS = RHS


    -- Goal (composition law):
    fmap getCompose . distributeRep
        = distributeRep . fmap distributeRep . getCompose
    distributeRep . fmap distributeRep . getCompose   -- RHS
    tabulate . flap . fmap index . fmap tabulate
            . fmap flap . fmap (fmap index) . getCompose
    -- index . tabulate = id
    tabulate . flap . fmap flap . fmap (fmap index) . getCompose
    tabulate . flap . fmap flap . getCompose . fmap index
    -- flap follows the composition law
    tabulate . fmap getCompose . flap . fmap index
    -- tabulate is natural
    fmap getCompose . tabulate . flap . fmap index
    fmap getCompose . distributeRep  -- RHS = LHS
    ```

Our ultimate aim here is to go the other way around, from `Distributive`
to `Representable`.

## No need to choose

If we are to start from `Distributive`, though, there is a pretty
fundamental difficulty: setting up a `Representable g` instance requires
picking a suitable `Rep g`, and there is nothing in `Distributive` that
could possibly correspond to such a choice. That being so, we will spend
some more time contemplating `Representable`, looking for a way to
somehow obviate the need for specifying `Rep g`.

### askRep

Let's have another look at the type of `tabulate`:

``` haskell
tabulate :: Representable g => (Rep g -> a) -> g a
```

`tabulate` is a natural transformation from the function functor `((->)
(Rep g)` to `g`. Now, all natural transformations from a function
functor have the form: [^yoneda]

[^yoneda]: That is a manifestation of the Yoneda lemma. For a
  Haskell-oriented introduction to it, see Dan Piponi's [*Reverse
  Engineering Machines with the Yoneda Lemma*](
  http://blog.sigfpe.com/2006/11/yoneda-lemma.html).

``` haskell
-- For some type R, functor G, and any
t :: forall x. (R -> x) -> G x
-- There is a
w :: G R
-- Such that
t f = f <$> w
w = t id
```

In words, the natural transformation must amount to mapping the function
over some functorial value. In our case, `t` is `tabulate`; as
for `w`, we will call it `askRep`, which is [the name it goes by in
`Data.Functor.Rep`](
https://hackage.haskell.org/package/adjunctions-4.4/docs/src/Data-Functor-Rep.html#askRep).
[^askRep]. That being so, we have:

[^askRep]: `askRep` is indeed `ask` for `MonadReader (Rep g) g`;
  accordingly, `tabulate` is `asks`/`reader`.

``` haskell
askRep :: Representable g => g (Rep g)
askRep = tabulate id

tabulate f = f <$> askRep
```

The `Representable` laws can be recast in terms of `askRep` and `index`.
Here is the home direction of the isomorphism:

``` haskell
tabulate . index = id
tabulate (index u) = u
index u <$> askRep = u
```

That is, we can reconstruct any `u :: g a` by taking `askRep` and
replacing every `Rep g` provided by it with the `a` value that applying
`index u` on it gives us.

It is worth noting that `index u <$> askRep = u` also tells us that for
any `u :: g a` there is a function (namely, `index u`) which will change
`askRep` into `u` through `fmap`. That largely corresponds to the
intuition that a representable functor must have a single shape.

The away direction of the isomorphism becomes:

``` haskell
index . tabulate = id
index (f <$> askRep) = f
-- index is a natural transformation
f <$> index askRep = f
-- fmap @((->) _) = (.)
f . index askRep = f
-- In particular, suppose f = id
-- (note that this step is reversible)
index askRep = id
```

Intuitively, if we think of `Rep g` values as corresponding to positions
in the `g` shape that can be queried through `index`, `index askRep =
id` tells us that each and every `Rep g` will be found in `askRep`
occupying the position it corresponds to. For example, with the
`Representable` instance from the previous section, `askRep @Duo` looks
like this:

``` haskell
ghci> askRep @Duo
Duo False True
```

Lastly, we can also express `distributeRep` in terms of `askRep`:

``` haskell
distributeRep m
tabulate (flap (index <$> m))
flap (index <$> m) <$> askRep
(\r -> (\f -> f r) <$> (index <$> m)) <$> askRep
(\r -> (\u -> index u r) <$> m) <$> askRep

distributeRep m = (\r -> (\u -> index u r) <$> m) <$> askRep
```

That is, replace every `Rep g` in `askRep` with the result of using it to
`index` every `g a` in `m`.

### Extracting and revealing

Now let's direct our attention to `index`:

``` haskell
index :: Representable g => g a -> Rep g -> a
```

Flipping `index` gives us:

``` haskell
fromRep :: Representable g => Rep g -> g a -> a
fromRep r = \u -> index u r
```

`fromRep` converts a `Rep g` into what I will call a *polymorphic
extractor*, of type `forall a. g a -> a`, which gives us `a` out of `g
a`. The existence of `fromRep` is quite suggestive. Since `forall a. g a
-> a` doesn't use `Rep g`, finding an inverse to `fromRep`, and thus
showing those two types are isomorphic, might give us a way to work with
`Representable` without relying on `Rep g`.

How might we go about converting a polymorphic extractor into a `Rep g`
value? To do it in a non-trivial way , we will need a `g (Rep g)` source
of `Rep g` on which we can use the extractor.  Considering the
discussion in the previous subsection, `askRep` looks like a reasonable
option:

``` haskell
toRep :: Representable g => (forall x. g x -> x) -> Rep g
toRep p = p askRep
```

Now let's check if `fromRep` and `toRep` are indeed inverses, beginning
with the `toRep . fromRep` direction:

``` haskell
toRep . fromRep
(\p -> p askRep) . (\r -> \u -> index u r)
\r -> (\u -> index u r) askRep
\r -> index askRep r
-- index askRep = id
id
```

We can proceed similarly with `fromRep . toRep`:

``` haskell
fromRep . toRep
(\r -> \u -> index u r) . (\p -> p askRep)
\p -> \u -> index u (p askRep)
```

To simplify this further, we can note that a polymorphic extractor
`forall x. g x -> x` amounts to natural transformation from `g` to
`Identity`. That being so, we have, for any extractor `p` and any `f`:

``` haskell
f . p = p . fmap f
```

The above is the usual naturality property, `fmap f . p = p . fmap f`,
except that, to account for the omission of the `Identity` newtype
boilerplate, `fmap @Identity` has been replaced on the left-hand side by
plain function application. We can now carry on:

``` haskell
\p -> \u -> index u (p askRep)
\p -> \u -> p (index u <$> askRep)
-- index u <$> askRep = u
\p -> \u -> p u
id
```

And there it is: for any `Representable`, `Rep g` must be isomorphic to
`forall x. g x -> x`. That being so, we can use `forall x. g x -> x` as
a default `Rep g` that can be specified in terms of `g` alone. The
change of perspective can be made clearer by setting up an alternative
class:

``` haskell
type Pos g = forall x. g x -> x

elide :: g a -> Pos g -> a
elide u = \p -> p u

class Functor g => Revealable g where
    reveal :: (Pos g -> a) -> g a
    chart :: g (Pos g)

    reveal e = e <$> chart
    chart = reveal id
    {-# MINIMAL reveal | chart #-}
```

Both the arrangement of those definitions and my idiosyncratic choice of
names call for some explanation:

* `Pos g` is a synonym for the type of polymorphic extractors. The name
  `Pos` is short for "position", and is meant to allude to the intuition
  that an extractor picks a value from some specific position in a
  `g`-shaped  structure.

* `elide` corresponds to `index`, defined in such a way that `fromRep =
  id`. Since all it does is applying a `Pos g` extractor, on its own it
  doesn't require any constraints on `g`. The choice of name is
  motivated by how `elide` hides the `g` shape, in that that the only
  information about `u :: g a` that can be recovered from `elide u` are
  the `a` values that a `Pos g` extractor can reach.

* `reveal`, in turn, corresponds to `tabulate`, and is the inverse of
  `elide`. If `g` is `Representable`, the `g` shape can be reconstituted
  with no additional information, and so it is possible to undo the
  hiding performed by `elide`.

* `chart` corresponds to `askRep`, with it and `reveal` being
  interdefinable. In particular, `chart` can be used to reveal the `g
  a` that corresponds to a `Pos g -> a` function by providing the means
  to reach every position in the `g` shape. [^impredicative-types]

[^impredicative-types]: On a technical note, given that the type of
  `chart` amounts to `g (forall x. g x -> x)` using these definitions as
  written requires the `ImpredicativeTypes` extension and, ideally, GHC
  9.2 or above. Doing it without `ImpredicativeTypes` would require
  making `Pos g` a `newtype` instead of a mere synonym.

Here is the `Duo` instance of `Revealable`. Note how each position in
`chart` holds its own extractor:

``` haskell
instance Revealable Duo where
    reveal e = Duo (e fstDuo) (e sndDuo)
    chart = Duo fstDuo sndDuo
```

`distribute` can be implemented for `Revealable` in a way completely
analogous to how it was done for `Representable`:

``` haskell
distributeRev :: (Revealable g, Functor f) => f (g a) -> g (f a)
distributeRev = reveal . flap . fmap elide
```

Or, in terms of `chart`:

``` haskell
distributeRev m = (\p -> p <$> m) <$> chart
```

That is, `distributeRev m` amounts to mapping every extractor in `chart`
over `m`.

As for the laws, just like we were able to choose between expressing the
`Representable` isomorphism directly, via `tabulate`, or indirectly via
`askRep`, here we can use either `reveal` or `chart`:

```
reveal . elide = id
-- Or, equivalently
elide u <$> chart = u

elide . reveal = id
-- Or, equivalently
p chart = p
```

With `Revealable`, though, we can streamline things by showing `p chart
= p` follows from `elide u <$> chart = u`. The proof relies on the
naturality of the polymorphic extractors:

``` haskell
elide u <$> chart = u
-- Apply some p :: Pos g to both sides
p (elide u <$> chart) = p u
-- p is natural
elide u (p chart) = p u
-- elide u p = p u
(p chart) u = p u
-- u is arbitrary
p chart = p
```

That being so, `elide u <$> chart = u` is the only law we need to
characterise `Revealable`. Since `elide` does not depend on the
`Revealable` instance, we might as well inline its definition, which
leaves us with:

``` haskell
(\p -> p u) <$> chart = u
```

I suggest calling it the *law of extractors*: it tells us that the
extractors provided by `chart` suffice to reconstitute an arbitrary `g
a` value.

## Revisiting Distributive

In `Revealable`, we have a class equivalent to `Representable` which
doesn't rely on the `Rep` type family. That makes it feasible to
continue our investigation by attempting to show that every
`Distributive` functor is `Revealable`.

### Natural wonders

Naturality laws and parametricity properties not infrequently have
interesting consequences that seem to us as hidden in plain sight.
Considering the increased strength of `Distributive`'s naturality law
relative to its `Traversable` counterpart and the important role
naturality properties had in setting up `Revealable`, resuming our work
on `Distributive` from the naturality law sounds like a reasonable bet:

``` haskell
-- For any natural transformation t
-- t :: (Functor f1, Functor f2) => forall x. f1 x -> f2 x
fmap t . distribute = distribute . t
```

In particular, suppose `f1` is a function functor:

``` haskell
-- t :: Functor f => forall x. (r -> x) -> f x
t <$> distribute f = distribute (t f)
```

Now, by the same argument used back when we defined `askRep`, `t` must
have the form:

``` haskell
-- m :: f r
t f = f <$> m
```

Therefore:

``` haskell
(\p -> p <$> m) <$> distribute f = distribute (f <$> m)
```

In particular, suppose `f = id`. We then end up with an specification of
`distribute` in terms of `distribute id`:

``` haskell
(\p -> p <$> m) <$> distribute id = distribute m
```

`distribute id` has the following type:

``` haskell
ghci> :t distribute id
distribute id :: Distributive g => g (g a -> a)
```

This looks a lot like something that holds extractors, and the
specification itself mirrors the definition of `distributeRev` in terms
of `chart`. As a preliminary check, `distribute @Duo id` holds `fstDuo`
and `sndDuo` on their respective positions, exactly like `chart @Duo`:

``` haskell
distribute @Duo id
Duo (id <$> fstDuo) (id <$> sndDuo)
Duo fstDuo sndDuo
```

Given the clear resemblance, I will optimistically refer to `distribute
id` as `chartDist`:

```
chartDist :: Distributive g => g (g a -> a)
chartDist = distribute id
```

We therefore have:

```
distribute m = (\p -> p <$> m) <$> chartDist
```

Now suppose `m = Identity u` for some `u :: g a`, and invoke the
identity law:

```
distribute (Identity u) = (\p -> p <$> Identity u) <$> chartDist
distribute (Identity u) = (\p -> Identity (p u)) <$> chartDist
runIdentity <$> distribute (Identity u)
    = runIdentity <$> ((\p ->Identity (p u)) <$> chartDist)
runIdentity <$> distribute (Identity u)
    = (\p -> runIdentity (Identity (p u))) <$> chartDist
runIdentity <$> distribute (Identity u)
    = (\p -> p u) <$> chartDist
-- By the identity law
runIdentity (Identity u) = (\p -> p u) <$> chartDist
u = (\p -> p u) <$> chartDist
```

We therefore have a `Distributive` version of the law of extractors,
with `chartDist` playing the role of `chart`. It is also possible to
turn things around and obtain the identity law from this law of
extractors:

``` haskell
(\p -> p u) <$> chartDist = u
runIdentity . Identity . (\p -> p u) <$> chartDist = u
runIdentity . (\p -> Identity (p u)) <$> chartDist = u
runIdentity . (\p -> p <$> Identity u) <$> chartDist = u
-- distribute m = (\p -> p <$> m) <$> chartDist
runIdentity <$> distribute (Identity u) = u
runIdentity <$> distribute (Identity u) = runIdentity (Identity u)
fmap runIdentity . distribute = runIdentity
```

These are auspicious results. Given that the law of extractors is enough
to establish an implementation of `chart` as lawful, and that there
can't be multiple distinct lawful implementations of `distribute`
[^uniqueness-of-distribute], all we need to do is to identify
`chartDist` with `chart`.

[^uniqueness-of-distribute]: In brief: two implementations of
  `distribute :: f (g a) -> g (f a)` might differ by what they do to the
  `a` values, `f` shapes, or `g` shapes. Naturality means `a` and `f`
  can't be affected by `distribute`, and so any difference would have to
  arise from what is done to `g`. However, the identity law means the
  `g` shape can't be affected either. Therefore, implementations which
  follow the identity law can't differ.

### The roadblock, and a detour

Identifying `chartDist` with `chart`, however, is not trivial. As
similar as `chart` and `chartDist` might feel like, their types differ
in an insurmountable way:

``` haskell
chart @G     :: G (forall a. G a -> a)  -- G (Pos G)
chartDist @G :: forall a. G (G a -> a)
```

In particular:

* The `a` in `forall a. G (G a -> a)` can be directly specialised to a
  concrete choice of `a`, and, as far as the specialised type `G (G A -> A)`
  is concerned, it is conceivable that the involved `G A -> A` functions
  might not be natural in `A`.

* Accordingly, a rank-2 function that takes a `Pos G`, such as the
  argument to `reveal`, can be mapped over `chart`, but not `chartDist`.

* There is no way to obtain the impredicative type of `chart`, or the
  rank-3 type of `reveal`, through `distribute`.

To put it in another way, `chartDist` doesn't have a type strong enough
to, on its own, ensure that it provides natural, polymorphic extractors,
and `Distributive` is not enough to implement a `chart` which provides
such guarantees.

Still, not all is lost. If there is a way to use the laws of
`Distributive` to show that the extractors of `chartDist` are natural,
we should be able to claim `chart` and `chartDist` are morally the same,
providing the same extractors with subtly different types.

(Meta note: while I believe the following argument suffices for the task
at hand, it is not as crystalline as the derivations elsewhere in this
post.  Upgrading it to a proper proof will probably require some tricky
parametricity maneuver which I haven't managed to fully figure out yet.)

Let's turn to the composition law, the one we haven't touched so far:

``` haskell
fmap getCompose . distribute = distribute . fmap distribute . getCompose
```

That is, given some `m :: Compose fo fi (g a)` ("o" is for outer, and
"i" for inner):

``` haskell
getCompose <$> distribute m = distribute (distribute <$> getCompose m)
```

Let's use `distribute m = (\p -> p <$> m) <$> chartDist` on the
left-hand side, and on the outer `distribute` on the right-hand side:

``` haskell
getCompose <$> ((\p -> p <$> m) <$> chartDist)
    = (\q -> q <$> (distribute <$> getCompose m)) <$> chartDist
```

Note that the left-hand side `chartDist` has type `g (g a -> a)`, while
the right-hand side one has type `g (g (fi a) -> fi a)`. Since we can't
take for granted that the extractors provided by them (which are bound
to `p` and `q`, respectively) are natural, it is important to keep track
of this difference.

Tidying the equation a little further, we get:

``` haskell
getCompose <$> ((\p -> p <$> m) <$> chartDist)
    = (\q -> q <$> (distribute <$> getCompose m)) <$> chartDist
(\p -> getCompose (p <$> m)) <$> chartDist
    = (\q -> q . distribute <$> getCompose m) <$> chartDist
(\p -> fmap p <$> getCompose m) <$> chartDist
    = (\q -> q . distribute <$> getCompose m) <$> chartDist
```

On either side of the equation, we have `fmap` being used to obtain a `g
(fo (fi a))` result. That being so, any `fo (fi a)` value that, thanks
to `fmap`, shows up in the left-hand side must also show up in the
right-hand side. More precisely, given any `p :: g a -> a` drawn from
`chartDist` on the left-hand side, there must be some `q :: g (fi a) ->
fi a` drawn from the `chartDist` on the right hand side such that...

``` haskell
fmap p <$> getCompose m = q . distribute <$> getCompose m
```

... and vice versa. That allows us to reason about `p` and `q`, which
amount to the extractors drawn from `chartDist` we are interested in.

As neither `p` nor `q` involve `fo`, and the equation must hold for all
choices of `fo`, we can freely consider the case in which it is
`Identity`, or anything else that has an injective `fmap`. If `fmap` is
injective, the equation further simplifies to:

``` haskell
fmap p = q . distribute
```

Now, `fmap p :: fi (g a) -> fi a` cannot affect the `fi` shape;
therefore, the same holds for `q . distribute :: fi (g a) ->  fi a`.
`distribute :: fi (g a) -> g (fi a)` is natural in `fi`, and so it, too,
can't affect the `fi` shape. It follows that `q :: g (fi a) -> fi a` is
also unable to affect the `fi` shape.

Zooming back out, we have just established that, if the composition law
holds, `chartDist :: g (g (fi a) -> fi a)` only provides extractors that
preserve the `fi` shape.  `chartDist`, however, is defined as
`distribute id :: forall b. g (g b -> b)`, which is fully polymorphic on
the element type `b`. That being so, if there is a way for `distribute
id` to somehow produce non-natural extractors, it cannot possibly rely
in any way about the specifics of `b`. That, in particular, rules out
any means of, given `b ~ fi a` for some functor `fi`, producing just
non-natural extractors that preserve the `fi` shape: such a distinction
cannot be expressed. We must conclude, therefore, that if the
composition law holds `chartDist` can only provide natural extractors,
as we hoped to show.

The converse of this conclusion, by the way, also holds: assuming the
identity law holds, if all `q` drawn from ` chartDist` are natural, the
composition law must hold. To show that, we can use the fact that, for
a natural `q :: forall x. g x -> x`, `q chartDist = q` holds, just like
it does for `chart`:

``` haskell
(\p -> p u) <$> chartDist = u
q ((\p -> p u) <$> chartDist)) = q u
-- Since q is natural, q . fmap f = f . q
(\p -> p u) (q chartDist) = q u
(q chartDist) u = q u
q chartDist = q
```

As a consequence, `q . distribute = fmap q`:

``` haskell
q (distribute m)
q ((\p -> p <$> m) <$> chartDist)
-- q is natural
(\p -> p <$> m) (q chartDist)
(\p -> p <$> m) q
q <$> m
```

We can now return to the rearranged version of the composition law we
were dealing with in the preceding argument, this time without taking it
for granted:

``` haskell
(\p -> fmap p <$> getCompose m) <$> chartDist
    = (\q -> q . distribute <$> getCompose m) <$> chartDist
```

By the above, however, if `q` is natural the right-hand side amounts
to...

``` haskell
(\q -> fmap q <$> getCompose m) <$> chartDist
```

... which is the same as the left-hand side.

### In summary

After quite a long ride, we have managed to shed some light on the
connection between `Distributive` and `Representable`:

* Every `Distributive` is indeed `Representable`, even though, as
  expected, `Representable` cannot be implemented in terms of
  `distribute`.

* The connection is mediated by choosing `forall x. g x -> x`, the type
  of polymorphic extractors, as a default representation, encoded here
  as the `Revealable` class. It can then be shown that this
  representation is mirrored in `Distributive` by `chartDist =
  distribute id :: Distributive g => g (g a -> a)`, which gives a
  corresponding characterisation of `Distributive` in terms of
  extractors.

* The single-shapedness characteristic of both distributive and
  representable functors follows from the identity law of
  `Distributive`.

* The composition law plays an important, if unobvious, role in the
  connection, as it ensures the naturality of the extractors provided by
  `chartDist`, a property that can't be established on the basis of the
  involved types.

## The Select loophole

There is one aspect of our investigation that is worth a closer look. All the
concern with establishing that `chartDist` can only provide natural extractors,
which kept us busy for a good chunk of the previous section, might have felt
surprising. `chartDist`, after all...

``` haskell
chartDist :: forall g a. Distributive g => g (g a -> a)
```

... is fully polymorphic in `a`, and therefore its definition cannot rely on
anything specific about `a`. That being so, it may seem outlandish to suppose
that specialising `chartDist` to, say, `g (g Integer -> Integer)` might somehow
bring forth non-natural `g Integer -> Integer` extractors that perform
`Integer`-specific operations.

To illustrate why the naturality of extractors is, in fact, a relevant issue,
let's consider the curious case of [`Select`](
https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Select.html):

``` haskell
-- A paraphrased, non-transformer version of Select.
newtype Select r a = Select { runSelect :: (a -> r) -> a }

instance Functor (Select r) where
    fmap f u = Select $ \k -> f (u `runSelect` \a -> k (f a))
```

(A `Select r a` value can be thought of as a way to choose an `a` value
based on some user-specified criterion, expressed as an `a -> r`
function.)

Corner cases such as `r ~ ()` aside, `Select r` cannot be
`Representable`, as that would require it to be isomorphic to a function
functor; that being so, it should be similarly ill-suited for
`Distributive`. In spite of that, there is a nontrivial implementation
of a `Select r` combinator with the type `chartDist` would have:
[^chartSelect]

[^chartSelect]: I originally realised it is possible through [a Stack
  Overflow answer by Sergei Winitzki](
  https://stackoverflow.com/a/39736535/2751851). I thank him for helping
  to drive this post to completion, as thinking about `Select` was
  instrumental in putting the pieces together.

``` haskell
chartSelect :: Select r (Select r a -> a)
chartSelect = Select $ \k -> \u -> u `runSelect` \a -> k (const a)
```

What's more, `chartSelect` follows the law of extractors:

``` haskell
-- Goal:
(\p -> p u) <$> chartSelect = u
-- LHS
(\p -> p u) <$> chartSelect
(\p -> p u) <$> Select $ \k -> \u -> u `runSelect` \a -> k (const a)
Select $ \k' ->
    (\p -> p u) (\u -> u `runSelect` \a -> k' ((\p -> p u) (const a)))
Select $ \k' -> u `runSelect` \a -> k' ((\p -> p u) (const a))
Select $ \k' -> u `runSelect` \a -> k' (const a u)
Select $ \k' -> u `runSelect` \a -> k' a
u  -- LHS = RHS
```

That means the `distribute` candidate we get out of `chartSelect`...

``` haskell
nonDistribute :: Functor f => f (Select r a) -> Select r (f a)
nonDistribute m = Select $
    \k -> (\u -> u `runSelect` \a -> k (a <$ m)) <$> m
```

... follows the identity law. As `Select r` is not supposed to be
`Distributive`, we expect `nonDistribute` to break the composition law,
and that is indeed what happens. [^nonDistribute-violation]

[^nonDistribute-violation]: Sparing the very messy full proof, the gist
  of it has to do with the `(<$)` trick `nonDistribute` uses to borrow
  the shape of `m` in order to have something to feed the `k :: f a ->
  r` criterion. In the left-hand side of the composition law, `fmap
  getCompose . distribute`, the trick is applied once, at the outermost
  level, while in the right-hand side, `distribute . fmap distribute.
  getCompose`, thanks to the `fmap distribute` it is also done inside of
  the outer layer. That being so, there is no way the two sides might be
  equal.

Now, by the earlier arguments about the naturality of extractors, if a
candidate implementation of `chartDist` follows the extractors law and
only provides natural extractors, the corresponding `distribute` must
follow the composition law. Since `chartSelect` follows the extractors
law but doesn't give rise to a lawful `distribute`, we must conclude
that it provides non-natural extractors. How does that come to pass?

Every criterion function `k :: a -> r` gives rise to a non-natural
extractor for `Select r a`, namely `\u -> u `runSelect` k :: Select a r
-> a`.  `chartSelect` indirectly makes all these non-natural extractors
available through its own criterion argument, the `k` that shows up in
its definition. (How the encoding works can be seen in the verification
above of the law of extractors: note how performing the `fmap` between
the third and fourth lines of the proof requires replacing `k :: (Select
r a -> a) -> r` with `k' :: a -> r`.)

Non-naturality sneaking into `chartSelect` has to do with `Select r` not
being a strictly positive functor; that is, it has an occurrence of the
element type variable, `a`, to the left of a function arrow. [^polarity]
The lack of strict positivity creates a loophole, through which things
can be incorporated to a `Select r a` value without being specified. It
is a plausible conjecture that the composition law of `Distributive` is
a way of ruling out functors that aren't strictly positive,  with lack
of strict positivity being the only possible source of non-naturality in
`chartDist`, and any non-trivial lack of strict positivity leading to
non-naturality and the composition law being broken. [^shapes-of-Select]

[^polarity]: Though it doesn't explicitly mention strict positivity,
  Michael Snoyman's [*Covariance and Contravariance*](
  https://www.fpcomplete.com/blog/2016/11/covariance-contravariance/) is
  an useful primer on polarity, production and consumption in functors.
  In particular, the `CallbackRunner` example in the "Positive and
  negative position" section towards the end is a `Functor` that isn't
  strictly positive.

[^shape-of-Select]: On a tangential note, the lack of strict positivity
  also breaks down the intuitive notion of the shape, as something that
  can be distinguished from the `a` values contained or produced by some
  functorial value `f a`. While `chartSelect` abiding by the law of
  extractors suggests that we should think of `Select r` as
  single-shaped, it is hard to even tell what a shape is supposed to be
  in this case. If `Select r a` were a garden-variety function type, we
  might say that there is one `a` result for every possible `a -> r`
  criterion. However, the number of possible `a -> r` functions also
  depends on the choice of `a`. As a result, the number of inhabitants
  (that is, distinct possible values) of `Select r a` grows much faster
  than linearly with the number of inhabitants of `a`. Were we to say
  `Select r` is a single-shaped functor, we would have to concede the
  shape is [is bigger on the inside](
  https://tvtropes.org/pmwiki/pmwiki.php/Main/BiggerOnTheInside).

## Further reading and adjacent themes

There are other interesting ways of approaching `Distributive` and
`Representable` that I haven't covered here to avoid making this post
longer than it already is. Here are a few suggestions for further
reading:

* Chris Penner's [*Adjunctions and
  Battleship*](https://chrispenner.ca/posts/adjunction-battleship) post
  is a fine introduction to [`Adjunction`](
  https://hackage.haskell.org/package/adjunctions-4.4/docs/Data-Functor-Adjunction.html),
  the class for **Hask**-**Hask** adjunctions, which provides an
  alternative encoding of `Representable`.

* The following Stack Overflow answers by Conor McBride on *Naperian
  functors*, "Naperian" here being an alternative name for
  `Representable`:

    * [*Which Haskell Functors are equivalent to the Reader functor*](
      https://stackoverflow.com/a/46502280/2751851), which introduces
      Naperian functors in a style reminiscent of the `askRep`-centric
      formuation of `Representable` discussed here.

    * [*Writing cojoin or cobind for n-dimensional grid type*](
      https://stackoverflow.com/a/13100857/2751851), which includes an
      outline of how Naperian functors are handled by container theory.

On a final note, there is a [reworking of `Representable`](
https://github.com/ekmett/distributive/blob/117377d7ba38efc5b115169b565dfb80de8ad407/src/Data/Rep/Internal.hs)
being developed as part of a future release of the *distributive*
package.  When it lands, will unify all the presentations of the concept
we have seen here into a single, more versatile class. I plan to write a
follow-up post on how that will happen.
