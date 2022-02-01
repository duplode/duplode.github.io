---
title: "Distributive Draft 2"
license: CC-BY-SA
published: 2022-02-07T22:00:00-03:00
gh-issue: 17
toc: true
---

"Every `Distributive` `Functor` is actually `Representable`", as the
documentation for [`Representable`](
https://hackage.haskell.org/package/adjunctions-4.4/docs/Data-Functor-Rep.html)
tells us straight away, and yet it is far from obvious why that should
be the case.  At first glance, [`Distributive`](
https://hackage.haskell.org/package/distributive-0.6.2.1/docs/Data-Distributive.html),
the dual to `Traversable`, appears to have little if anything to do with
`Representable`, the class for functors isomorphic to functions.  The
goal of this post is making this connection explicit. In the process, we
will tease out a fair amount of information from the austere definition
of `Distributive`, and also build a clearer picture of what makes it
tricky to fully bridge the gap to `Representable`, a difficulty which is
in itself interesting.

<!-- more -->

## Distributive: the basics

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

It is worth noting that the naturality law of `Distributive` is stronger
than its `Traversable` counterpart. The `Applicative` constraint in
`sequenceA` means only natural transformations between applicative
functors that preserve `pure` and `(<*>)` are preserved by `distribute`.
In contrast, `distribute` is oblivious to any specifics of `f1` and `f2`
functor, and so any natural transformation will do.

Notably, the function functor is distributive.  A `f (r -> a)`
functorial value can be turned into a `r -> f a` function which feeds
its argument to all available `r -> a` functions. The combinator which
performs this conversion follows the distributive laws:

``` haskell
flap :: Functor f => f (r -> a) -> r -> f a
flap m = \r -> (\f -> f r) <$> m

instance Distributive ((->) r) where
    distribute = flap
```

``` haskell
ghci> distribute [(*3), (+7), (^2)] 8
[24,15,64]
```

`flap`, a name which I have borrowed from [*relude*](
https://hackage.haskell.org/package/relude-1.0.0.1/docs/Relude-Functor-Fmap.html#v:flap),
will be used in this text as a synonym for `distribute @((->) _)`
whenever convenient, or necessary to avoid circularity. [^flap]

[^flap]: The name `flap` is a play on how `distribute @((->) _) @((->)
  _)` turns out to be `flip`. `flap` is also known as [`(??)`](
  https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Lens.html#v:-63--63-),
  the choice of operator being similarly motivated. For instance, `elem
  ?? [1,3,5] $ 3` amounts to `elem 3 [1,3,5]`, as if the `??` were
  marking where the other argument should go.

Homogenous pairs provide a straightforward example of a non-function
distributive, which we will default to whenever a tangible distributive
is needed to illustrate a point:

``` haskell
data Duo a = Duo a a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

fstDuo, sndDuo :: Duo a -> a
fstDuo (Duo x _) = x
sndDuo (Duo _ y) = y

instance Distributive Duo where
    distribute m = Duo (fstDuo <$> m) (sndDuo <$> m)
```

`distribute` often amounts to transposition:

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

For a variation on theme, we can eliminate the `f` functorial layer in
the result of `distributive` with an `f a -> b` function. That gives
rise to `cotraverse`:

``` haskell
cotraverse :: (Distributive g, Functor f) => (f a -> b) -> f (g a) -> g b
cotraverse f = fmap f . distribute
```

It has a markedly zip-like flavour:

``` haskell
powers :: [Duo Integer]
powers = (\x -> Duo (x^2) (x^3)) <$> [1..5]
```

``` haskell
ghci> powers
[Duo 1 1, Duo 4 8, Duo 9 27, Duo 16 64, Duo 25 125]
ghci> cotraverse sum powers
Duo 55 225
```

We will find out more about `distribute` and `Distributive` in due course.
For now, let's take a moment to consider `Representable`, with an eye
towards bridging the gap between the classes.

## Representable: the basics

For our immediate purposes, it suffices to characterise `Representable`
as a class for functors isomorphic to functions:

``` haskell
class Functor g => Representable g where
    type Rep g :: Type
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

I have opted to leave out the `Distributive g => Representable g`
relationship imposed by `Data.Functor.Rep` in order to treat the classes
in an even-handed way. In any case, every representable is distributive,
with a default definition of `distribute` which uses the isomorphism to
delegate to `flap` (that is, `distribute` for functions):

``` haskell
distributeRep :: Representable g => f (g a) -> g (f a)
distributeRep = tabulate . flap . fmap index
```

Our overall goal here is to go, in the opposite direction, from
`Distributive` to `Representable`. Right at the outset, there is a
fairly fundamental complication: while setting up a `Representable g`
instance requires picking a suitable `Rep g`, there is nothing in
`Distributive`, as defined in the previous section, that could possibly
correspond to such a choice. That being so, our next step will be
exploring what else a `Representable` issue tells us about `g`, while
looking for a way to somehow obviate the need for specifying `Rep g`.
