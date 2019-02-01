---
title: "Idempotent Effects, Parametricity, and a Puzzle"
license: CC-BY-SA
---

<!--
published: 2017-12-01T23:30:00Z
gh-issue: 14
-->

Some applicative functors have idempotent effects, in the sense that
repeating an effect is the same as having it just once. `Maybe` and
`ZipList` are two examples. This property can be expressed as `f <$>
u <*> u = (\x -> f x x) <$> u`. In a Stack Overflow comment, David Feuer
conjectured that it might be possible to express it as simply `x *>
x = x`. Does that conjecture hold? In this post, I will explain how
I went about solving this problem, while telling a few things I learnt
about paramtericity in the process.

<div></div><!--more-->

Before I begin, a brief historical note. The earliest mention of
idempotency of effects I know of is in *Combining Monads*, a paper by
King and Wadler [^wadler-monads]. There, monads with that property are
referred to as "idempotent monads", which are presented alongside the
most widely known notion of "commutative monads" (`f <$> u <*> v = flip
f <$> v <*> u`). I am using different terminology because the property
generalises straightforwardly to applicative functors, and also because
"idempotent monad" usually means something else in category theory (a
monad that, in Haskell parlance, has a `join` that is an isomorphism).

[^wadler-monads]: One of [Philip Wadler's papers about
monads](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

## Prolegomena

In a bid to reduce clutter in what follows, I will mostly stick to the
[the monoidal presentation of
`Applicative`](http://blog.ezyang.com/2012/08/applicative-functors/),
and make use of a handful of pair manipulation functions:

``` haskell
-- fzip and unit are equivalent in power to (<*>) and pure.
fzip :: Applicative f => (f a, f b) -> f (a, b)
fzip (u, v) = (,) <$> u <*> v

unit :: Applicative f => f ()
unit = pure ()

-- Two teensy little combinators.
app :: (a -> b, a) -> b
app (f, x) = f x

dup :: a -> (a, a)
dup x = (x, x)

{-
The converse definitions of (<*>) and pure would be:

u <*> v = app <$> fzip (u, v)
pure x = const x <$> unit

I will also use the Bifunctor methods for pairs, which amount to:

bimap f g (x, y) = (f x, g y)
first f = bimap f id
second g = bimap id g
-}
--
```

Using that vocabulary, the applicative laws become:

``` haskell
-- "~" here means "the same up to a relevant isomorphism"
fzip (u, unit) ~ u -- up to pairing with ()
fzip (unit, u) ~ u -- up to pairing with ()
fzip (fzip (u, v), w) ~ fzip (u, fzip (v, w)) -- up to reassociating pairs
```

In terms of `fzip`, the idempotency property can be expressed as...

``` haskell
fzip (u, u) = dup <$> u -- fzip . dup = fmap dup
```

... and `(*>)` and its sibling `(<*)` can be defined as:

``` haskell
u <* v = fst <$> fzip (u, v)
u *> v = snd <$> fzip (u, v)
```

(Proofs of the claims just above can be found at the appendix at
the end of this post.)

The conjecture then becomes "`snd <$> fzip (u, u) = u` is equivalent to
`fzip (u, u) = dup <$> u`" [^conjecture-symmetry]. That `fzip (u, u) =
dup <$> u` implies `snd <$> fzip (u, u)` is immediate, as `snd . dup =
id`. Our goal, then, is getting `fzip (u, u) = dup <$> u` out of `snd
<$> fzip (u, u) = u`.

[^conjecture-symmetry]: We might as well have chosen to work with `fst
<$> fzip (u, u) = u` instead, and nothing of any significance would
change -- if we can prove the conjecture for `(*>)`, we can do the same
for the symmetrical conjecture involving `(<*)`.

## A free theorem

How might we get from `snd <$> fzip (u, u) = u` to `fzip (u, u) = dup
<$> u`? It appears we have to deduce something about the first
components of the pairs in `fzip (u, u)` from a fact about the second
components. At first glance, the applicative laws do not suggest any
connection between the components we might exploit. However, `snd <$>
fzip (u, u) = u` also tells us something else: that we can turn `fzip
(u, u)` into `u` using `fmap`; informally, we can say that they have the
same shape. That means `fzip (u, u)` and `u` can be *related* using
`fmap snd` when applying a free theorem. Let's now have a brief look at
what this means in the case of `fzip` [^parametricity].

[^parametricity]: For a gentle initial illustration of the underlying
theme of parametricity, see [*What Does fmap
Preserve?*](/posts/what-does-fmap-preserve.html). For a more thorough
introduction, see [*Parametricity: Money for Nothing and Theorems for
Free*](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free),
by Bartosz Milewski.

The free theorem for `fzip` tells us that `fzip` preserves relations
[^relations], in the sense indicated by this diagram:

[^relations]: A relation is a set of pairs; or, if you will, of
associations between values. As an arbitrary example, we can have a
less-than relation on integers which includes all pairs of integers `(x,
y)` such that `x < y`. In particular, functions are relations: seen as a
relation, a function `f` seen in this way includes all pairs `(x, f x)`,
there being exactly one pair for each possible value of the first
component.

```
              fzip
(F a, F b) ----------> F (a, b)
   |    |                 |  |
   |    |                 |  |
  R|    |S               R|  |S
   |    |                 |  |
   |    |                 |  |
(F c, F d) ----------> F (c, d)
              fzip
```

Here, `a`, `b`, `c` and `d` stand for arbitrary types, and `F` is an
arbitrary applicative functor. `R` is an arbitrary relation between `a`
values and `c` values, and `S` is an arbitrary relation between `b`
values and `d` values. They are lifted across the applicative layer `F`
(so that, informally speaking, values in matching positions of
applicative values with matching shapes are related -- if the relations
are functions, `fmap` can perform that kind of lifting) and across pairs
(so that values taken from matching components of the pairs are related
-- for functions, that is what `first` and `second`, or `bimap`, do). In
what follows, we will use the free theorem by picking relations `R` and
`S` such that the resulting diagram tells us something relevant to our
problem.

## Drawing relations

Having `snd <$> fzip (u, u) = u` as a premise suggests plugging in `snd`
as a relation in the free theorem. The other piece of knowledge we have
right now are the laws; in particular, the associativity law appears
relevant to what we are trying to do. Consider this diagram:

```
                  fzip
(F (a, b), F c) --------> F ((a, b), c)
     |      |                  |     |
     |      |                  |     |
  snd|      |id             snd|     |id
     |      |                  |     |
     v      v                  v     v
(F   b, F   c ) --------> F (  b,    c)
                  fzip

fmap (first snd) . fzip = fzip . first (fmap snd)
```

In particular, for an arbitrary `u :: F A`...

```
first snd <$> fzip (fzip (u, u), u) = fzip (snd <$> fzip (u, u), u)
```

... and, thanks to our `snd <$> fzip (u, u) = u` premise:

```
first snd <$> fzip (fzip (u, u), u) = fzip (u, u)
```

Now, we can get a very similar diagram to work with by nesting the pairs
the other way around:

```
                  fzip
(F a, F (b, c)) --------> F (a, (b, c))
   |      |                  |    |
   |      |                  |    |
 id|      |snd             id|    |snd
   |      |                  |    |
   v      v                  v    v
(F a, F   c   ) --------> F (a,   c   )
                  fzip

fmap (second snd) . fzip = fzip . second (fmap snd)
```

Proceeding just like before, we get:

```
second snd <$> fzip (u, fzip (u, u)) = fzip (u, snd <$> fzip (u, u))
second snd <$> fzip (u, fzip (u, u)) = fzip (u, u)
```

Since `fzip (u, fzip (u, u)) ~ fzip (fzip (u, u), u)` (associativity),
we can shuffle the above into:

```
-- Both first fst and second snd get rid of the value in the middle.
first fst <$> fzip (fzip (u, u), u) = fzip (u, u)
```

The equations we squeezed out of the diagrams...

```
first snd <$> fzip (fzip (u, u), u) = fzip (u, u)
first fst <$> fzip (fzip (u, u), u) = fzip (u, u)
```

... can be combined into:

```
fzip (fzip (u, u), u) = first dup <$> fzip (u, u)
```

At this point, we might have a go at sketching a diagram of a slightly
different nature, which shows relations across the specific values that
appear in the equation above:

```
                                        fzip 
          (u, u) :: (F   a   , F a) -----------> F (  a   , a)
                         |       |                    |     |
                         |       |                    |     |
                        R|      S|               {dup}| {id}|
                         |       |                    |     |
                         |       |      fzip          |     |
(fzip (u, u), u) :: (F (a, a), F a) -----------> F ((a, a), a)

```

`dup` can be used to relate `fzip (u, u)` and `fzip (fzip (u, u), u)` on
the right of the diagram. That this diagram involves specific values
leads to a subtle yet crucial difference from the ones we were working
with up to this point: the relation on the right side is not necessarily
the function `dup`, but some relation that happens to agree with `dup`
*for the specific values we happen to be using here* (that is what I
have attempted to suggest by adding the curly brackets as ad hoc
notation and dropping the arrow tips from the vertical connectors). This
is important, given how `fzip` preserves relations, we
might be tempted to work backwards and identify `R` on the left side
with `dup`, which would give us our proof right there -- `dup <$> u =
fzip (u, u)` would be an immediate consequence. We can't do that,
though, as `R` only has to agree with `dup` for those values which show
up at the relevant places on the right side. In other words, consider
some element `x :: a` of `u`. If `x` shows up as the first component of
any element of `fzip (u, u)`, then the corresponding element of `fzip
(u, u)` must have its first and second components equal to each other
and (since `snd <$> fzip (u, u) = u`) to `x`. If that held for all
elements of `u`, we would have `fzip (u, u) = dup <$> u`. However if `x`
*doesn't* show up as a first component in `fzip (u, u)`, there are no
guarantees, and so we don't have enough grounds.

 <!-- (Divergence.)

Slipping `fmap (first snd)` in both sides leads to (as `snd . dup =
id`):

```
first snd <$> fzip (fzip (u, u), u) = fzip (u, u)
```

-->

Something twisted
----

While those parametricity tricks did not lead to a proof, we did learn
something interesting: the conjecture holds as long as all elements from
`u` show up as first components in `fzip (u, u)`. That sounds like a
decent lead for a counterexample, so let's look for one instead. To
begin with, here is an inoffensive length-two vector type (or an
homogeneous pair):

``` haskell
{-# LANGUAGE DeriveFunctor #-}

data Good a = Good a a
    deriving (Eq, Show, Ord, Functor)
```

The `Applicative` instance is straightforward. Here is its specification
in terms of the monoidal presentation:

``` haskell
unit = Good () ()

fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
```

`Good`, like any other applicative with a single shape, is idempotent --
as there is just one shape, it can't help but be preserved
[^single-shape]. That means we need a second constructor:

[^single-shape]: One way to prove that is by using parametricity in
tandem with the identity laws, analogously to how we used associativity
in the previous section, while exploiting how there being only one shape
means any applicative value can be related to `unit`. 

``` haskell
data Twisted a = Evil a a | Good a a
    deriving (Eq, Show, Ord, Functor)
```

`unit` can remain the same...

``` haskell
unit = Good () ()
```

... which means the `Good`-and-`Good` case *must* remain the same: the
identity effect has to be idempotent [^unit-effect]:

[^unit-effect]: See the previous note about relating things to `unit`.

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = _
fzip (Evil x1 x2, Good y1 y2) = _
fzip (Good x1 x2, Evil y1 y2) = _
```

The twist comes in the `Evil`-and-`Evil` case: we can repeat our pick of
the first element of the vector, and thus leave off one of the first
elements. (We can't do the same with the second element, as we want `snd
<$> fzip (u, u) = u` to hold.)

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y1) (x1, y2)
fzip (Evil x1 x2, Good y1 y2) = _
fzip (Good x1 x2, Evil y1 y2) = _
```

The `Evil`-and-`Good` case is determined by the right identity law...

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y2) (x2, y2)
fzip (Evil x1 x2, Good y1 y2) = Evil (x1, y1) (x2, y2)
fzip (Good x1 x2, Evil y1 y2) = _
```

... while associativity forces our hand in the `Good`-and-`Evil` case
(consider what would happen in a `Good`-`Evil`-`Evil` chain):

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y1) (x1, y2)
fzip (Evil x1 x2, Good y1 y2) = Evil (x1, y1) (x2, y2)
fzip (Good x1 x2, Evil y1 y2) = Evil (x1, y1) (x1, y2)
```


`Evil` spreads, leaving a trail of repeated picks of first elements to
the left of its rightmost occurrence in an applicative chain.

Getting an actual `Applicative` instance from those definitions is easy:
fill `unit` with something, and take away the commas from `fzip`:

``` haskell
instance Applicative Twisted where
    pure x = Good x x

    Good x1 x2 <*> Good y1 y2 = Good (x1 y1) (x2 y2)
    Evil x1 x2 <*> Evil y1 y2 = Evil (x1 y1) (x1 y2)
    Evil x1 x2 <*> Good y1 y2 = Evil (x1 y1) (x2 y2)
    Good x1 x2 <*> Evil y1 y2 = Evil (x1 y1) (x1 y2)
```

And there it is:

``` haskell
GHCi> test = Evil 1 2
GHCi> test *> test
Evil 1 2
GHCi> dup <$> test
Evil (1,1) (2,2)
GHCi> fzip (test, test)
Evil (1,1) (1,2)
GHCi> (\x -> x + x) <$> test
Evil 2 4
GHCi> (+) <$> test <*> test
Evil 2 3
```

Appendix
----

Below are some derivations that might distract from the main thrust of
the post.

### Alternative presentation of the idempotency property

One direction of the equivalency between the two formulations of the
idempotency property follows from a straightforward substitution...

``` haskell
f <$> u <*> u = (\x -> f x x) <$> u
(,) <$> u <*> u = (\x -> (,) x x) <$> u
fzip (u, u) = dup <$> u
```

... while the other calls for a small dose of parametricity.
Parametricity ensures that `fzip` is not affected by using `fmap` and
`bimap` to change values through the `Applicative` layer and the pairs

``` haskell
-- fmap (bimap f g) (fzip (u, v)) = fzip (bimap (fmap f) (fmap g) (u, v))
bimap f g <$> fzip (u, v) = fzip (f <$> u, g <$> v)
-- In particular:
first f <$> fzip (u, v) = fzip (f <$> u, v)
```

That being so, we have:

``` haskell
fzip (u, u) = dup <$> u
first f <$> fzip (u, u) = first f <$> dup <$> u
-- g <$> f <$> u = g . f <$> u
first f <$> fzip (u, u) = (\x -> (f x, x)) <$> u
-- Parametricity: first f <$> fzip (u, v) = fzip (f <$> u, v)
fzip (f <$> u, u) = (\x -> (f x, x)) <$> u
app <$> fzip (f <$> u, u) = app <$> (\x -> (f x, x)) <$> u
f <$> u <*> u = (\x -> f x x) <$> u
```

### Alternative definitions of `(<*)` and `(*>)`

We can start from...

``` haskell
u <* v = const <$> u <*> v
u *> v = flip const <$> u <*> v
```

... we get:

``` haskell
u <* v = app <$> fzip (const <$> u, v)
u *> v = app <$> fzip (flip const <$> u, v)
```

It follows from parametricity that...

``` haskell
u <* v = app . first const <$> fzip (u, v)
u *> v = app . first (flip const) <$> fzip (u, v)
```

... which amount to...

``` haskell
u <* v = (\(x, y) -> const x y) <$> fzip (u, v)
u *> v = (\(x, y) -> flip const x y) <$> fzip (u, v)
```

... or simply:

``` haskell
u <* v = fst <$> fzip (u, v)
u *> v = snd <$> fzip (u, v)
```
