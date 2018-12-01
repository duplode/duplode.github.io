---
title: "Idempotent Effects, Parametricity, and a Puzzle"
license: CC-BY-SA
---

<!--
published: 2017-12-01T23:30:00Z
gh-issue: 14
-->

Some applicative functors have idempotent effects, in the sense that
repeating an effect leaves it unchanged. `Maybe` and `ZipList` are two
examples. This property can be expressed as `f <$> u <*> u = (\x -> f x
x) <$> u`. In a Stack Overflow comment, David Feuer raised the
conjecture that it might be possible to simplify the property to `x *> x
= x`. Looking closely at this conjecture made me learn a few things
about parametricity, and gave me a tale worth telling.

<div></div><!--more-->

Before I begin, a brief historical note. The earliest mention of
idempotency of effects I know of is in *Combining Monads*, by King and
Wadler [^wadler-monads]. There, it is referred to as "idempotent
monads", and is presented alongside the more widely know notion of
"commutative monads" (`f <$> u <*> v = flip f <$> v <*> u`). I am using
different terminology because the property generalises straightforwardly
to applicative functors, and also because "idempotent monad" usually
means something else in category theory (a monad that, in Haskell
parlance, has a `join` that is an isomorphism).

[^wadler-monads]: One of [Philip Wadler's papers about
monads](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

The monoidal presentation
----

Expressing the property in terms of [the monoidal presentation of
`Applicative`](http://blog.ezyang.com/2012/08/applicative-functors/)
helps to clear the view. Instead of `(<*>)` and `pure`, we will use:

``` haskell
fzip :: Applicative f => (f a, f b) -> f (a, b)
fzip (u, v) = (,) <$> u <*> v

unit :: Applicative f => f ()
unit = pure ()
```

Since I will be using pairs and uncurried functions a lot, to keep the
derivations tidy I will employ a couple pointfree helpers...

``` haskell
app :: (a -> b, a) -> b
app (f, x) = f x

dup :: a -> (a, a)
dup x = (x, x)
```

... as well as the
[`Bifunctor`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html)
methods for pairs:

``` haskell
bimap f g (x, y) = (f x, g y)
first f = bimap f id
second g = bimap id g
```

With this vocabulary, the usual `Applicative` methods can be defined as:

``` haskell
u <*> v = app <$> fzip (u, v)
pure x = const x <$> unit
```

... and the laws become:

``` haskell
-- "~" here means "the same up to a relevant isomorphism"
fzip (u, unit) ~ u -- up to pairing with ()
fzip (unit, u) ~ u -- up to pairing with ()
fzip (fzip (u, v), w) ~ fzip (u, fzip (v, w)) -- up to reassociating pairs
```

The idempotency property can be expressed in terms of `fzip` as:

``` haskell
fzip (u, u) = dup <$> u
```

`(*>)` and its sibling `(<*)` have the following alternative
definitions:

``` haskell
u <* v = fst <$> fzip (u, v)
u *> v = snd <$> fzip (u, v)
```

(Derivations of the claims above can be found in the appendix at the end
of this post.)

The conjecture then becomes "`snd <$> fzip (u, u) = u` is equivalent to
`fzip (u, u) = dup <$> u`". For the sake of readability, I will instead
work with the symmetrical premise `fst <$> fzip (u, u) = u` (that is, `u
<* u = u`) -- what goes for one also goes for the other.

Warming up
----

How might we get from `fst <$> fzip (u, u) = u` to `fzip (u, u) = dup
<$> u`? It appears we have to deduce something about the second
components of the pairs in `fzip (u, u)` from a fact about the first
components. At first glance, the applicative laws do not suggest any
connection between the components. However, `fst <$> fzip (u, u) = u`
also means that `fzip (u, u)` and `u` can be related through `fmap fst`
when applying a free theorem. As a preliminary demonstration of the sort
of trick I will make use of, I will first work out a simpler scenario:
showing, through parametricity, that applicative functors with only one
possible shape necessarily have idempotent effects [^parametricity].

[^parametricity]: For a gentle initial illustration of parametricity,
see [*What Does fmap Preserve?*](/posts/what-does-fmap-preserve.html).
For a more thorough introduction, see [*Parametricity: Money for Nothing
and Theorems for
Free*](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free),
by Bartosz Milewski.

The free theorem for `fzip` tells us that `fzip` preserves relations, in
the sense indicated by this diagram:

```
              fzip
(F A, F B) ----------> F (A, B)
   |    |                 |  |
   |    |                 |  |
  R|    |S               R|  |S
   |    |                 |  |
   |    |                 |  |
(F C, F D) ----------> F (C, D)
              fzip
```

Here, `A`, `B`, `C` and `D` are arbitrary types, and `F` is an arbitrary
applicative functor. `R` is an arbitrary relation between `A` values and
`C` values, and `S` is an arbitrary relation between `B` values and `C`
values. They are lifted across pairs (to hold between matching
components) and the applicative layer `F` (to hold between values in,
informally speaking, matching positions of applicative values with
matching shapes). If we substitute the `id` function for `R`, `const ()`
for `S`, and `A` for `B`, we get:

```
              fzip
(F A, F A) ----------> F (A, A)
   |    |                 |  |
   |    |                 |  |
 id|    |const ()       id|  |const ()
   |    |                 |  |
   v    v                 v  v
(F A, F ()) ---------> F (A, ())
              fzip

fmap (second (const ())) . fzip = fzip . second (fmap (const ()))
```

If some `u :: F A` and `unit :: F ()` have the same shape, they can be
related through `const ()`, as `unit = const () <$> u`. That being so,
we have:

``` haskell
second (const ()) <$> fzip (u, u) = fzip (u, unit)
```

Since `fzip (u, unit) ~ u` (right identity law of `Applicative`), it
follows that `fst <$> fzip (u, u) = u`. A symmetrical argument relying
on the left identity law gives `snd <$> fzip (u, u) = u`; combining the
two leads to `fzip (u, u) = dup <$> u`. We have just shown that any
effect matching that of `unit` must be idempotent. In particular, if an
applicative functor only has a single possible effect (or shape), its
effect must be idempotent. Examples include the function/`Reader`
applicative, and more generally any `Applicative` which is also
[`Representable`](http://hackage.haskell.org/package/representable-functors-3.2.0.2/docs/Data-Functor-Representable.html),
including infinite streams and homogeneous tuples.

The real deal
----

To decide about the conjecture, though, we can't restrict ourselves to
single shape applicative functors, and so the identity laws won't lead
to a proof that it holds. We have to resort to associativity. Consider
this diagram, which exploits our `fst <$> fmap (u, u) = u` premise:

```
                  fzip
(F (A, A), F A) --------> F ((A, A), A)
     |      |                  |     |
     |      |                  |     |
  fst|      |id             fst|     |id
     |      |                  |     |
     v      v                  v     v
(F   A, F   A ) --------> F (  A,    A)
                  fzip

first fst <$> fzip (fzip (u, u), u) = fzip (fst <$> fzip (u, u), u)
first fst <$> fzip (fzip (u, u), u) = fzip (u, u)
```

In this case, it is helpful to proceed by regarding the mediating
functions as relations, and consider what they end up relating within
the applicative values. I will use `i`, `j` and `k` indices to refer to
values drawn from arbitrary positions of `u` [^shapes-and-positions].
For instance, let's say that the *i*-th position in `fzip (u, u)` holds
the pair `(i, j)` (the first component must be `i` because `fst <$> fzip
(u, u) = u`). We can use the diagram above as a scheme for figuring out
which constraints that imposes on corresponding positions around the
diagram. Writing `i` and `j` on the lower right corner and tracing the
relations from there, we get:

[^shapes-and-positions]: At this point, one could pose some really
thorny questions about what exactly do I mean by "shapes" and
"positions", specially when it comes to functors which aren't strictly
positive. I believe that, for our current purposes, appealing to the
relational parametricity machinery as a way to specify some notion of
position would be enough to bail me out of such a bind, but my picture
of how to go about that is, for the moment, fuzzy.

```
                  fzip
({(i, j)}, {j}) --------> {((i, j), j)}
    |       |                 |     |
    |       |                 |     |
 fst|       |id            fst|     |id
    |       |                 |     |
    v       v                 v     v
({  i   }, {j}) --------> {(  i,    j)}
                  fzip
```

(The curly brackets correspond to the occurrences of `F` in the original
diagram, and clarify what is bundled in a single position of an
applicative value.)

We also have a second, very similar diagram to work with:

```
                  fzip
(F A, F (A, A)) --------> F (A, (A, A))
   |      |                  |    |
   |      |                  |    |
 id|      |fst             id|    |fst
   |      |                  |    |
   v      v                  v    v
(F A, F   A   ) --------> F (A,   A   )
                  fzip

second fst <$> fzip (u, fzip (u, u)) = fzip (fst <$> fzip (u, u), u)
second fst <$> fzip (u, fzip (u, u)) = fzip (u, u)
```

An analogous diagram of positions follows:

```
                  fzip
({i}, {(j, k)}) --------> {(i, (j, k)}
  |      |                  |    |
  |      |                  |    |
id|      |fst             id|    |fst
  |      |                  |    |
  v      v                  v    v
({i}, {  j   }) --------> {(i,   j  )}
                  fzip
```

(`(j, k)` is the pair in the *j*-th position of `fzip (u, u)`.)

The upper right corners of the position diagrams tell us the *i*-th
position in `fzip (fzip (u, u), u)` and `fzip (u, fzip (u, u))` holds
`((i, j), j)` and `(i, (j, k))`, respectively. However, we know that
`fzip (fzip (u, u), u) ~ fzip (u, fzip (u, u))` (associativity), and so
`j = k`. That brings us tantalisingly close to proving the conjecture,
but not really there: if `fst <$> fzip (u, u) = u`, and the value from
the *j*-th position of `u` is drawn as the second component of any
position in `fzip (u, u)`, then the two components from the *j*-th
position of `fzip (u, u)` are equal. That does not mean *all* positions
have their two components equal to each other, as not all positions must
be drawn as a second component -- repeat picks are possible.

In the end, this strategy didn't lead us to a proof of the conjecture. What
about a refutation, though? 


Something twisted
----

Reading between the lines of what we learned through those parametricity
tricks gives us just enough material to tailor a counterexample to the
conjecture.

Let's begin with something nice and simple: an homogeneous pair:

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

However, we know that applicatives with a single shape are idempotent no
matter what. Therefore, `Good` won't do. We need a second constructor:

``` haskell
data Twisted a = Evil a a | Good a a
    deriving (Eq, Show, Ord, Functor)
```

`unit` can remain the same...

``` haskell
unit = Good () ()
```

... which means the `Good`-and-`Good` case *must* remain the same: the
identity effect has to be idempotent:

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = _
fzip (Good x1 x2, Evil y1 y2) = _
fzip (Evil x1 x2, Good y1 y2) = _
```

The twist comes in the `Evil`-and-`Evil` case: by the argument in the
previous section, we are allowed to repeat our pick of second component
of the pairs:

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y2) (x2, y2)
fzip (Good x1 x2, Evil y1 y2) = _
fzip (Evil x1 x2, Good y1 y2) = _
```

The `Good`-and-`Evil` case is determined by the left identity law...

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y2) (x2, y2)
fzip (Good x1 x2, Evil y1 y2) = Evil (x1, y1) (x2, y2)
fzip (Evil x1 x2, Good y1 y2) = _
```

... while associativity forces our hand on the `Evil`-and-`Good` case:

``` haskell
fzip (Good x1 x2, Good y1 y2) = Good (x1, y1) (x2, y2)
fzip (Evil x1 x2, Evil y1 y2) = Evil (x1, y2) (x2, y2)
fzip (Good x1 x2, Evil y1 y2) = Evil (x1, y1) (x2, y2)
fzip (Evil x1 x2, Good y1 y2) = Evil (x1, y2) (x2, y2)
```

`Evil` spreads, leaving a trail of values from the second position being
picked to fill the first one to the right of where it is first
introduced.

Getting an actual `Applicative` instance from those definitions is easy:
fill `unit` with something, and take away the commas from `fzip`:

``` haskell
instance Applicative Twisted where
    pure x = Good x x

    Good x1 x2 <*> Good y1 y2 = Good (x1 y1) (x2 y2)
    Evil x1 x2 <*> Evil y1 y2 = Evil (x1 y2) (x2 y2)
    Good x1 x2 <*> Evil y1 y2 = Evil (x1 y1) (x2 y2)
    Evil x1 x2 <*> Good y1 y2 = Evil (x1 y2) (x2 y2)
```

And there it is:

``` haskell
GHCi> test = Evil 1 2
GHCi> test <* test
Evil 1 2
GHCi> dup <$> test
Evil (1,1) (2,2)
GHCi> fzip (test, test)
Evil (1,2) (2,2)
GHCi> (\x -> x + x) <$> test
Evil 2 4
GHCi> (+) <$> test <*> test
Evil 3 4
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
