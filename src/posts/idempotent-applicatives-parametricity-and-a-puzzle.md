---
title: "Idempotent Applicatives, Parametricity, and a Puzzle"
license: CC-BY-SA
published: 2019-04-01T02:30:00-03:00
gh-issue: 14
---

Some applicative functors are idempotent, in the sense that repeating an
effect is the same as having it just once. An example and a
counterexample are `Maybe` and `IO`, respectively (contrast `Just 3 *>
Just 3` with `print 3 *> print 3`). More precisely, idempotency means
that `f <$> u <*> u = (\x -> f x x) <$> u`. Given the informal
description I began with, though, one might wonder whether the simpler
property `u *> u = u`, which seems to capture the intuition about
repeated effects, is equivalent to the usual idempotency property. In
this post, I will tell how I went about exploring this conjecture, as
well as a few things I learnt about parametricity along the way.

<div></div><!--more-->

Before I begin, a few remarks about this notion of idempotency. The
earliest mention of it that I know of is in *Combining Monads*, a paper
by King and Wadler [^wadler-monads]. There, idempotent monads are
presented alongside the most widely known concept of commutative monads
(`f <$> u <*> v = flip f <$> v <*> u`). Both properties generalise
straightforwardly to applicative functors, which has the neat
side-effect of allowing myself to skirt the ambiguity of the phrase
"idempotent monad" (in category theory, that usually means a monad that,
in Haskell parlance, has a `join` that is an isomorphism -- a meaning
that mostly doesn't show up in Haskell). Lastly, I knew of the
conjecture about idempotency amounting to `u *> u = u` through a Stack
Overflow comment by David Feuer, and so I thank him for inspiring this
post.

[^wadler-monads]: One of [Philip Wadler's papers about
monads](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html).

## Prolegomena

Given we are looking into a general claim about applicatives, our first
port of call are the applicative laws. Since the laws written in terms
of `(<*>)` can be rather clunky to wield, I will switch to [the monoidal
presentation of
`Applicative`](http://blog.ezyang.com/2012/08/applicative-functors):

``` haskell
-- fzip and unit correspond to (<*>) and pure, respectively.
fzip :: Applicative f => (f a, f b) -> f (a, b)
fzip (u, v) = (,) <$> u <*> v

unit :: Applicative f => f ()
unit = pure ()

```

Note I am using an uncurried version of `fzip`, as I feel it makes what
follows slightly easier to explain. I will also introduce a couple
teensy little combinators so that the required tuple shuffling becomes
easier on the eye:

``` haskell
app :: (a -> b, a) -> b
app (f, x) = f x

dup :: a -> (a, a)
dup x = (x, x)

{-
I will also use the Bifunctor methods for pairs, which amount to:

bimap f g (x, y) = (f x, g y)
first f = bimap f id
second g = bimap id g
-}
--
```

The converse definitions of `pure` and `(<*>)` in terms of `unit` and
`fzip` would be:

``` haskell
u <*> v = app <$> fzip (u, v)
pure x = const x <$> unit
```

Using that vocabulary, the applicative laws become:

``` haskell
-- "~" here means "the same up to a relevant isomorphism"
fzip (u, unit) ~ u -- up to pairing with ()
fzip (unit, u) ~ u -- up to pairing with ()
fzip (fzip (u, v), w) ~ fzip (u, fzip (v, w)) -- up to reassociating pairs
```

As for the idempotency property, it can be expressed as:

``` haskell
fzip (u, u) = dup <$> u -- fzip . dup = fmap dup
```

`(*>)` and its sibling `(<*)` become:

``` haskell
u <* v = fst <$> fzip (u, v)
u *> v = snd <$> fzip (u, v)
```

(Proofs of the claims just above can be found at the appendix at
the end of this post.)

Finally, the conjecture amounts to:

``` haskell
snd <$> fzip (u, u) = u -- u *> u = u
-- Is equivalent to...
fzip (u, u) = dup <$> u -- idempotency
```

That `fzip (u, u) = dup <$> u` implies `snd <$> fzip (u, u)` is
immediate, as `snd . dup = id`. Our goal, then, is getting `fzip (u, u)
= dup <$> u` out of `snd <$> fzip (u, u) = u`.

## Drawing relations

How might we get from `snd <$> fzip (u, u) = u` to `fzip (u, u) = dup
<$> u`? It appears we have to take a fact about the second components of
the pairs in `fzip (u, u)` (note that mapping `snd` discards the first
components) and squeeze something about the first components out of it
(namely, that they are equal to the second components everywhere). At
first glance, it doesn't appear the applicative laws connect the
components in any obviously exploitable way. The one glimmer of hope
lies in how, in the associativity law...

``` haskell
fzip (fzip (u, v), w) ~ fzip (u, fzip (v, w)) -- up to reassociating pairs
```

... whatever values originally belonging to `v` must show up as second
components of pairs on the left hand side, and as first components on
the right hand side. While that, on its own, is too vague to be
actionable, there is a seemingly innocuous observation we can make use
of: `snd <$> fzip (u, u) = u` tells us we can turn `fzip (u, u)` into
`u` using `fmap` (informally, we can say that they have the same shape),
and that they can be *related* using `snd` while making use of a free
theorem [^parametricity]. For our current purposes, that means we can
borrow the types involved in the left side of the associativity law and
use them to draw the following diagram... 

[^parametricity]: For a gentle initial illustration of the underlying
theme of parametricity, see [*What Does fmap
Preserve?*](/posts/what-does-fmap-preserve.html). For a more thorough
introduction, see [*Parametricity: Money for Nothing and Theorems for
Free*](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free),
by Bartosz Milewski.

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
```

... such that we get the same result by following either path from the
top left corner to the bottom right one. Omitting the occurrences of
`id`, we can state that through this equation:

``` haskell
fmap (first snd) . fzip = fzip . first (fmap snd)
```

In words, it doesn't matter whether we use `snd` after or before using
`fzip`. `fmap` and `first`, left implicit in the diagram, are used to
lift `snd` across the applicative layer and the pairs, respectively.
This is just one specific instance of the free theorem; instead of `snd`
and `id`, we could have any functions -- or, more generally, any
relations [^relations]-- between the involved types. Free theorems tell
us about relations being preserved; in this case, `snd` sets up a
relation on the left side of the diagram, and `fzip` preserves it.

[^relations]: A relation is a set of pairs; or, if you will, of
associations between values. As an arbitrary example, we can have a
less-than relation on integers which includes all pairs of integers `(x,
y)` such that `x < y`. In particular, functions are relations: seen as a
relation, a function `f` seen in this way includes all pairs `(x, f x)`,
there being exactly one pair for each possible value of the first
component.

We can get back to our problem by slipping in suitable concrete values
in the equation. For an arbitrary `u :: F A`, we have...

``` haskell
first snd <$> fzip (fzip (u, u), u) = fzip (snd <$> fzip (u, u), u)
```

... and, thanks to our `snd <$> fzip (u, u) = u` premise:

``` haskell
first snd <$> fzip (fzip (u, u), u) = fzip (u, u)
```

Now, why should we restrict ourselves to the left side of the
associativity law? We can get a very similar diagram to work with from
the right side:

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
```

Or, as an equation:

``` haskell
fmap (second snd) . fzip = fzip . second (fmap snd)
```

Proceeding just like before, we get:

``` haskell
second snd <$> fzip (u, fzip (u, u)) = fzip (u, snd <$> fzip (u, u))
second snd <$> fzip (u, fzip (u, u)) = fzip (u, u)
```

Since `fzip (u, fzip (u, u)) ~ fzip (fzip (u, u), u)` (associativity),
we can shuffle that into:

``` haskell
-- Both first fst and second snd get rid of the value in the middle.
first fst <$> fzip (fzip (u, u), u) = fzip (u, u)
```

The equations we squeezed out of the diagrams...

``` haskell
first snd <$> fzip (fzip (u, u), u) = fzip (u, u)
first fst <$> fzip (fzip (u, u), u) = fzip (u, u)
```

... can be combined into:

``` haskell
fzip (fzip (u, u), u) = first dup <$> fzip (u, u)
```

This kind of looks like idempotency, except for the extra occurrence of
`u` tagging along for the ride. We might have a go at getting rid of it
by sketching a diagram of a slightly different nature, which shows how
the relations play out across the specific values that appear in the
equation above:

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
leads to a subtle yet crucial difference from the previous ones: the
relation on the right side is not necessarily the function `dup`, but
some relation that happens to agree with `dup` *for the specific values
we happen to be using here* (that is what I have attempted to suggest by
adding the curly brackets as ad hoc notation and dropping the arrow tips
from the vertical connectors). This is important because, given how
`fzip` preserves relations, we might be tempted to work backwards and
identify `R` on the left side with `dup`, giving us a proof -- `dup <$>
u = fzip (u, u)` would be an immediate consequence. We can't do that,
though, as `R` only must agree with `dup` for those values which show up
in a relevant way on the right side. More explicitly, consider some
element `x :: a` of `u`. If `x` shows up as a first component anywhere
in `fzip (u, u)`, then the corresponding element of `fzip (u, u)` must
have its first and second components equal to each other (because `dup`
agrees with `R` on `x`, and `R` in turn relates `u` and `fzip (u, u)`),
and to `x` (since `snd <$> fzip (u, u) = u`). If that held for all
elements of `u`, we would have `fzip (u, u) = dup <$> u`. However if `x`
*doesn't* show up as a first component in `fzip (u, u)`, there are no
guarantees (as the right side offers no evidence on what `x` is related
to through `R`), and so we don't have grounds for the ultimate claim.

Close, but no cigar.

## Something twisted

While those parametricity tricks gave us no proof, we did learn
something interesting: the conjecture holds as long as all elements from
`u` show up as first components in `fzip (u, u)`. That sounds like a
decent lead for a counterexample, so let's switch course and look for
one instead. To begin with, here is an inoffensive length-two
vector/homogeneous pair type:

``` haskell
{-# LANGUAGE DeriveFunctor #-}

data Good a = Good a a
    deriving (Eq, Show, Ord, Functor)
```

Here is its `Applicative` instance, specified in terms of the monoidal
presentation:

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

The twist comes in the `Evil`-and-`Evil` case: we repeat our pick of a
first element of the vector, and thus discard one of the first elements.
(We can't do the same with the second element, as we want `snd <$> fzip
(u, u) = u` to hold.)

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
(consider what would happen in a `Good`-`Evil`-`Evil` chain
[^twisted-is-lawful]):

[^twisted-is-lawful]: The appendix includes a proof of the lawfulness of
`Twisted`.

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

The conjecture is thus refuted. While parametricity isn't truly
necessary to bring out this counterexample, I am far from sure I would
have thought of it without having explored it under the light of
parametricity. On another note, it is rather interesting that there are
biased applicatives like `Twisted`. I wonder whether less contrived
cases can be found out there in the wild. 

## Appendix

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

... while the other one calls for a small dose of parametricity:

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

Starting from...

``` haskell
u <* v = const <$> u <*> v
u *> v = flip const <$> u <*> v
```

... we can switch to the monoidal presentation:

``` haskell
u <* v = app <$> fzip (const <$> u, v)
u *> v = app <$> fzip (flip const <$> u, v)
```

It follows from parametricity that...

``` haskell
-- Parametricity: first f <$> fzip (u, v) = fzip (f <$> u, v)
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

### Lawfulness of `Twisted` as an applicative functor

Right identity:

``` haskell
fzip (u, unit) ~ u
-- Case: u = Good x1 x2 
fzip (Good x1 x2, Good () ()) -- LHS
Good (x1, ()) (x2, ()) -- LHS ~ RHS
-- Note that Twisted behaves like an ordinary length-two vector as
-- long as only Good is involved. That being so, it would have been
-- fine to skip the Good-only cases here and elsewhere.
-- Case: u = Evil x1 x2
fzip (Evil x1 x2, Good () ()) -- LHS
Evil (x1, ()) (x2, ()) -- LHS ~ RHS
```

Left identity:

``` haskell
fzip (unit, u) ~ u
-- Case: u = Good x1 x2 
fzip (Good () (), Good y1 y2)
Evil ((), y1) ((), y2) -- LHS ~ RHS
-- Case: u = Evil x1 x2 
fzip (Good () (), Evil y1 y2)
Evil ((), y1) ((), y2) -- LHS ~ RHS
```

Associativity:

``` haskell
fzip (fzip (u, v), w) ~ fzip (u, fzip (v, w))
-- Good/Good/Good case: holds.
fzip (fzip (Good x1 x2, Good y1 y2), Good z1, z2) -- LHS
fzip (Good (x1, y1) (x2, y2), Good z1, z2)
Good ((x1, y1), z1) ((x2, y2), z2)
fzip (Good x1 x2, fzip (Good y1 y2, Good z1 z2)) -- RHS
fzip (Good x1 x2, Good (y1, z1) (y2, z2))
Good (x1, (y1, z1)) (x2, (y2, z2)) -- LHS ~ RHS
-- Evil/Evil/Evil case:
fzip (fzip (Evil x1 x2, Evil y1 y2), Evil z1, z2) -- LHS
fzip (Evil (x1, y1) (x1, y2), Evil z1, z2)
Evil ((x1, y1), z1) ((x1, y1), z2)
fzip (Evil x1 x2, fzip (Evil y1 y2, Evil z1 z2)) -- RHS
fzip (Evil x1 x2, Evil (y1, z1) (y1, z2))
Evil (x1, (y1, z1)) (x1, (y1, z2)) -- LHS ~ RHS
-- Good/Evil/Evil case:
fzip (fzip (Good x1 x2, Evil y1 y2), Evil z1, z2) -- LHS
fzip (Good (x1, y1) (x2, y2), Evil z1, z2)
Evil ((x1, y1), z1) ((x1, y1), z2)
fzip (Good x1 x2, fzip (Evil y1 y2, Evil z1 z2)) -- RHS
fzip (Good x1 x2, Evil (y1, z1) (y1, z2))
Evil (x1, (y1, z1)) (x1, (y1, z2)) -- LHS ~ RHS
-- Evil/Good/Evil case:
fzip (fzip (Evil x1 x2, Good y1 y2), Evil z1, z2) -- LHS
fzip (Evil (x1, y1) (x2, y2), Evil z1, z2)
Evil ((x1, y1), z1) ((x1, y1), z2)
fzip (Evil x1 x2, fzip (Good y1 y2, Evil z1 z2)) -- RHS
fzip (Evil x1 x2, Evil (y1, z1) (y1, z2))
Evil (x1, (y1, z1)) (x1, (y1, z2)) -- LHS ~ RHS
-- Evil/Evil/Good case:
fzip (fzip (Evil x1 x2, Evil y1 y2), Good z1, z2) -- LHS
fzip (Evil (x1, y1) (x1, y2), Good z1, z2)
Evil ((x1, y1), z1) ((x1, y2), z2)
fzip (Evil x1 x2, fzip (Evil y1 y2, Good z1 z2)) -- RHS
fzip (Evil x1 x2, Evil (y1, z1) (y2, z2))
Evil (x1, (y1, z1)) (x1, (y2, z2)) -- LHS ~ RHS
-- Evil/Good/Good case:
fzip (fzip (Evil x1 x2, Good y1 y2), Good z1, z2) -- LHS
fzip (Evil (x1, y1) (x2, y2), Good z1, z2)
Evil ((x1, y1), z1) ((x2, y2), z2)
fzip (Evil x1 x2, fzip (Good y1 y2, Good z1 z2)) -- RHS
fzip (Evil x1 x2, Good (y1, z1) (y2, z2))
Evil (x1, (y1, z1)) (x2, (y2, z2)) -- LHS ~ RHS
-- Good/Evil/Good case:
fzip (fzip (Good x1 x2, Evil y1 y2), Good z1, z2) -- LHS
fzip (Evil (x1, y1) (x1, y2), Good z1, z2)
Evil ((x1, y1), z1) ((x1, y2), z2)
fzip (Good x1 x2, fzip (Evil y1 y2, Good z1 z2)) -- RHS
fzip (Good x1 x2, Evil (y1, z1) (y2, z2))
Evil (x1, (y1, z1)) (x1, (y2, z2)) -- LHS ~ RHS
-- Good/Good/Evil case:
fzip (fzip (Good x1 x2, Good y1 y2), Evil z1, z2) -- LHS
fzip (Good (x1, y1) (x2, y2), Evil z1, z2)
Evil ((x1, y1), z1) ((x1, y1), z2)
fzip (Good x1 x2, fzip (Good y1 y2, Evil z1 z2)) -- RHS
fzip (Good x1 x2, Evil (y1, z1) (y1, z2))
Evil (x1, (y1, z1)) (x1, (y1, z2)) -- LHS ~ RHS
```

