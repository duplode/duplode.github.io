---
title: "Winding Recursion Schemes"
license: CC-BY-SA
gh-issue: 16
---

Recursion schemes are lovely creatures, even if their tongue-twisting
names might suggest otherwise. Recoginising them can be useful for
working with data structures, as they provide orientation for navigating
through trick recursive and corecursive implementations. In this post, I
will present a handful of related recursion schemes, including
zygomorphisms, paramorphisms, apomorphisms, and more.

<div></div><!-- more -->

I will use catamorphisms -- the common fold -- as a starting point, and
the *recursion-schemes* conventions as framing. Both of these topics are
covered by [*What's in a fold*](/posts/whats-in-a-fold.html), my
previous post about recursion schemes, so if this is the first time you
meet either of them you will likely find that post useful as preliminary
reading.

Before we begin, here are all of the imports and pragmas which are
needed for some example in the text, gathered in one place so that you
can easily drop them in a file:

``` haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Numeric.Natural
import Control.Arrow
import qualified Control.Foldl as L
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
```

## Entwining folds

This is our friend `cata`, the catamorphism:

``` haskell
cata :: Recursive t => (Base t b -> b) -> t -> b
cata f = c
    where 
    c = f . fmap c . project 
```

In a nutshell, `cata` takes a function -- the *algebra* -- that
specifies how to consume one layer of recursive structure, and returns a
function that consumes the whole structure. To pick a traditional
example, here are two algebras for lists [^listf]...

[^listf]: They are written in terms of `ListF`, the *base functor* of
  lists.

``` haskell
sumAlg :: Num a => ListF a a -> a
sumAlg = \case
    Nil -> 0
    Cons a s -> a + s

lenAlg :: ListF a Natural -> Natural
lenAlg = \case
    Nil -> 0
    Cons _ n -> 1 + n
```

... and the corresponding folds:

``` haskell
sumList :: Num a => [a] -> a
sumList = cata sumAlg

lengthList :: [a] -> Natural
lengthList = cata lenAlg
```

With `sumList` and `lengthList` available, it seems straightforward to
calculate the average of list elements:

``` haskell
averageList :: Fractional a => [a] -> a
averageList xs = sumList xs / fromIntegral (lengthList xs)
```

There is one problem, though. This implementation of `averageList`
performs two passes through `xs`. That means it takes up more time than
a single-pass implementation would. More worringly, after the first pass
is done `xs` must be retained in memory, which can eat up lots of memory
if the list is long. Ideally, we would rather have a single algebra...

``` haskell
sumAndLenAlg :: Num a => ListF a (a, Natural) -> (a, Natural)
```

... that we could give `cata` in order to get sum and length with a
single fold. The question, then, is: can we define this algebra in terms
of `sumAlg` and `lenAlg`, so that we don't have to wire it up all
over again?

(At this point. you might be finding this example a bit questionable.
After all, `cata` for lists is a lazy right fold, while collapsing a
list into a number is better done, if we truly care about performance,
with a strict left fold -- in `Data.List` parlance, with `foldl'`
instead of `foldr`. Please bear with me: I will address this objection
later on.)

### mutu

One approach to this problem is looking at the base libraries to see if
anything there could help with fusing two algebras. The shuffling of
pairs and functions suggests `Control.Arrow`. In particular, `(&&&)`
specialised to functions has type:

``` haskell
(&&&) :: (r -> a) -> (r -> b) -> (r -> (a, b))
```

If the result is to be an *F-algebra*, that is, an ordinary algebra we
can give to `cata`, `r` should be `Base t (a, b)`. That leaves us with:

``` haskell
(&&&) :: Recursive t
    => (Base t (a, b) -> a)
    -> (Base t (a, b) -> b)
    -> (Base t (a, b) -> (a, b))
```

This is not exactly what we wanted, but it is pretty close. The
arguments are not F-algebras with types `Base t a -> a` and `Base t b ->
b`. Instead, each of the algebras has access to its intermediate result
*and also* to the intermediate result of the other algebra. In effect,
we can use `(&&&)` to run two mutually dependent folds in tandem.

``` haskell
\f g -> cata (f &&& g) :: Recursive t
    => (Base t (a, b) -> a)
    -> (Base t (a, b) -> b)
    -> (t -> (a, b))
```

That is interesting enough for us to suspend our original problem for a
moment in order to identify a different recursion scheme: the
*mutumorphism*.

``` haskell
mutu :: Recursive t
    => (Base t (a, b) -> a)
    -> (Base t (a, b) -> b)
    -> (t -> b)
mutu f g = snd . cata (f &&& g)
```

The `snd` at the end is there because the mutumorphism combinator is
usually defined so that the ultimate result of the fold is that given by
one of the algebras, with the other one being deemed as a mere helper.

(Note that *recursion-schemes* doesn't provide a ready-made `mutu`. If
you feel like using one, there is Vanessa McHale's
[*recursion-schemes-ext*](http://hackage.haskell.org/package/recursion-schemes-ext-1.0.0.4/docs/Data-Functor-Foldable-Exotic.html#v:mutu)
on Hackage.)

As a quick example of what can be encoded with a mutumorphism, here is a
function that skips every other element in a list [^this-that-so]:

[^this-that-so]: Example adapted from [*Getting even and odd position of
  elements in list - Haskell Mutual
  Recursion*](https://stackoverflow.com/q/49843681/2751851), asked by
  [ArshSoni](https://stackoverflow.com/users/8279634/arshsoni) at Stack
  Overflow.

``` haskell
skipEveryOther :: [a] -> [a]
skipEveryOther = mutu thatAlg thisAlg
    where
    thatAlg = \case
        Nil -> []
        Cons _ (_, this) -> this
    thisAlg = \case
        Nil -> []
        Cons a (that, _) -> a : that
```

The algebras cooperate by exchanging `this` and `that` at every step,
while `thisAlg` adds an element to `that` (which becomes `this` for the
next step). Note we could also get hold of the other list (the one with
the skipped elements) by directly using `cata (thatAlg &&& thisAlg)`.

### zygo

We often don't need the full power of mutual recursion provided by
`mutu`, as *semi-mutual recursion* -- that is, a main fold that depends
on an auxiliary one, but not vice-versa -- turns out to be enough.  In
such cases, we might as well tweak `mutu` to prune what we won't use and
get something with a simpler type signature:

``` haskell
zygo :: Recursive t
    => (Base t a -> a)
    -> (Base t (a, b) -> b)
    -> (t -> b)
zygo f g = snd . cata (f . fmap fst &&& g)
```

The semi-mutual recursion scheme `zygo` implements is known as
*zygomorphism* [^zygo-etymology]. In it, the `fmap fst` discards the
result of the other algebra, breaking one of the links between them and
allowing us to use a plain F-algebra as `f` [^zygo-recursion-schemes].

[^zygo-etymology]: "zygo" is a Greek prefix meaning "yoked", or
  "paired", most often seen in biological terms.

[^zygo-recursion-schemes]: The implementation of `zygo` in
  *recursion-schemes* is equivalent to the one here, though that fact is
  somewhat obscured by the "recursion schemes from comonads" machinery
  being used over there.

Let's have a look at a couple examples of `zygo` in action. To begin
with, here is a function that doubles every other element in a list of
numbers [^double-every-other-so]:

[^double-every-other-so]: Example adapted from my answer to [*What to
  use instead of explicit recursion in
  Haskell?*](https://stackoverflow.com/q/23842473/2751851) , asked by
  [Harold Carr](https://stackoverflow.com/users/814846/haroldcarr) at
  Stack Overflow.

``` haskell
doubleEveryOther :: Num a => [a] -> [a]
doubleEveryOther = zygo flagAlg emitAlg
    where
    flagAlg = \case
        Nil -> False
        Cons _ flag -> not flag
    emitAlg = \case
        Nil -> []
        Cons a (flag, as) -> (if flag then 2*a else a) : as
```

By structuring it as a zygomorphism, we separate the decision about
whether the next element should be doubled (the `Bool`-returning
auxiliary algebra `flagAlg`) from the actual emission of said element
(`emitAlg`).

There is one general point about folds that this example illustrates
quite well. I suggest spending a moment to hazard a guess: Which
elements do you think that will be doubled in `doubleEveryOther [1..4]`?
And in `doubleEveryOther [1..5]`?

``` haskell
GHCi> doubleEveryOther [1..4]
[2,2,6,4]
GHCi> doubleEveryOther [1..5]
[1,4,3,8,5]
```

Given that `flagAlg` gives out `False` in the base case, the rightmost
element of the list is not doubled, and so `doubleEveryOther`, perhaps
counterintuitively, doubles every other element starting from the right.
Additionally, since `emitAlg` always uses the result of `flagAlg` from the
previous fold step, `doubleEveryOther` cannot handle infinite lists:

``` haskell
GHCi> take 10 $ doubleEveryOther [1..]
[
```

We will revisit the issues with this example later on. For now, the main
takeaway is that, generally speaking, *fold results are computed from
tip to handle*.  In the case of a list, the tip is the `[]` at the end
of every (finite) list, and the handle is the first constructor met by
anything that consumes it. That being so, tip and handle amount to right
and left, respectively [^direction-of-a-fold].

[^direction-of-a-fold]: I am deliberately using nonstandard terminology
  because the language here is terrifyingly slippery. The quandary is
  precisely captured in [a
  footnote](https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/#fn2)
  to Patrick Thomson's *Recutsion Schemes, Part IV*:

  > Rob Rix points out that, though catamorphisms are often described as
  > “bottom-up”, this term is ambiguous: catamorphisms’ recursion occurs
  > top-down, but the folded value is constructed bottom-up. I had never
  > noticed this ambiguity before.

  "Recursion occurs top-down" (that is, from handle to tip) because
  operationally there is no other way to move along the spine of a data
  structure -- the handle is where you can put your hands on. That is
  what makes it possible to, given a sufficiently lazy algebra,
  implement structure-building functions such as `map` and `filter` (or,
  for that matter, `skipEveryOther`) with a fold and still be able to
  get useful partial results out of even an infinite list.  (For a
  crystalline illustration of this perspective, see [*Drawing foldl and
  foldr*](http://www.joachim-breitner.de/blog/753-Drawing_foldl_and_foldr)
  by Joachim Breitner.)

  Here, I am emphasising the other aspect: "the folded value is
  constructed bottom-up" (that is, from tip to handle) because,
  operational aspects aside, the calculation of the final result of the
  fold is driven from the tip. One way to visualise that is looking at
  where the parentheses go when we evaluate a fold with pen and paper: 

  ``` haskell
  foldr (+) 0 [ 1 ,  2 ,  3 ]
  foldr (+) 0 ( 1 : (2 : (3 : [])) )
              ( 1 + (2 + (3 + 0 )) )
              ( 1 + (2 +    3 ) )
              ( 1 +    5 )
                  6
  ```

More compelling illustrations of zygomorphisms are arguably given by
bringing more featureful tree types into play. That can also give us a
fuller picture of what it means to compute from tip to handle. For
instance, here is a binary tree type with values on the nodes.

``` haskell
data BT a = Leaf | Node (BT a) a (BT a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeBaseFunctor ''BT
```

`makeBaseFunctor` is a *recursion-schemes* tool that synthesises, via
Template Haskell, the following base functor type:

``` haskell
GHCi> :info BTF
data BTF a r = LeafF | BranchF r a r 	-- Defined at RS.hs:430:1
instance Traversable (BTF a) -- Defined at RS.hs:430:1
instance Foldable (BTF a) -- Defined at RS.hs:430:1
instance Functor (BTF a) -- Defined at RS.hs:430:1
```

In tree speak, tip-to-handle amounts to leaf-to-root. Let's see how it
plays out in the following function, which checks whether a tree is
perfectly balanced [^perfect-source]: 

[^perfect-source]: This example was adapted from [*Unifying Structured
  Recursion
  Schemes*](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/urs.pdf),
  by Ralf Hinze, Nicolas Wu and Jeremy Gibbons.

``` haskell
perfect :: BT a -> Bool
perfect = zygo depthAlg perfectAlg
    where
    depthAlg = \case
        LeafF -> 0
        NodeF depthL _ depthR -> 1 + max depthL depthR
    perfectAlg = \case
        LeafF -> True
        NodeF (depthL, perfL) _ (depthR, perfR) ->
            perfL && perfR && depthL == depthR
```

In words, a tree is perfectly balanced if it is a leaf, or if it is a
node with two perfectly balanced trees of the same depth (the depths of
the subtrees are provided by the auxiliary algebra, `depthAlg`). The
results flow from leaves to nodes. 

As a bonus example, we can use a similar strategy to find out the
path across a tree that has the maximum sum of elements. 

``` haskell
maxPath :: (Num a, Ord a) => BT a -> [a]
maxPath = zygo sumAlg emitAlg
    where
    sumAlg = \case
        LeafF -> 0
        NodeF sumL a sumR -> a + max sumL sumR
    emitAlg = \case
        LeafF -> []
        NodeF (sumL, pathL) a (sumR, pathR) ->
            a : if sumL >= sumR then pathL else pathR 
```

### Desserts

To get from `mutu` to `zygo`, we, by slipping in `fmap fst`,  cut off one of the links between the
two algebras. What happens if we also cut the other link?

``` haskell
\f g -> cata (f . fmap fst &&& g . fmap snd) :: Recursive t
    => (Base t a -> a)
    -> (Base t b -> b)
    -> (t -> (a, b))
```

This is precisely what we were looking for before I started talking
about `mutu`: two F-algebras tearing down a structure, working in a
single pass and yet independently from each other, to give out two
results. For extra convenience, we can extract the expression for the
combined F-algebra to a separate operator:

``` haskell
(@@@) :: Functor f => (f a -> a) -> (f b -> b) -> (f (a, b) -> (a, b))
f @@@ g = f . fmap fst &&& f . fmap snd
```

And there it is -- now we can run folds side by side:

``` haskell
GHCi> cata (sumAlg @@@ lenAlg) [1..10]
(55,10)
```

``` haskell
averageList :: Fractional a => [a] -> a
averageList = (\(s, n) -> s / fromIntegral n) . cata (sumAlg @@@ lenAlg)
```

I will refer to `(@@@)` as the *banana split* operator, in reference to
the name commonly given in the recursion schemes liteature to the law
which guarantees the correctness of this kind of fold fusion.

Banana split fusion is eminently practical. A fabulous demonstration of
that is [*foldl*](hackage.haskell.org/package/foldl), Gabriel Gonzalez's
popular library for "composable, streaming, and efficient left folds".
Here is what `averageList` would look like if recast with the *foldl*
vocabulary [^foldl-mean]:

[^foldl-mean]: With respect to this specific example, it should be
  mentioned that among many other utility folds, *foldl* offers [a
  purpose-built `mean`
  fold](http://hackage.haskell.org/package/foldl-1.4.5/docs/Control-Foldl.html#v:mean),
  which very likely is what you would actually want to use in practice
  for calculating an arithmetic mean of the elements in a `Foldable`
  container.

``` haskell
GHCi> L.fold ((\x n -> x / fromIntegral n) <$> L.sum <*> L.length) [1..10]
5.5
```

For our current purposes, there are three differences worth mentioning
between the *foldl* machinery and what we have seen so far:

*   Firstly, *foldl* doesn't use a special-purpose banana split
    operator, with `(<*>)` playing that role. That makes sense: if we
    look again at the type of `(@@@)`, we will note it is quite
    `zip`-like, and `zip`-like things often suggest `Applicative` (by
    the way of [the monoidal
    presentation](http://blog.ezyang.com/2012/08/applicative-functors/).
    If we have a closer look at *foldl*'s `Fold` type...

    ``` haskell
    data Fold a b
      -- | @Fold @ @ step @ @ initial @ @ extract@
      = forall x. Fold (x -> a -> x) x (x -> b)
    ```

    ... we will note it is essentially an F-algebra, with a few tweaks:
    The "step" and "initial" fields correspond to the `Cons` and `Nil`
    cases of a list F-algebra. As for the "extract" field, it is not
    just a convenience function for massaging fold results. Rather, it
    makes `Fold a` a covariant functor, something that wouldn't be
    feasible in terms of `x` type variable used by the algebra (note how
    existential quantification is used to keep `x` hidden), and which is
    necessary for an `Applicative` instance.

*   Secondly, since *foldl* deals with strict folds, the implementation
    of `(<*>)` for `Fold a` must use a strict pair type...

    ``` haskell
    data Pair a b = Pair !a !b
    ```

    ... instead of the usual `(a, b)`.

*   Lastly, since the folds are supposed to be strict *left* folds, the
    `fold` function, which is the *foldl* counterpart to `cata`,
    performs the `foldl'`-from-`foldr` trick:

    ``` haskell
    -- | Apply a strict left 'Fold' to a 'Foldable' container
    fold :: Foldable f => Fold a b -> f a -> b
    fold (Fold step begin done) as = F.foldr cons done as begin
      where
        cons a k x = k $! step x a
    ```

None of the three differences mentioned above is fundamental: at heart,
this still is banana split. <!-- lise -->
