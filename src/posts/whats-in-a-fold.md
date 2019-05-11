---
title: "What's in a Fold: The Basic Catamorphism in recursion-schemes"
published: 2017-03-10T06:00:00Z
license: CC-BY-SA
reddit: 5ykjyl
gh-issue: 12
---

This article is meant as an accessible introduction to the most basic
recursion scheme, the catamorphism. It won't engage in deep dives into
theory, or survey practical motives for using recursion schemes --
that will be covered by the further reading suggestions at the end.
Rather, its main goal is simply offering a concrete presentation of
how folds can be generalised. This presentation will be done in terms
of the types and combinators used by
the
[*recursion-schemes*](https://hackage.haskell.org/package/recursion-schemes-5.0.1) library,
so that the article doubles as an introduction to some of its key
conventions.

<div></div><!--more-->

foldr
-----

The primeval example of a fold in Haskell is the right fold of a list.

``` haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

One way of picturing what the first two arguments of `foldr` are for
is seeing them as replacements for the list constructors: the `b`
argument is an initial value corresponding to the empty list, while
the binary function incorporates each element prepended through `(:)`
into the result of the fold.

``` haskell
data [a] = [] | a : [a]

foldr (+) 0 [ 1 ,  2 ,  3 ]
foldr (+) 0 ( 1 : (2 : (3 : [])) )
            ( 1 + (2 + (3 + 0 )) )
```

By applying this strategy to other data structures, we can get
analogous folds for them.

``` haskell
-- This is foldr; I have flipped the arguments for cosmetic reasons.
data [a] = [] | (:) a [a]
foldList :: b -> (a -> b -> b) -> [a] -> b

-- Does this one look familiar?
data Maybe a = Nothing | Just a
foldMaybe :: b -> (a -> b) -> Maybe a -> b

-- This is not the definition in Data.List.NonEmpty; the differences
-- between them, however, are superficial.
data NEList :: NEList a (Maybe (NEList a))
foldNEList :: (a -> Maybe b -> b) -> NEList a -> b

-- A binary tree like the one in Diagrams.TwoD.Layout.Tree (and in
-- many other places).
data BTree a = Empty | BNode a (BTree a) (BTree a)
foldBTree :: b -> (a -> b -> b -> b) -> BTree a -> b
```

It would make sense to capture this pattern into an abstraction. At
first glance, however, it is not obvious how to do so. Though we know
intuitively what the folds above have in common, their type signatures
have lots of superficial differences between them. Our immediate goal,
then, will be simplifying things by getting rid of these
differences.[^foldable]

[^foldable]: By the way, it is worth emphasising that the `Foldable`
    class from *base* is not the abstraction we are looking for. One
    way of seeing why is placing the signature of `foldBTree` side by
    side with the one of `Foldable.foldr`.

ListF
-----

We will sketch the simplification using the tangible and familiar
example of lists. Let's return to the type of `foldr`.

``` haskell
(a -> b -> b) -> b -> [a] -> b
```

With the cosmetic flip I had applied previously, it becomes:

``` haskell
b -> (a -> b -> b) -> [a] -> b
```

The annoying irregularities among the types of the folds in the
previous section had to do with the number of arguments other than the
data structure (one per constructor) and the types of said arguments
(dependent on the shape of each constructor). Though we cannot
entirely suppress these differences, we have a few tricks that make it
possible to disguise them rather well. The number of extra arguments,
for instance, can be always be reduced to just one with sufficient
uncurrying:

``` haskell
(b, a -> b -> b) -> [a] -> b
```

The first argument is now a pair. We continue by making its two halves
more like each other by converting them into unary functions: the
first component acquires a dummy `()` argument, while the second one
gets some more uncurrying:

``` haskell
(() -> b, (a, b) -> b) -> [a] -> b
```

We now have a pair of unary functions with result type `b`. A pair of
functions with the same result type, however, is equivalent to a
single function from `Either` one of the argument types (if you are
sceptical about that, you might want to work out the isomorphism --
that is, the pair of conversion functions -- that witnesses this
fact):

``` haskell
(Either () (a, b) -> b) -> [a] -> b
```

At this point, the only extra argument of the fold is an unary
function with result type `b`. We have condensed the peculiarities of
the original arguments at a single place (the argument of said
function), which makes the overall shape of the signature a lot
simpler. Since it can be awkward to work with anonymous nestings of
`Either` and pairs, we will replace `Either () (a, b)` with an
equivalent type equipped with suggestive names:

``` haskell
data ListF a b =  Nil | Cons a b
--            Left () | Right (a,b)
```

That leaves us with:

``` haskell
(ListF a b -> b) -> [a] -> b
```

The most important fact about `ListF` is that it mirrors the shape of
the list type except for one crucial difference...

``` haskell
data []    a   = []  | (:)  a [a]
data ListF a b = Nil | Cons a b
```

... namely, *it is not recursive*. An `[a]` value built with `(:)` has
another `[a]` in itself, but a `ListF a b` built with `Cons` does not
contain another `ListF a b`. To put it in another way, `ListF` is the
outcome of taking away the recursive nesting in the list data type and
filling the resulting hole with a placeholder type, the `b` in our
signatures, that corresponds to the result of the fold. This strategy
can be used to obtain a `ListF` analogue for any other data
structure. You might, for instance, try it with the `BTree a` type shown
in the first section.

cata
----

We have just learned that the list `foldr` can be expressed using this
signature:

``` haskell
(ListF a b -> b) -> [a] -> b
```

We might figure out a `foldr` implementation with this signature in a
mechanical way, by throwing all of the tricks from the previous
section at `Data.List.foldr` until we squeeze out something with the
right type. It is far more illuminating, however, to start from
scratch. If we go down that route, the first question that arises is
how to apply a `ListF a b -> b` function to an `[a]`. It is clear that
the list must somehow be converted to a `ListF a b`, so that the
function can be applied to it.

``` haskell
foldList :: (ListF a b -> b) -> [a] -> b
foldList f = f . something
-- foldList f xs = f (something xs)
-- something :: [a] -> ListF a b
```

We can get part of the way there by recalling how `ListF` mirrors the
shape of the list type. That being so, going from `[a]` to `ListF a
[a]` is just a question of matching the corresponding
constructors.[^lambdacase]

[^lambdacase]: In what follows, I will use the `LambdaCase` extension
    liberally, so that I have fewer boring variable names to make
    up. If you haven't seen it yet, all you need to know is that...

    ``` haskell
    \case
        [] -> foo
        x:xs -> bar
    ```

    ... is the same as:

    ``` haskell
    \list -> case list of
        [] -> foo
        x:xs -> bar
    ```

``` haskell
project :: [a] -> ListF a [a]
project = \case
    [] -> Nil
    x:xs -> Cons x xs

foldList :: (ListF a b -> b) -> [a] -> b
foldList f = f . something . project
-- something :: ListF a [a] -> ListF a b
```

`project` witnesses the simple fact that, given that `ListF a b` is
the `[a]` except with a `b` placeholder in the tail position, where
there would be a nested `[a]`, if we plug the placeholder with `[a]`
we get something equivalent to the `[a]` list type we began with.

We now need to go from `ListF a [a]` to `ListF a b`; in other words,
we have to change the `[a]` inside `ListF` into a `b`. And sure
enough, we do have a function from `[a]` to `b`...

``` haskell
foldList :: (ListF a b -> b) -> ([a] -> b)
f :: ListF a b -> b
foldList f :: [a] -> b
```

... the fold itself! To conveniently reach inside `ListF a b`, we set
up a `Functor` instance:

``` haskell
instance Functor (ListF a) where
    fmap f = \case
        Nil -> Nil
        Cons x y -> Cons x (f y)

foldList :: (ListF a b -> b) -> [a] -> b
foldList f = f . fmap (foldList f) . project
```

And there it is, the list fold. First, `project` exposes the list (or,
more precisely, its first constructor) to our machinery; then, the
tail of the list (if there is one -- what happens if there isn't?) is
recursively folded through the `Functor` instance of `ListF`; finally,
the overall result is obtained by applying `f` to the resulting `ListF
a b`.

``` haskell
-- A simple demonstration of foldList in action.
f :: Num a => ListF a a -> a
f = \case { Nil -> 0; Cons x y -> x + y }

foldList f [1, 2, 3]
-- Let's try and evaluate this by hand.
foldList f (1 : 2 : 3 : [])
f . fmap (foldList f) . project $ (1 : 2 : 3 : [])
f . fmap (foldList f) $ Cons 1 (2 : 3 : [])
f $ Cons 1 (foldList f (2 : 3 : []))
f $ Cons 1 (f . fmap (foldList f) $ project (2 : 3 : []))
f $ Cons 1 (f . fmap (foldList f) $ Cons 2 (3 : []))
f $ Cons 1 (f $ Cons 2 (foldList f (3 : [])))
f $ Cons 1 (f $ Cons 2 (f . fmap (foldList f) . project $ (3 : [])))
f $ Cons 1 (f $ Cons 2 (f . fmap (foldList f) $ Cons 3 []))
f $ Cons 1 (f $ Cons 2 (f $ Cons 3 (foldList f [])))
f $ Cons 1 (f $ Cons 2 (f $ Cons 3 (f . fmap (foldList f) . project $ [])))
f $ Cons 1 (f $ Cons 2 (f $ Cons 3 (f . fmap (foldList f) $ Nil)))
f $ Cons 1 (f $ Cons 2 (f $ Cons 3 (f $ Nil)))
f $ Cons 1 (f $ Cons 2 (f $ Cons 3 0))
f $ Cons 1 (f $ Cons 2 3)
f $ Cons 1 5
6
```

One interesting thing about our definition of `foldList` is that all
the list-specific details are tucked within the implementations of
`project`, `fmap` for `ListF` and `f` (whatever it is). That being so,
if we look only at the implementation and not at the signature, we
find no outward signs of anything related to lists. No outward signs,
that is, except for the name we gave the function. That's easy enough
to solve, though: it is just a question of inventing a new one:

``` haskell
cata f = f . fmap (cata f) . project
```

`cata` is short for *catamorphism*, the fancy name given to ordinary
folds in the relevant theory. There is a function called `cata` in
*recursion-schemes*.
[Its implementation](https://hackage.haskell.org/package/recursion-schemes-5.0.1/docs/Data-Functor-Foldable.html#t:Recursive)...

``` haskell
cata f = c where c = f . fmap c . project
```

... is the same as ours, almost down to the last character. Its type
signature, however, is much more general:

``` haskell
cata :: Recursive t => (Base t b -> b) -> t -> b
```

It involves, in no particular order:

- `b`, the type of the result of the fold;

- `t`, the type of the data structure being folded. In our example,
  `t` would be `[a]`; or, as GHC would put it, `t ~ [a]`.

- `Base`, a type family that generalises what we did with `[a]` and
  `ListF` by assigning *base functors* to data types. We can read
  `Base t` as "the base functor of `t`"; in our example, we have `Base
  [a] ~ ListF a`.

- `Recursive`, a type class whose minimal definition consists of
  `project`, with the type of `project` now being `t -> Base t t`.

The base functor is supposed to be a `Functor`, so that we can use
`fmap` on it. That is enforced through a `Functor (Base t)` constraint
in the definition of the `Recursive` class. Note, however, that there
is no such restriction on `t` itself: it doesn't need to be a
polymorphic type, or even to involve a type constructor.

In summary, once we managed to concentrate the surface complexity in
the signature of `foldr` at a single place, the `ListF a b -> b`
function, an opportunity to generalise it revealed itself.
Incidentally, that function, and more generally any `Base t b -> b`
function that can be given to `cata`, is referred to as an
*algebra*. In this context, the term "algebra" is meant in a precise
technical sense; still, we can motivate it with a legitimate recourse
to intuition. In basic school algebra, we use certain rules to get
simpler expressions out of more complicated ones, such as $ax + bx =
(a + b)x$. Similarly, a `Base t b -> b` algebra boils down to a set
of rules that tell us what to do to get a `b` result out of the `Base
t b` we are given at each step of the fold.

Fix
---

The `cata` function we ended up with in the previous section...

``` haskell
cata :: Recursive t => (Base t b -> b) -> t -> b
cata f = c where c = f . fmap c . project
```

... is perfectly good for practical purposes: it allows us to fold
anything that we can give a `Base` functor and a corresponding
`project`. Not only that, the implementation of `cata` is very
elegant. And yet, a second look at its signature suggests that there
might be an even simpler way of expressing `cata`. The signature uses
both `t` and `Base t b`. As we have seen for the `ListF` example,
these two types are very similar (their shapes match except for
recursiveness), and so using both in the same signature amounts to
encoding the same information in two different ways -- perhaps
unnecessarily so.

In the implementation of `cata`, it is specifically `project` that
links `t` and `Base t b`, as it translates the constructors from one
type to the other.

``` haskell
project (1 : 2 : 3 : [])
Cons 1 (2 : 3 : [])
```

Now, let's look at what happens once we repeatedly expand the
definition of `cata`:

``` haskell
c = cata f
p = project
                                c
                         f . fmap c . p
               f . fmap (f . fmap c . p) . p
     f . fmap (f . fmap (f . fmap c . p) . p) . p
f . fmap (   .   .   .   f . fmap c . p   .   .   .   ) . p
```

This continues indefinitely. The fold terminates when, at some point,
`fmap c` does nothing (in the case of `ListF`, that happens when we
get to a `Nil`). Note, however, that even at that point we can carry
on expanding the definition, merrily introducing do-nothing operations
for as long as we want.

At the right side of the expanded expression, we have a chain of
increasingly deep `fmap`-ped applications of
`project`:[^2nd-functor-law]

[^2nd-functor-law]: While that is clear to the naked eye, it can be
    shown more rigorously by applying the second functor law, that is:

    ``` haskell
    fmap (g . f) = fmap g . fmap f
    ```

``` haskell
.   .   .   fmap (fmap project) . fmap project . project
```

If we could factor that out into a separate function, it would change
a `t` data structure into something equivalent to it, but built with
the `Base t` constructors:

``` haskell
GHCi> :{
GHCi| fmap (fmap (fmap project))
GHCi|     . fmap (fmap project) . fmap project . project
GHCi|     $ 1 : 2 : 3 : []
GHCi| :}
Cons 1 (Cons 2 (Cons 3 Nil))
```

We would then be able to regard this conversion as a preliminary,
relatively uninteresting step that precedes the application of a
slimmed down `cata`, that doesn't use neither `project` nor the `t`
type.[^bind]

[^bind]: This is in some ways similar to how `(>>= f) = join . fmap f`
    can be read as a factoring of `(>>=)` into a preliminary step
    (`fmap f`) followed by the quintessential monadic operation
    (`join`).

``` haskell
cata f = leanCata f . omniProject
```

Defining `omniProject` seems simple once we notice the
self-similarity in the chain of `project`:

``` haskell
omniProject = .   .   .   fmap (fmap project) . fmap project . project
omniProject = fmap (fmap (   .   .   .   project) . project) . project
omniProject = fmap omniProject . project
```

Guess what happens next:

``` haskell
GHCi> omniProject = fmap omniProject . project

<interactive>:502:16: error:
    • Occurs check: cannot construct the infinite type: b ~ Base t b
      Expected type: t -> b
        Actual type: t -> Base t b
    • In the expression: fmap omniProject . project
      In an equation for ‘omniProject’:
          omniProject = fmap omniProject . project
    • Relevant bindings include
        omniProject :: t -> b (bound at <interactive>:502:1)
```

GHCi complains about an "infinite type", and that is entirely
appropriate. Every `fmap`-ped `project` changes the type of the result
by introducing a new layer of `Base t`. That being so, the type of
`omniProject` would be...

``` haskell
omniProject :: Recursive t => t -> Base t (Base t (Base t (  .  .  .
```

... which is clearly a problem, as we don't have a type that encodes an
infinite nesting of type constructors. There is a simple way of solving
that, though: we *make up* the type we want!

``` haskell
newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f
```

If we read `Fix f` as "infinite nesting of `f`", the right-hand side
of the `newtype` definition just above reads "an infinite nesting of
`f` contains an `f` of infinite nestings of `f`", which is an entirely
reasonable encoding of such a thing.[^fixed-point]

[^fixed-point]: The name `Fix` comes from "fixed point", the
    mathematical term used to describe a value which is left unchanged
    by some function. In this case, if we have an infinite nesting of
    the `f` type constructor, it doesn't make any difference if we
    apply `f` to it one more time.

All we need to make our tentative definition of `omniProject` legal
Haskell is wrapping the whole thing in a `Fix`. The recursive
`fmap`-ping will ensure `Fix` is applied at all levels:

``` haskell
omniProject :: Recursive t => t -> Fix (Base t)
omniProject = Fix . fmap omniProject . project
```

Another glance at the definition of `cata` shows that this is just
`cata` using `Fix` as the algebra:

``` haskell
omniProject = cata Fix
```

That being so, `cata Fix` will change anything with a `Recursive`
instance into its `Fix`-wearing form:

``` haskell
GHCi> cata Fix [0..9]
Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix (Cons 4 (
Fix (Cons 5 (Fix (Cons 6 (Fix (Cons 7 (Fix (Cons 8 (Fix (Cons 9 (
Fix Nil))))))))))))))))))))
```

Defining a `Fix`-style structure from scratch, without relying on a
`Recursive` instance, is just a question of introducing `Fix` in the
appropriate places. For extra convenience, you might want to define
"smart constructors" like these two:[^smart-cons]

[^smart-cons]: As suggested by Jared Tobin's *Practical Recursion
    Schemes* article, which is in the further reading list at the end
    of this post.

``` haskell
nil :: Fix (ListF a)
nil = Fix Nil

cons :: a -> Fix (ListF a) -> Fix (ListF a)
cons x xs = Fix (Cons x xs)
```

``` haskell
GHCi> 1 `cons` (2 `cons` (3 `cons` nil))
Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil))))))
```

Before we jumped into this `Fix` rabbit hole, we were trying to find
a `leanCata` function such that:

``` haskell
cata f = leanCata f . omniProject
```

We can now easily define `leanCata` by mirroring what we have done for
`omniProject`: first, we get rid of the `Fix` wrapper; then, we fill
in the other half of the definition of `cata` that we left behind when
we extracted `omniProject` -- that is, the repeated application of
`f`:

``` haskell
leanCata f = f . fmap (leanCata f) . unfix
```

(It is possible to prove that this *must* be the definition of
`leanCata` using the definitions of `cata` and `omniProject` and the
`cata f = leanCata f . omniProject` specification. You might want to
work it out yourself; alternatively, you can find the derivation in an
appendix at the end of this article.)


What should be the type of `leanCata`? `unfix` calls for a `Fix f`,
and `fmap` demands this `f` to be a `Functor`. As the definition
doesn't use `cata` or `project`, there is no need to involve `Base` or
`Recursive`. That being so, we get:

``` haskell
leanCata :: Functor f => (f b -> b) -> Fix f -> b
leanCata f = f . fmap (leanCata f) . unfix
```

This is how you will usually see `cata` being defined in other texts
about the subject.[^names]

[^names]: The names in said texts tend to be different, though. Common
    picks include `μ` for the `Fix` type constructor, `In` for the
    `Fix` value constructor, `out` for `unfix`, and `⦇f⦈` for
    `leanCata f` (using the famed banana brackets).

Similarly to what we have seen for `omniProject`, the implementation of
`leanCata` looks a lot like the `cata` we began with, except that it
has `unfix` where `project` used to be. And sure enough,
*recursion-schemes* defines...

``` haskell
type instance Base (Fix f) = f

instance Functor f => Recursive (Fix f) where
  project (Fix a) = a
```

... so that its `cata` also works as `leanCata`:

``` haskell
GHCi> foo = 1 `cons` (2 `cons` (3 `cons` nil))
GHCi> foo
Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil))))))
GHCi> cata (\case {Nil -> 1; Cons x y -> x * y}) foo
6
```

In the end, we did manage to get a tidier `cata`. Crucially, we now
also have a clear picture of folding, the fundamental way of consuming
a data structure recursively. On the one hand, any fold can be
expressed in terms of an algebra for the base functor of the structure
being folded by the means of a simple function, `cata`. On the other
hand, the relationship between data structures and their base functors
is made precise through `Fix`, which introduces recursion into
functors in a way that captures the essence of recursiveness of data
types.

To wrap things up, here a few more questions for you to ponder:

- Does the data structure that we get by using `Maybe` as a base
  functor correspond to anything familiar? Use `cata` to write a fold
  that does something interesting with it.

- What could possibly be the base functor of a non-recursive data structure?

- Find *two* base functors that give rise to non-empty lists. One of
  them corresponds directly to the `NEList` definition given at the
  beginning of this article.

- As we have discussed, `omniProject`/`cata Fix` can be used to
  losslessly convert a data structure to the corresponding
  `Fix`-encoded form. Write the other half of the isomorphism for
  lists; that is, the function that changes a `Fix (ListF a)` back
  into an `[a]`.

Closing remarks
---------------

When it comes to recursion schemes, there is a lot more to play with
than just the fundamental catamorphism that we discussed here. In
particular, *recursion-schemes* offers all sorts of specialised folds
(and *un*folds), often with richly decorated type signatures meant to
express more directly some particular kind of recursive (or
*co*recursive) algorithm. But that's a story for another time. For
now, I will just make a final observation about unfolds.

Intuitively, an unfold is the opposite of a fold -- while a fold
consumes a data structure to produce a result, an unfold generates a
data structure from a seed. In recursion schemes parlance, the
intuition is made precise by the notion of *anamorphism*, a
counterpart (technically, a *dual*) to the catamorphism. Still, if we
have a look at `unfoldr` in `Data.List`, the exact manner in which it
is opposite to `foldr` is not immediately obvious from its signature.

``` haskell
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```

One way of clarifying that is considering the first argument of
`unfoldr` from the same perspective that we used to uncover `ListF`
early in this article.

Further reading
---------------

- [*Understanding F-Algebras*](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras),
  by Bartosz Milewski, covers similar ground to this article from an
  explicitly categorical perspective. A good follow-up read for
  sharpening your picture of the key concepts we have discussed here.

- [*An Introduction to Recursion Schemes*](http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/),
  by Patrick Thompson, is the first in a series of three articles that
  present some common recursion schemes at a gentle pace. You will
  note that examples involving syntax trees and simplifying
  expressions are a running theme across these articles. That is in
  line with what we said about the word "algebra" at the end of the
  section about `cata`.

- [*Practical Recursion Schemes*](https://jtobin.io/practical-recursion-schemes),
  by Jared Tobin, offers a faster-paced demonstration of basic
  recursion schemes. Unlike the other articles in this list, it
  explores the machinery of the *recursion-schemes* library that we
  have dealt with here.

- [*Functional Programming With Bananas, Lenses, Envelopes and Barbed Wire](http://maartenfokkinga.github.io/utwente/#detail_0000003415),
  by Erik Meijer, Maarten Fokkinga and Ross Paterson, is a classic
  paper about recursion schemes, the one which popularised concepts
  such as catamorphism and anamorphism. If you plan to go through it,
  you may
  find
  [this key to its notation](http://blog.ezyang.com/2010/05/bananas-lenses-envelopes-and-barbed-wire-a-translation-guide/) by
  Edward Z. Yang useful.


Appendix: leanCata
------------------

This is the derivation mentioned in the middle of the section about
`Fix`. We begin from our specification for `leanCata`:

``` haskell
cata f = leanCata f . omniProject
```

Take the left-hand side and substitute the definition of `cata`:

``` haskell
f . fmap (cata f) . project
```

Substitute the right-hand side of the `leanCata` specification:

``` haskell
f . fmap (leanCata f . omniProject) . project
```

By the second functor law:

``` haskell
f . fmap (leanCata f) . fmap omniProject . project
```

`unfix . Fix = id`, so we can slip it in like this:

``` haskell
f . fmap (leanCata f) . unfix . Fix . fmap omniProject . project
```

Substituting the definition of `omniProject`:

``` haskell
f . fmap (leanCata f) . unfix . omniProject
```

Substituting this back into the specification:

``` haskell
f . fmap (leanCata f) . unfix . omniProject = leanCata f . omniProject
```

Assuming a sensible `Recursive` and `Base` instances for `t`, `t` and
`Fix (Base t)` should be isomorphic (that is, losslessly
interconvertible) types, with `omniProject` performing one of the two
relevant conversions.  As a consequence, `omniProject` is surjective
(that is, it is possible to obtain every `Fix (Base t)` value through
it). That being so, we can "cancel out" the `omniProject`s at the
right end of both sides of the equation above. The definition of
`leanCata` follows immediately.

``` haskell
f . fmap (leanCata f) . unfix = leanCata f
```
