---
title: "Divisible and the monoidal quartet"
license: CC-BY-SA
published: 2021-11-06T20:00:00-03:00
gh-issue: 15
---

A recent blog post by Gabriella Gonzalez,
[*Co-Applicative programming style*](
https://www.haskellforall.com/2021/10/co-applicative-programming-style.html),
has motivated some discussion on ergonomic ways to make use of [the
`Divisible` type class](https://hackage.haskell.org/package/contravariant-1.5.5).
The conversation pointed to an interesting rabbit hole, and diving into
it resulted in these notes, in which I attempt to get a clearer view of
picture of the constellation of monoidal functor classes that
`Divisible` belongs to. The gist of it is that "`Divisible` is a
contravariant `Applicative`, and `Decidable` is a contravariant
`Alternative`" is not a full picture of the relationships between the
classes, as there are a few extra connections that aren't obvious to the
naked eye.

<div></div><!--more-->

Besides Gabriella's post, which is an excellent introduction to
`Divisible`, I recommend as background reading Tom Ellis'
[*`Alternatives` convert products to sums*](
http://h2.jaguarpaw.co.uk/posts/alternatives-convert-products-to-sums/),
which conveys the core intuition about monoidal functor classes in an
accessible manner. There is a second Tom Ellis post, [*The Mysterious
Incomposability of `Decidable`*](
http://h2.jaguarpaw.co.uk/posts/mysterious-incomposability-of-decidable/),
that this article will be in constant dialogue with, in particular as a
source of examples. From now on I will refer to it as "the *Decidable*
post". Needless to say, these notes are heavily indebted to those posts,
and so I thank Gabriella and Tom for the spark of inspiration.

For those of you reading with GHCi on the side, here is an import list:

``` haskell
{-# LANGUAGE LambdaCase #-}

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Control.Applicative
import Control.Arrow
import Data.Void
```

## Applicative

As I hinted at the introduction, this post is not solely about
`Divisible`, but more broadly about monoidal functor classes. To start
from familiar ground, as well as set up a reference point, I will first
look at the best known of those classes, `Applicative`. We won't,
however make use of the usual presentation of `Applicative` in terms of
`(<*>)`, as it doesn't generalise to the other classes we're interested
in. Instead, we will switch to the monoidal presentation
[^applicative-monoidal]:

[^applicative-monoidal]: See [the relevant section of the
  *Typeclassopedia*](
  https://wiki.haskell.org/Typeclassopedia#Alternative_formulation) for
  a brief explanation of it.

``` haskell
zipped :: Applicative f => f a -> f b -> f (a, b)
zipped = liftA2 (,)

-- An operator spelling, for convenience.
(&*&) :: Applicative f => f a -> f b -> f (a, b)
(&*&) = zipped
infixr 5 &*&

unit :: Applicative f => f ()
unit = pure ()

-- Laws:

-- unit &*& v ~ v
-- u &*& unit ~ u
-- (u &*& v) &*& w ~ u &*& (v &*& w)

-- These laws characterise the functor as a monoidal one; accordingly,
-- the laws the other monoidal functor classes follow the same pattern.
```

The matter with `(<*>)` (and also `liftA2`) that stops it from being
generalised for our purposes is that it leans heavily on the fact that
**Hask** is a *cartesian closed category*, with pairs as the relevant
product. Without that, the currying and the partial application we rely
on when writing in applicative style become unfeasible.

Before we continue, a few notes about naming and style choices in this
post. Purely for the sake of consistency, I will try to stick to the
`Data.Functor.Contravariant.Divisible` naming conventions for functions
like `zipped` [^fzip-name].

[^fzip-name]: Personally, my favourite prefix name for it is not
  `zipped`, but `fzip`, which I borrowed from [Justus Sagemüller](
  https://stackoverflow.com/a/50703828/2751851).

While keep ourselves away from `(<*>)` and `liftA2`, we can recover, if
not the full flexibility, the power of applicative style with a variant
of `liftA2` that takes an uncurried function:

``` haskell
lizip :: Applicative f => ((a, b) -> c) -> f a -> f b -> f c
lizip f u v = fmap f (zipped u v)
```

(That is admittedly a weird name; all the clashing naming conventions
around this topic has left me with few good options.)

On a closing note for this section, my choice of operator for `zipped`
is motivated by the similiarity with `(&&&)` from `Control.Arrow`:

``` haskell
(&&&) :: Arrow p => p a b -> p a c -> p a (b, c)
```

In particular, `(&*&)` for the function `Applicative` coincides with
`(&&&)` for the function `Arrow`.

Leaning on connections like this one, I will use `Control.Arrow`
combinators liberally, beginning with the definition of the following
two convenience functions that will show up shortly:

``` haskell
dup :: a -> (a, a)
dup = id &&& id

forget :: Either a a -> a
forget = id ||| id
```

## Divisible

As Tom Ellis summarises it at the beginning of the *Decidable* post,
while `Applicative` converts products to products covariantly,
`Divisible` converts products to products contravariantly.  From that
vantage point, I will take `divided`, the counterpart to `zipped`, as
the fundamental combinator of the class:

``` haskell
-- This is the divided operator featured on Gabriella's post, which will
-- soon be available from Data.Functor.Contravariant.Divisible
(>*<) :: Divisible k => k a -> k b -> k (a, b)
(>*<) = divided
infixr 5 >*<

-- Laws:

-- conquered >*< v ~ v
-- u >*< conquered ~ u
-- (u >*< v) >*< w ~ u >*< (v >*< w)
```

Recovering `divide` from `divided` is straightforward, and entirely
analogous to how `lizip` can be obtained from `zipped`:

``` haskell
divide :: Divisible k => (a -> (b, c)) -> k b -> k c -> k a
divide f = contramap f (divided u v)
```

Lessened currying aside, we might say that `divide` plays the role of
`liftA2` in `Divisible`.

It's about time for an example. For that, I will borrow the one from
Gabriella's post:

``` haskell
data Point = Point { x :: Double, y :: Double, z :: Double }
    deriving Show

nonNegative :: Predicate Double
nonNegative = Predicate (0 <=)

-- (>$<) = contramap
nonNegativeOctant :: Predicate Point
nonNegativeOctant =
    adapt >$< nonNegative >*< nonNegative >*< nonNegative
    where
    adapt = x &&& y &&& z
```

The slight distortion to Gabriella's style in using `(&&&)` to write
`adapt` pointfree is meant to emphasise how that deconstructor can be
cleanly assembled out of the component projection functions `x`, `y` and
`z`. Importantly, that holds in general: pair-producing functions `a ->
(b, c)` are isomorphic `(a -> b, a -> c)` pairs of projections. That
gives us a variant of `divide` that takes the projections separately:

``` haskell
tdivide :: Divisible k => (a -> b) -> (a -> c) -> k b -> k c -> k a
tdivide f g u v = divide (f &&& g) u v
```

Besides offering an extra option with respect to ergonomics, `tdivide`
hints at extra structure available from the `Divisible` class. Let's
play with the definitions a little:

``` haskell
tdivide f g u v
divide (f &&& g) u v
contramap (f &&& g) (divided u v)
contramap ((f *** g) . dup) (divided u v)
(contramap dup . contramap (f *** g)) (divided u v)
contramap dup (divided (contramap f u) (contramap g v))
divide dup (contramap f u) (contramap g v)
```

`divide dup`, which duplicates input in order to feed each of its
arguments, is a combinator worthy of a name:

``` haskell
dplus :: Divisible k => k a -> k a -> k a
dplus = divide dup

(>+<) :: Divisible k => k a -> k a -> k a
(>+<) = dplus
infixr 5 >+<
```

So we have:

``` haskell
tdivide f g u v = dplus (contramap f u) (contramap g v)
```

Or, using the operators:

``` haskell
tdivide f g u v = f >$< u >+< g >$< v
```

An alternative to using the projections to set up a deconstructor to be
used with `divide` is to contra-map each projection to its corresponding
divisible value and combine the pieces with `(>+<)`. That is the style
favoured by Tom Ellis [^tom-style], which is why I have added a "t"
prefix to `tdivide` comes from. For instance, Gabriella's example would
be spelled as follows in this style:

``` haskell
nonNegativeOctantT :: Predicate Point
nonNegativeOctantT =
    x >$< nonNegative >+< y >$< nonNegative >+< z >$< nonNegative
```

[^tom-style]: See, for instance, [this Twitter conversation](
  https://twitter.com/tomjaguarpaw/status/1451235378363609096), or the
  `Divisible` example in the *Decidable* post. Note that, though I'm
  using `(>$<)` here for the sake of parallelism, the examples in this
  style arguably look tidier when spelled with `contramap`.

## Alternative

The `(>+<)` combinator defined above is strikingly similar to `(<|>)`
from `Alternative`, down to its implied monoidal nature [^arrowplus]:

``` haskell
(>+<) :: Divisible k => k a -> k a -> k a

(<|>) :: Alternative f => f a -> f a -> f a
```

[^arrowplus]: A proof that `(>+<)` is indeed monoidal is in an appendix
  to this post.

  On a related note, my choice of `>+<` as the `dplus`
  operator is, in part, a pun on [`(<+>)` from `ArrowPlus`](
  https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Arrow.html#t:ArrowPlus).
  `(>+<)` works very much like `(<+>)`, monoidally combining outputs,
  even if there probably isn't a sensible way to actually make the types
  underlying the various `Divisible` functors instances of `ArrowPlus`.

It is surprising that `(>+<)` springs forth in `Divisible` rather than
`Decidable`, which might look like the more obvious candidate to be
`Alternative`'s contravariant counterpart. To understand what is going
on, it helps to look at `Alternative` from the same perspective we have
used here for `Applicative` and `Divisible`. For that, first of all we
need a `divided` counterpart. Let's borrow the definition from
[*Applicatives convert products to sums*](
http://h2.jaguarpaw.co.uk/posts/alternatives-convert-products-to-sums/):

``` haskell
combined :: Alternative f => f a -> f b -> f (Either a b)
combined u v = Left <$> u <|> Right <$> v

(-|-) :: Alternative f => f a -> f b -> f (Either a b)
(-|-) = combined
infixr 5 -|-

-- We also need a suitable identity:
zero :: Alternative f => f Void
zero = empty

-- Laws:

-- zero -|- v ~ v
-- u -|- zero ~ u
-- (u -|- v) -|- w ~ u -|- (v -|- w)
```

(I won't entertain the various controversies about the `Alternative`
laws here, nor any interaction laws involving `Applicative`. Those might
be intersting matters to think about from this vantage point, though.)

A `divide` analogue follows:

``` haskell
combine :: Alternative f => (Either a b -> c) -> f a -> f b -> f c
combine f u v = fmap f (combined u v)
```

Crucially, `Either a b -> c` can be split in a way dual to what we have
seen earlier with `a -> (b, c)`: an `Either`-consuming function amounts
to a pairs of functions, one to deal with each component. That being so,
we can use the alternative style trick done for `Divisible` by
dualising things:

``` haskell
tcombine :: Alternative f => (a -> c) -> (b -> c) -> f a -> f b -> f c
tcombine f g = combine (f ||| g)
```

``` haskell
tcombine f g u v
combine (f ||| g) u v
fmap (f ||| g) (combined u v)
fmap (forget . (f +++ g)) (combined u v)
fmap forget (combined (fmap f u) (fmap g v))
combine forget (fmap f u) (fmap g v)
```

We now go full circle...

``` haskell
aplus :: Alternative f => f a -> f a -> f a
aplus = combine forget
-- (<|>) = aplus
```

... to end up with:

``` haskell
tcombine f g u v = aplus (fmap f u) (fmap g v)

tcombine f g u v = f <$> u <|> g <$> v
```

For instance, here is the `Alternative` composition example from the
*Decidable* post...

``` haskell
alternativeCompose :: [String]
alternativeCompose = show <$> [1,2] <|> reverse <$> ["hello", "world"]
```

... and how it might be rendered using `combine`/`(-|-)`:

``` haskell
alternativeComposeG :: [String]
alternativeComposeG = merge <$> [1,2] -|- ["hello", "world"]
    where
    merge = show ||| reverse
```

There is, therefore, something of a subterranean connection between
`Alternative` and `Divisible`. The function arguments to both `combine`
and `divide`, whose types are dual to each other, can be split in a way
that not only reveals an underlying monoidal operaion, `(<|>)` and
`(>+<)` respectively, but also allows for a certain flexibiliy in
using the class combinators.

## Decidable

Last, but not least, there is `Decidable` to deal with.
`Data.Functor.Contravariant.Divisible` already provides `chosen` as the
`divided` analogue, so let's just supply the and operator variant
[^chosen-operator]:

[^chosen-operator]: Both [*dhall*](
  https://hackage.haskell.org/package/dhall-1.40.1/docs/Dhall-Marshal-Encode.html#v:-62--124--60-)
  and [*co-log-core*](
  https://hackage.haskell.org/package/co-log-core-0.3.0.0/docs/Colog-Core-Action.html#v:-62--124--60-)
  define `(>|<)` as `chosen`-like operators. To my eyes, though, `>|<`
  fits `dplus` better. As a compromise, I opted to not use `>|<` for
  neither of them here.

``` haskell
(|-|) :: Decidable k => k a -> k b -> k (Either a b)
(|-|) = chosen
infixr 5 |-|

-- Laws:

-- lost |-| v ~ v
-- u |-| lost ~ u
-- (u |-| v) |-| w ~ u |-| (v |-| w)
```

`choose` can be recovered from `chosen` in the usual way:

``` haskell
choose :: Decidable k => (a -> Either b c) -> k b -> k c -> k a
choose f u v = contamap f (chosen u v)
```

The `a -> Either b c` argument of `choose`, however, is not amenable to
the function splitting trick we have used for `divide` and `combine`.
`Either`-producing functions are not splittable in that manner, as the
case analysis to decide whether to return `Left` or `Right` cannot be
disentangled. This is ultimately what Tom Ellis' complaint about the
"mysterious incomposability" of `Decidable` is about. Below is a
paraphrased version of the `Decidable` example from the *Decidable*
post:

``` haskell
data Foo = Bar String | Baz Bool | Quux Int
    deriving Show

pString :: Predicate String
pString = Predicate (const False)

pBool :: Predicate Bool
pBool = Predicate id

pInt :: Predicate Int
pInt = Predicate (>= 0)

decidableCompose :: Predicate Foo
decidableCompose = analyse >$< pString |-| pBool |-| pInt
    where
    analyse = \case
        Bar s -> Left s
        Baz b -> Right (Left b)
        Quux n -> Right (Right n)
```

The problem identified in the post is that there is no straightfoward
way around having to write "the explicit unpacking into an `Either`"
performed by `analyse`. In the `Divisible` and `Alternative` examples,
it was possible to avoid tuple or `Either` shuffling by decomposing the
counterparts to `analyse`, but that is not possible here.

In the last few paragraphs, we have mentioned `Divisible`, `Alternative`
and `Decidable`. What about `Applicative`, though? The `Applicative`
example from the *Decidable* post is written in the usual applicative
style:

``` haskell
applicativeCompose :: [[String]]
applicativeCompose =
    f <$> [1, 2] <*> [True, False] <*> ["hello", "world"]
    where
    f = (\a b c -> replicate a (if b then c else "False"))
```

As noted earlier, though, applicative style is a fortunate consequence
of **Hask** being cartesian closed, which makes it possibke to turn `(a,
b) -> c` into `a -> b -> c`. If we leave out `(<*>)` and restrict
ourselves to `(&*&)`, we end up having to deal explicitly with tuples,
which is a dual version of the `Decidable` issue:

``` haskell
monoidalCompose :: [[String]]
monoidalCompose =
    consume <$> [1, 2] &*& [True, False] &*& ["hello", "world"]
    where
    consume (a, (b, c)) = replicate a (if b then c else "False")
```

Just like `a -> Either b c` functions, `(a, b) -> c` functions cannot be
decomposed: the `c` value can be produced by using the `a` and `b`
components in arbitrary ways, and there is no way to disentangle that.

`Decidable`, then, relates to `Applicative` in an analogous way to how
`Divisible` does to `Alternative`. There are a few other similiarities
between them that are worth pointing out:

- Neither `Applicative` nor `Decidable` offer a monoidal `f a -> f a ->
  f a` operation like the ones of `Alternative` and `Decidable`. A
  related observation is that, for example, `Op`'s `Decidable` instance
  inherits a `Monoid` constraint from `Divisible` but doesn't actually
  use it in the method implementations.

- `choose Left` and `choose Right` can be used to combine consumers so
  that one of them doesn't actually receive input. That is analogous to
  how `(<*) = lizip fst` and `(*>) = lizip snd` combine applicative
  values while discarding the output from one of them.

- Dually to how `zipped`/`&*&` for the function functor is `(&&&)`,
  `chosen` for decidables such as `Op` and `Predicate` amounts to
  `(|||)`. My choice of `|-|` as the corresponding operator hints at
  that.

## In summary

To wrap things up, here is a visual summary of the parallels between the
four classes:

![*Diagram of the four monoidal functor classes under consideration, with
`Applicative` and `Decidable` in one diagonal, and `Alternative` and
`Divisible` in the other.*](/images/posts/monoidal-quartet-diagram.png)

To my eyes, the main takeaway of our around this diagram has to do with
its diagonals. Thanks to a peculiar kind of duality, classes in opposite
corners are similar in quite a few ways. In particular, the orange
diagonal classes, `Alternative` and `Divisible`, have monoidal
operations of `f a -> f a -> f a` signature that arise out of their
monoidal functor structure.

## Appendix

### `(>+<)` is a monoidal operation

If we are to show that `(>+<)` is a monoidal operation, first of all we
need an identity for it. `conquer :: f a` sounds like a reasonable
candidate. It can be expressed in terms of `conquered`, the unit for
`divided`, as follows:

``` haskell
-- conquer discards input.
conquer = const () >$< conquered
```

The identity laws do come out all right:

``` haskell
conquer >+< v = v  -- Goal
conquer >+< v  -- LHS
dup >$< (conquer >*< v)
dup >$< ((const () >$< conquered) >*< v)
dup >$< (first (const ()) >$< (conquered >*< v))
first (const ()) . dup >$< (conquered >*< v)
-- conquered >*< v ~ v
first (const ()) . dup >$< (snd >$< v)
snd . first (const ()) . dup >$< v
v  -- LHS = RHS

u >+< conquer = u  -- Goal
u >+< conquer  -- LHS
dup >$< (u >*< discard)
dup >$< (u >*< (const () >$< conquered))
dup >$< (second (const ()) >$< (u >*< conquered))
second (const ()) . dup >$< (u >*< conquered)
-- u >*< conquered ~ u
second (const ()) . dup >$< (fst >$< u)
fst . second (const ()) . dup >$< u
u  -- LHS = RHS
```

And so does the associativity one:

``` haskell
(u >+< v) >+< w = u >+< (v >+< w)  -- Goal
(u >+< v) >+< w  -- LHS
dup >$< ((dup >$< (u >*< v)) >*< w)
dup >$< (first dup >$< ((u >*< v) >*< w))
first dup . dup >$< ((u >*< v) >*< w)
u >+< (v >+< w)  -- RHS
dup >$< (u >*< (dup >$< (v >*< w)))
dup >$< (second dup >$< (u >*< (v >*< w)))
second dup . dup >$< (u >*< (v >*< w))
-- (u >*< v) >*< w ~ u >*< (v >*< w)
-- assoc ((x, y), z) = (x, (y, z))
second dup . dup >$< (assoc >$< ((u >*< v) >*< w))
assoc . second dup . dup >$< ((u >*< v) >*< w)
first dup . dup >$< ((u >*< v) >*< w)  -- LHS = RHS
```
