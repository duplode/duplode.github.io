---
title: Lenses You Can Make at Home
published: 2014-04-26T12:00:00Z
license: CC-BY-SA
reddit: 241aec
gh-issue: 1
---

The most striking traits of the `lens` library are its astonishing
breadth and generality. And yet, the whole edifice is built around van
Laarhoven lenses, which are a simple and elegant concept. In this
hands-on exposition, I will show how the `Lens` type can be understood
without prerequisites other than a passing acquaintance with Haskell
functors.  Encouraging sound intuition in an accessible manner can go a
long way towards making `lens` and lenses less intimidating.

<div></div><!--more-->

Humble Beginnings
-----------------

Dramatis personæ:

> import Data.Functor.Identity (Identity(..))
> import Control.Applicative (Const(..))

I will define a toy data type so that we have something concrete to play
with, as well as a starting point for working out generalisations.

> data Foo = Foo { bar :: Int } deriving (Show)

The record definition gets us a function for accessing the `bar` field.

< GHCi> :t bar
< bar :: Foo -> Int

As for the setter, we have to define it ourselves, unless we feel like
mucking around with record update syntax.

> setBar :: Foo -> Int -> Foo
> setBar x y = x { bar = y }

Armed with a proper getter and setter pair, we can easily flip the sign
of the `bar` inside a `Foo`.

< GHCi> let x = Foo 3
< GHCi> setBar x (negate $ bar x)
< Foo {bar = -3}

We can make it even easier by defining a modifier function for `bar`.

> modifyBar :: (Int -> Int) -> Foo -> Foo
> modifyBar k x = setBar x . k . bar $ x

< GHCi> modifyBar negate x
< Foo {bar = -3}

`setBar` can be recovered from `modifyBar` by using `const` to discard
the original value and put the new one in its place.

< const y = \_ -> y

> setBar' :: Foo -> Int -> Foo
> setBar' x y = modifyBar (const y) x

If our data type had several fields, defining a modifier for each of
them would amount to quite a lot of boilerplate. We could minimise it
by, starting from our `modifyBar` definition, abstracting from the
specific getter and setter for `bar`. Here, things begin to pick up
steam. I will define a general `modify` function, which, given an
appropriate getter-setter pair, can deal with any field of any data
type.

> modify :: (s -> a) -> (s -> a -> s) -> (a -> a) -> s -> s
> modify getter setter k x = setter x . k . getter $ x

It is trivial to recover `modifyBar`; when we do so, `s` becomes `Foo`
and `a` becomes `Int`.

> modifyBar' :: (Int -> Int) -> Foo -> Foo
> modifyBar' = modify bar setBar

Functors Galore
---------------

The next step of generalisation is the one leap of faith I will ask of
you in the way towards lenses. I will introduce a variant of `modify`
in which the modifying function, rather than being a plain `a -> a`
function, returns a functorial value. Defining it only takes an extra
`fmap`.

> modifyF :: Functor f => (s -> a) -> (s -> a -> s)
>                      -> (a -> f a) -> s -> f s
> modifyF getter setter k x = fmap (setter x) . k . getter $ x

And here is its specialisation for `bar`.

> modifyBarF :: Functor f => (Int -> f Int) -> Foo -> f Foo
> modifyBarF = modifyF bar setBar

Why on Earth we would want to do that? For one, it allows for some nifty
tricks depending on the functor we choose. Let's try it with lists.
Specialising the `modifyF` type would give:

< modifyL :: (s -> a) -> (s -> a -> s) -> (a -> [a]) -> s -> [s]

Providing the getter and the setter would result in a `(a -> [a]) -> s
-> [s]` function. Can you guess what it would do?

< GHCi> modifyBarF (\y -> [0..y]) x
< [Foo {bar = 0},Foo {bar = 1},Foo {bar = 2},Foo {bar = 3}]

As the types suggest, we get a function which modifies the field in
multiple ways and collects the results.

I claimed that moving from `modify` to `modifyF` was a generalisation.
Indeed, we can recover `modify` by bringing `Identity`, the dummy
functor, into play.

< newtype Identity a = Identity { runIdentity :: a }
<
< instance Functor Identity where
<     fmap f (Identity x) = Identity (f x)

< modifyI :: (s -> a) -> (s -> a -> s) -> (a -> Identity a) -> s -> Identity s

> modify' :: (s -> a) -> (s -> a -> s) -> (a -> a) -> s -> s
> modify' getter setter k =
>     runIdentity . modifyF getter setter (Identity . k)

We wrap the field value with `Identity` value after applying `k` and
unwrap the final result after applying the setter. Since `Identity` does
nothing interesting to the wrapped values, the overall result boils down
to our original `modify`. If you have found this definition confusing, I
suggest that you, as an exercise, rewrite it in pointful style and
substitute the definition of `modifyF`.

We managed to get `modify` back with little trouble, which is rather
interesting. However, what is truly surprising is that we can
reconstruct not only the modifier but also the getter! To pull that
off, we will use `Const`, which is a very quaint functor.

< newtype Const a b = Const { getConst :: a }
<
< instance Functor (Const a) where
<     fmap _ (Const y) = Const y

< modifyC :: (s -> a) -> (s -> a -> s) -> (a -> Const r a) -> s -> Const r s

If functors were really containers, `Const` would be an Acme product. A
`Const a b` value does not contain anything of type `b`; what it does
contain is an `a` value that we cannot even modify, given that `fmap f`
is `id` regardless of what `f` is. As a consequence, if, given a field
of type `a`, we pick `Const a` as the functor to use with `modifyF` and
use the modifying function to wrap the field value with `Const`, then
the value will not be affected by the setter, and we will be able to
recover it later. That suffices for recovering the getter.

> get :: (s -> a) -> (s -> a -> s) -> s -> a
> get getter setter = getConst . modifyF getter setter Const
>
> getBar :: Foo -> Int
> getBar = get bar setBar

The Grand Unification
---------------------

Given a getter and a setter, `modifyF` gets us a corresponding
functorial modifier. From it, by choosing the appropriate functors, we
can recover the getter and a plain modifier; the latter, in turn, allows
us to recover the setter.  We can highlight the correspondence by
redefining once more the recovered getters and modifiers, this time in
terms of the functorial modifier.

< modifyF :: Functor f => (s -> a) -> (s -> a -> s)
<                      -> ((a -> f a) -> s -> f s)

> modify'' :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
> modify'' modifier k = runIdentity . modifier (Identity . k)
>
> modifyBar'' :: (Int -> Int) -> Foo -> Foo
> modifyBar'' = modify'' modifyBarF
>
> set :: ((a -> Identity a) -> s -> Identity s) -> s -> a -> s
> set modifier x y = modify'' modifier (const y) x
>
> setBar'' :: Foo -> Int -> Foo
> setBar'' = set modifyBarF
>
> get' :: ((a -> Const a a) -> s -> Const a s) -> (s -> a)
> get' modifier = getConst . modifier Const
>
> getBar' :: Foo -> Int
> getBar' = get' modifyBarF

The bottom line is that given `modifyBarF` we can get by without
`modifyBar`, `setBar` and `bar`, as `modify''`, `set` and `get'` allow
us to reconstruct them whenever necessary. While our first version of
`get` was, in effect, just a specialised `const` with a wacky
implementation, `get'` is genuinely useful because it cuts the number of
separate field manipulation functions we have to deal with by a third.

Expanding Horizons
------------------

Even after all of the work so far we can still generalise further!
Let's have a second look at `modifyF`.

< modifyF :: Functor f => (s -> a) -> (s -> a -> s)
<                      -> (a -> f a) -> s -> f s
< modifyF getter setter k x = fmap (setter x) . k . getter $ x

The type of `setter` is `(s -> a -> s)`; however, nothing in the
implementation forces the first argument and the result to have the same
type. Furthermore, with a different signature `k` could have a more
general type, `(a -> f b)`, as long as the type of `setter` was adjusted
accordingly. We can thus give `modifyF` a more general type.

> modifyGenF :: Functor f => (s -> a) -> (s -> b -> t)
>                         -> (a -> f b) -> s -> f t
> modifyGenF getter setter k x = fmap (setter x) . k . getter $ x

For the sake of completeness, here are the generalised recovery
functions. `get` is not included because the generalisation does not
affect it.

> modifyGen :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
> modifyGen modifier k = runIdentity . modifier (Identity . k)
>
> setGen :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
> setGen modifier x y = modifyGen modifier (const y) x

By now, it is clear that our getters and setters need not be ways to
manipulate fields in a record. In a broader sense, a getter is anything
that produces a value from another; in other words, any function can be
a getter. By the same token, any binary function can be a setter, as all
that is required is that it combines one value with another producing a
third; the initial and final values do not even need to have the same
type.[^laws] That is a long way from the toy data type we started with!

[^laws]: We are not quite as free when it comes to pairing getters and
setters. Beyond the obvious need for getter and setter to start from
values of the same type, they should behave sanely when composed. In
particular, the following should hold:

    ``` { .haskell }
    get' modifier (setGen modifier y x) ≡ y

    setGen modifier (get' modifier x) x ≡ x

    setGen modifier z (setGen modifier y x) ≡ setGen modifier z x
    ```

The Reveal
----------

If we look at `modifyGenF` as a function of two arguments, its result
type becomes:

< Functor f => (a -> f b) -> s -> f t

Now, let's take a peek at
[Control.Lens.Lens](http://hackage.haskell.org/package/lens-4.1.2/docs/Control-Lens-Lens.html#t:Lens):

< type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

It is the same type! We have reached our destination.[^forall]
A lens is what we might have called a generalised functorial modifier;
furthermore, sans implementation details we have that:

- The `lens` function is `modifyGenF`;
- `modifyF` is `lens` specialised to produce simple lenses;[^simple]
- `modifyBarF` is a lens with type `Lens Foo Foo Int Int`;
- `(^.)` is flipped `get'`;
- `set` is `setGen`;
- `over` is `modifyGen` further generalised.[^profunctors]

[^forall]: "What about the `forall`?" you might ask. Are we cheating?
Not quite. The `forall` is there to control how `f` is specialised when
lens combinators are used.  The underlying issue does not affect our
reasoning here. If you are into type system subtleties, there were a few
interesting comments about it in the [reddit
thread](http://www.reddit.com/r/haskell/comments/241aec/lenses_you_can_make_at_home/ch2rbgp)
for this post.

[^simple]: `Lens' s a` or `Lens s s a a`, as opposed to `Lens s t a b`.

[^profunctors]: Yes, even further; from taking modifying functions to
taking modifying
[profunctors](https://www.fpcomplete.com/user/liyang/profunctors). The
difference need not worry us now.

`lens` uses type synonyms liberally, so those correspondences are not
immediately obvious form the signatures in the documentation. Digging a
little deeper, however, shows that in

< set :: ASetter s t a b -> b -> s -> t

`ASetter` is merely

< type ASetter s t a b = (a -> Identity b) -> s -> Identity t

Analogously, we have

< (^.) :: s -> Getting a s a -> a
<
< type Getting r s a = (a -> Const r a) -> s -> Const r s

Behind the plethora of type synonyms - `ASetter`, `Getting`, `Fold`,
`Traversal`, `Prism`, `Iso` and so forth - there are different choices
of functors,[^profunctors2] which make it possible to capture many
different concepts as variations on lenses. The variations may be more
general or less general than lenses; occasionally they are neither, as
the overlap is just partial. The fact that we can express so much
through parametrization of functors is key to the extraordinary breadth
of `lens`.

[^profunctors2]: And in some cases of profunctors to replace the
function type constructor.

Going Forward
-------------

This exposition is primarily concerned with building lenses, and so very
little was said about how to use them. In any case, we have seen enough
to understand why lenses are also known as functional references. By
unifying getters and setters, lenses provide a completely general
vocabulary to point at parts of a whole.

Finally, a few words about composition of lenses are unavoidable. One of
the great things about lenses is that they are just functions; even
better, they are functions with signatures tidy enough for them to
compose cleanly with `(.)`. That makes it possible to compose lenses
independently of whether you intend to get, set or modify their targets.
Here is a quick demonstration using the tuple lenses from `lens`.

< GHCi> :m
< GHCi> :m +Control.Lens
< GHCi> ((1,2),(3,4)) ^. _1 . _2
< GHCi> 2
< GHCi> set (_1 . _2) 0 ((1,2),(3,4))
< GHCi> ((1,0),(3,4))

A perennial topic in discussions about `lens` is the order of
composition of lenses. They are often said to compose backwards; that
is, backwards with respect to composition of record accessors and
similar getters. For instance, the getter corresponding to the `_1 . _2`
lens is `snd . fst`. The claim that lenses compose backwards, or in the
"wrong order", however, are only defensible when talking about style,
and not about semantics. That becomes clear after placing the signatures
of a getter and its corresponding lens side by side.

< GHCi> :t fst
< fst :: (a, b) -> a
< GHCi> :t _1 :: Lens' (a, b) a
< _1 :: Lens' (a, b) a
<  :: Functor f => (a -> f a) -> (a, b) -> f (a, b)

The getter takes a value of the source type and produces a value of the
target type. The lens, however, takes a function from the target type
and produces a function from the source type. Therefore, it is no
surprise that the order of composition differs, and the order for lenses
is entirely natural. That ties in closely to what we have seen while
implementing lenses. While we can squeeze lenses until they give back
getters, it is much easier to think of them as generalised modifiers.
