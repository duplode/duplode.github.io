---
title: "Traversable: A Remix"
published: 2017-05-19T07:30:00Z
license: CC-BY-SA
reddit: 6c2819
gh-issue: 13
---

`Traversable` is a fun type class. It lies at a crossroad, where many
basic Haskell concepts meet, and it can be presented in multiple ways
that provide complementary intuitions. In this post, `Traversable`
will be described from a slightly unusual point of view, or at least
one that is not put into foreground all that often. We will suspend
for a moment the picture of walking across a container while using an
effectful function, and instead start by considering what can be done
with effectful functions.

<div></div><!--more-->

Weird fishes
-----

Let's begin with a familiar sight:

``` haskell
a -> F b
```

There are quite a few overlapping ways of talking about functions with
such a type. If `F` is a `Functor`, we can say the function produces a
functorial context; if it is an `Applicative`, we (also) say it
produces an effect; and if it is a `Monad` we (also) call it a Kleisli
arrow. Kleisli arrows are the functions we use with `(>>=)`. Kleisli
arrows for a specific `Monad` form a category, with `return` as
identity and the fish operator, `(<=<)`, as composition. If we pick
`join` as the fundamental `Monad` operation, `(<=<)` can be defined in
terms of it as:

``` haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = join . fmap g . f
```

The category laws, then, become an alternative presentation of the
monad laws:

``` haskell
return <=< f = f
f <=< return = f
(h <=< g) <=< f = h <=< (g <=< f)
```

All of that is very well-known. Something less often noted, though, is
that there is an interesting category for `a -> F b` functions even if
`F` is not a `Monad`. Getting to it is amusingly easy: we just have to
take the Kleisli category operators and erase the monad-specific parts
from their definitions. In the case of `(<=<)`, that means removing
the `join` (and, for type bookkeeping purposes, slipping in a
[`Compose`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Compose.html)
in its place):

``` haskell
(<%<) :: (Functor f, Functor g) =>
    (b -> g c) -> (a -> f b) -> (a -> Compose f g c)
g <%< f = Compose . fmap g . f
```

While `(<=<)` creates two monadic layers and merges them, `(<%<)`
creates two functorial layers and leaves both in place. Note that
doing away with `join` means the `Functor`s introduced by the
functions being composed can differ, and so the category we are
setting up has *all* functions that fit `Functor f => a -> f b` as
arrows. That is unlike what we have with `(<=<)` and the corresponding
Kleisli categories, which only concern a single specific monad.

As for `return`, not relying on `Monad` means we need a different
identity. Given the freedom to pick any `Functor` mentioned just
above, it makes perfect sense to replace bringing a value into a
`Monad` in a boring way by bringing a value into the boring `Functor`
*par excellence*,
[`Identity`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Identity.html):

``` haskell
Identity :: a -> Identity a
```

With `(<%<)` as composition and `Identity` as identity, we can state
the following category laws:

``` haskell
Identity <%< f ~ f
f <%< Identity ~ f
(h <%< g) <%< f ~ h <%< (g <%< f)
```

Why didn't I write them as equalities? Once the definition of `(<%<)`
is substituted, it becomes clear that they do not hold literally as
equalities: the left hand sides of the identity laws will have a stray
`Identity`, and the uses of `Compose` on either side of the
associativity law will be associated differently. Since `Identity` and
`Compose` are essentially bookkeeping boilerplate, however, it would be
entirely reasonable to ignore such differences. If we do that, it
becomes clear that the laws do hold. All in all, we have a category,
even though we can't go all the way and shape it into a
[`Category`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Category.html)
instance, not only due to the trivialities we chose to overlook, but
also because of how each `a -> F b` function introduces a functorial
layer `F` in a way that is not reflected in the target object `b`.

The first thing to do once after figuring out we have a category in
our hands is looking for functors involving it.[^functor-dp] One of
the simplest paths towards one is considering a way to, given some
`Functor` `T`, change the source and target objects in an `a -> F b`
function to `T a` and `T b` (that is precisely what `fmap` does with
regular functions). This would give an endofunctor, whose arrow
mapping would have a signature shaped like this:

[^functor-dp]: For why that is a good idea, see Gabriel Gonzalez's
    [*The functor design pattern*](http://www.haskellforall.com/2012/09/the-functor-design-pattern.html).

``` haskell
(a -> F b) -> T a -> F (T b)
```

This signature shape, however,
[should ring a bell](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Traversable.html):

``` haskell
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    -- etc.
```

If `traverse` were the arrow mapping of our endofunctor, the relevant
functor laws would be:

``` haskell
traverse Identity = Identity
traverse (g <%< f) = traverse g <%< traverse f
```

Substituting the definition of `(<%<)` reveals these are the identity
and composition laws of `Traversable`:

``` haskell
traverse Identity = Identity
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
```

There it is: a `Traversable` instance is an endofunctor for a category
made of arbitrary context-producing functions.[^cat-laws]

[^cat-laws]: A more proper derivation for the results in this section
    can be found
    [in this Stack Overflow answer](http://stackoverflow.com/a/39955475/2751851),
    which I didn't transcribe here to avoid boring you.

Is it really, though? You may have noticed I have glossed over
something quite glaring: if `(<%<)` only involved `Functor`
constraints, where does the `Applicative` in `traverse` comes from?

Arpeggi
-------

Let's pretend we have just invented the `Traversable` class by
building it around the aforementioned endofunctor. At this point,
there is no reason for using anything more restrictive than `Functor`
in the signature of its arrow mapping:

``` haskell
-- Tentative signature:
traverse :: (Functor f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

The natural thing to do now is trying to write `traverse` for various
choices of `t`. Let's try it for one of the simplest `Functor`s
around: the pair functor, `(,) e` -- values with something extra
attached:

``` haskell
instance Traversable ((,) e) where
    -- traverse :: Functor f => (a -> f b) -> (e, a) -> f (e, b)
    traverse f (e, x) = ((,) e) <$> f x
```

Simple enough: apply the function to the contained value, and then
shift the extra stuff into the functorial context with `fmap`. The
resulting `traverse` follows the functor laws just fine.

If we try to do it for different functors, though, we quickly run into
trouble. `Maybe` looks simple enough...

``` haskell
instance Traversable Maybe where
    -- traverse :: Functor f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse f (Just x) = Just <$> f x
    traverse f Nothing  = -- ex nihilo
```

... but the `Nothing` case stumps us: there is no value that can be
supplied to `f`, which means the functorial context would have to be
created out of nothing.

For another example, consider what we might do with an homogeneous
pair type (or, if you will, a vector of length two):

``` haskell
data Duo a = Duo a a

instance Functor Duo where
    fmap f (Duo x y) = Duo (f x) (f y)

instance Traversable Duo where
    -- traverse :: Functor f => (a -> f b) -> Duo a -> f (Duo b)
    traverse f (Duo x y) = -- dilemma
```

Here, we seemingly have to choose between applying `f` to `x` or to
`y`, and then using `fmap (\z -> Duo z z)` on the result. No matter
the choice, though, discarding one of the values means the functor
laws will be broken. A lawful implementation would require somehow
combining the functorial values `f x` and `f y`.

As luck would have it, though, there is a type class which provides
ways to both create a functorial context out of nothing and to combine
functorial values: `Applicative`. `pure` solves the first problem;
`(<*>)`, the second:

``` haskell
instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse f (Just x) = Just <$> f x
    traverse f Nothing  = pure Nothing

instance Traversable Duo where
    -- traverse :: Applicative f => (a -> f b) -> Duo a -> f (Duo b)
    traverse f (Duo x y) = Duo <$> f x <*> f y
```

Shifting to the terminology of containers for a moment, we can
describe the matter by saying the version of `traverse` with the
`Functor` constraint can only handle containers that hold exactly one
value. Once the constraint is strengthened to `Applicative`, however,
we have the means to deal with containers that may hold zero or many
values. This is a very general solution: there are instances of
`Traversable` for the `Identity`,
[`Const`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Const.html),
[`Sum`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Sum.html),
and
[`Product`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor-Product.html)
functors, which suffice to encode any algebraic data type.[^unit-void]
That explains why the `DeriveTraversable` GHC extension exists. (Note,
though, that `Traversable` instances in general aren't unique.)

[^unit-void]: Suffice, that is, with the help of the trivial data
    types, `()` (unit) and
    [`Void`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Void.html).
    As an arbitrary example, `Maybe` can be encoded using this functor
    toolkit as `Sum (Const ()) Identity`.

It must be noted that our reconstruction does not reflect how
`Traversable` was discovered, as the idea of using it to walk across
containers holding an arbitrary number of values was there from the
start. That being so, `Applicative` plays an essential role in the
usual presentations of `Traversable`. To illustrate that, I will now
paraphrase Definition 3.3 in Jaskelioff and Rypacek's
[*An Investigation of the Laws of Traversals*](https://arxiv.org/abs/1202.2919).
It is formulated not in terms of `traverse`, but of `sequenceA`:

``` haskell
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
```

`sequenceA` is characterised as a natural transformation in the
category of applicative functors which "respects the monoidal
structure of applicative functor composition". It is worth it to take
a few moments to unpack that:

* The category of applicative functors has what the
  [`Data.Traversable` documentation](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Traversable.html)
  calls "applicative transformations" as arrows -- functions of
  general type `(Applicative f, Applicative g) => f a -> g a` which
  preserve `pure` and `(<*>)`.

* `sequenceA` is a natural transformation in the aforementioned
  category of applicative functors. The two functors it maps between
  amount to the two ways of composing an applicative functor with the
  relevant traversable functor. The naturality law of `Traversable`...

    ``` haskell
    -- t is an applicative transformation
    t . sequenceA = sequenceA . fmap t
    ```

  ... captures that fact (which, thanks to parametricity, is a given
  in Haskell).

* Applicative functors form a monoid, with `Identity` as unit and
  functor composition as multiplication. `sequenceA` preserves these
  monoidal operations, and the identity and composition laws of
  `Traversable` express that:

    ``` haskell
    sequenceA . fmap Identity = Identity
    sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
    ```

All of that seems only accidentally related to what we have done up to
this point. However, if `sequenceA` is taken as the starting point,
`traverse` can be defined in terms of it:

``` haskell
traverse f = sequenceA . fmap f
```

Crucially, the opposite path is also possible. It follows from
parametricity[^traverse-ft] that...

[^traverse-ft]: The property is an immediate consequence of the free
    theorem for `traverse`. Cf.
    [this Stack Overflow answer by Rein Heinrichs](http://stackoverflow.com/a/32813063/2751851).

``` haskell
traverse f = traverse id . fmap f
```

... which allows us to start from `traverse`, define...

``` haskell
sequenceA = traverse id
```

... and continue as before. At this point, our narrative merges with
the traditional account of `Traversable`.

A note about lenses
-------------------

In the previous section, we saw how using `Applicative` rather than
`Functor` in the type of `traverse` made it possible to handle
containers which don't necessarily hold just one value. It is not a
coincidence that, in *lens*, this is precisely the difference between
`Traversal` and `Lens`:

``` haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

A `Lens` targets exactly one value. A `Traversal` might reach zero,
one or many targets, which requires a strengthening of the
constraint. Van Laarhoven (i.e. *lens*-style) `Traversal`s and
`Lens`es can be seen as a straightforward generalisation of the
`traverse`-as-arrow-mapping view we have been discussing here, in
which the, so to say, functoriality of the container isn't necessarily
reflected at type level in a direct way.

A note about profunctors
------------------------

Early on, we noted that `(<%<)` gave us a category that cannot be
expressed as a Haskell `Category` because its composition is too
quirky. We have a general-purpose class that is often a good fit for
things that look like functions, arrows and/or `Category` instances
but don't compose in conventional ways: `Profunctor`. And sure enough:
*profunctors* defines a profunctor called
[`Star`](https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor.html#t:Star)...

``` haskell
-- | Lift a 'Functor' into a 'Profunctor' (forwards).
newtype Star f d c = Star { runStar :: d -> f c }
```

... which corresponds to the arrows of the category we presented in
the first section. It should come as no surprise that `Star` is an
instance of a class called `Traversing`...

``` haskell
-- Abridged definition.
class (Choice p, Strong p) => Traversing p where
  traverse' :: Traversable f => p a b -> p (f a) (f b)
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

instance Applicative m => Traversing (Star m) where
  traverse' (Star m) = Star (traverse m)
  wander f (Star amb) = Star (f amb)
```

... which is a profunctor-oriented generalisation of `Traversable`.

Amusingly, it turns out there is a baroque way of expressing `(<%<)`
composition with the *profunctors* vocabulary.
[`Data.Profunctor.Composition`](https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor-Composition.html)
gives us a notion of profunctor composition:

``` haskell
data Procompose p q d c where
  Procompose :: p x c -> q d x -> Procompose p q d c
```

`Procompose` simply pairs two profunctorial values with matching
extremities. That is unlike `Category` composition, which welds two
arrows[^non-arrow] into one:

[^non-arrow]: I mean "arrows" in the general sense, and not necessarily
    `Arrow`s as in
    [`Control.Arrow`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Arrows.html)!

``` haskell
(.) :: Category cat => cat b c -> cat a b -> cat a c
```

The difference is rather like that between combining functorial layers
at type level with `Compose` and fusing monadic layers with
`join`[^arrow-monoid].

[^arrow-monoid]: This is not merely a loose analogy. For details, see
    Bartosz Milewski's
    [*Monoids on Steroids*](https://bartoszmilewski.com/2017/02/09/monoids-on-steroids/), and
    and in particular its section about `Arrow`s.

Among a handful of other interesting things, `Data.Functor.Procompose`
offers
[a *lens*-style isomorphism](https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor-Composition.html#v:stars)...

``` haskell
stars :: Functor f => Iso' (Procompose (Star f) (Star g) d c) (Star (Compose f g) d c)
```

... which gives us a rather lyrical encoding of `(<%<)`:

``` haskell
GHCi> import Data.Profunctor
GHCi> import Data.Profunctor.Composition
GHCi> import Data.Profunctor.Traversing
GHCi> import Data.Functor.Compose
GHCi> import Control.Lens
GHCi> f = Star $ \x -> print x *> pure x
GHCi> g = Star $ \x -> [0..x]
GHCi> getCompose $ runStar (traverse' (view stars (g `Procompose` f))) [0..2]
0
1
2
[[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2]]
```

If you feel like playing with that, note that
[`Data.Profunctor.Sieve`](https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor-Sieve.html)
offers a more compact (though prosaic) spelling:

``` haskell
GHCi> import Data.Profunctor.Sieve
GHCi> :t sieve
sieve :: Sieve p f => p a b -> a -> f b
GHCi> getCompose $ traverse (sieve (g `Procompose` f)) [0..2]
0
1
2
[[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2]]
```

Further reading
---------------

* The already mentioned
  [*An Investigation of the Laws of Traversals*](https://arxiv.org/abs/1202.2919),
  by Mauro Jaskelioff and Ondrej Rypacek, is a fine entry point to the
  ways of formulating `Traversable`. It also touches upon some
  important matters I didn't explore here, such as how the notion of
  container `Traversable` mobilises can be made precise, or the
  implications of the `Traversable` laws. I plan to discuss some
  aspects of these issues in a follow-up post.

* Will Fancher's
  [*Profunctors, Arrows, & Static Analysis*](http://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html)
  is a good applied introduction to profunctors. In its final
  sections, it demonstrates some use cases for the `Traversing` class
  mentioned here.

* The explanation of profunctor composition in this post is
  intentionally cursory. If you want to dig deeper, Dan Piponi's
  [*Profunctors in Haskell*](http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html)
  can be a starting point. (N.B.: Wherever you see "cofunctor" there,
  read "contravariant functor" instead). Another option is going to
  [Bartosz Milewski's blog](https://bartoszmilewski.com) and searching
  for "profunctor" (most of the results will be relevant).
