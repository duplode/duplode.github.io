---
title: Applicative Archery
published: 2015-07-06T17:00:00-03:00
license: CC-BY-SA
reddit: 3ccglr
gh-issue: 5
---

It is widely agreed that the laws of the `Applicative` class are not
pretty to look at.

< pure id <*> v = v                            -- identity
< pure f <*> pure x = pure (f x)               -- homomorphism
< u <*> pure y = pure ($ y) <*> u              -- interchange
< pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- composition

Monad laws, in comparison, not only look less odd to begin with but
can also be stated in a much more elegant way in terms of Kleisli
composition `(<=<)`. Shouldn't there be an analogous nice presentation
for `Applicative` as well? That became a static question in my mind
while I was studying applicative functors many moons ago. After finding
surprisingly little commentary on this issue, I decided to try figuring
it out by myself. <!--more--> [^monoidal]

[^monoidal]: There is a reasonably well-known alternative formulation
of `Applicative`: the `Monoidal` class as featured in
[this post by Edward Z. Yang](http://blog.ezyang.com/2012/08/applicative-functors).
It is quite handy to work with when it comes to checking whether an
instance follows the laws.

Let's cast our eye over `Applicative`:

< class Functor t => Applicative t where
<     pure  :: a -> t a
<     (<*>) :: t (a -> b) -> t a -> t b

If our inspiration for reformulating `Applicative` is Kleisli
composition, the only sensible plan is to look for a category in which
the `t (a -> b)` functions-in-a-context from the type of `(<*>)` are
the arrows, just like `a -> t b` functions are arrows in a Kleisli
category. Here is one way to state that plan in Haskell terms:

> class Applicative t => Starry t where
>     idA  :: t (a -> a)
>     (.*) :: t (b -> c) -> t (a -> b) -> t (a -> c)
>
>     infixl 4 .*
> -- The Applicative constraint is wishful thinking:
> -- When you wish upon a star...

The laws of `Starry` are the category laws for the `t (a -> b)` arrows:

< idA .* v = v                -- left identity
< u .* idA = u                -- right identity
< u .* v .* w = u .* (v .* w) -- associativity

The question, then, is whether it is possible to reconstruct
`Applicative` and its laws from `Starry`. The answer is a resounding
yes! The proof is in
[this manuscript](/extras/applicative-archery-manuscript.pdf),
which I have not transcribed here as it is a little too long for a
leisurely post like this one [^manuscript]. The argument is set in
motion by establishing that `pure` is an arrow mapping of a functor from
**Hask** to a `Starry` category, and that both `(<*>)` and `(.*)` are
arrow mappings of functors in the opposite direction. That leads to
several naturality properties of those functors, from which the
`Applicative` laws can be obtained.  Along the way, we also get
definitions for the `Starry` methods in terms of the `Applicative`
ones...

[^manuscript]: Please excuse some oddities in the manuscript, such as
off-kilter terminology and weird conventions (e.g. consistently naming
arguments in applicative style as `w <*> v <*> u` rather than `u <*> v
<*> w` in applicative style). The most baffling choice was using `id`
rather than `()` as the throwaway argument to `const`. I guess I did
that because `($ ())` looks bad in handwriting.

>     idA = pure id
>     u .* v = fmap (.) u <*> v

... and vice-versa:

< pure x = fmap (const x) idA
< u <*> v = fmap ($ ()) (u .* fmap const v)

Also interesting is how the property relating `fmap` and `(<*>)`...

< fmap f u = pure f <*> u

... now tells us that a `Functor` results from composing the `pure`
functor with the `(<*>)` functor. That becomes more transparent if we
write it point-free:

< fmap = (<*>) . pure

In order to ensure `Starry` is equivalent to `Applicative` we still need
to prove the converse, that is, obtain the `Starry` laws from the
`Applicative` laws plus the definitions of `idA` and `(.*)` just above.
That is not difficult; all it takes is substituting the definitions in
the `Starry` laws and:

 - For left identity, noticing that `(id .) = id`.

 - For right identity, applying the interchange law and noticing that
   `($ id) . (.)` is `id` in a better disguise.

 - For associativity, using the laws to move all `(.)` to the left of
   the `(<*>)` and then verifying that the resulting messes of dots in
   both sides are equivalent.

As a tiny example, here is the `Starry` instance of `Maybe`...

< instance Starry Maybe where
<     idA              = Just id
<     Just g .* Just f = Just (g . f)
<     _      .* _      = Nothing

... and the verification of the laws for it:

< -- Left identity:
< idA .* u = u
< Just id .* u = u
< -- u = Nothing
< Just id .* Nothing = Nothing
< Nothing = Nothing
< -- u = Just f
< Just id .* Just f = Just f
< Just (id . f) = Just f
< Just f = Just f
<
< -- Right identity:
< u .* idA = u
< u .* Just id = u
< -- u = Nothing
< Nothing .* Just id = Nothing
< Nothing = Nothing
< -- u = Just g
< Just g .* Just id = Just g
< Just (g .* id) = Just g
< Just g = Just g
<
< -- Associativity:
< u .* v .* w = u .* (v .* w)
< -- If any of u, v and w are Nothing, both sides will be Nothing.
< Just h .* Just g .* Just f = Just h .* (Just g .* Just f)
< Just (h . g) .* Just f = Just h .* (Just (g . f))
< Just (h . g . f) = Just (h . (g . f))
< Just (h . g . f) = Just (h . g . f)

It works just as intended:

< GHCi> Just (2*) .* Just (subtract 3) .* Just (*4) <*> Just 5
< Just 34
< GHCi> Just (2*) .* Nothing .* Just (*4) <*> Just 5
< Nothing

I do not think there will be many opportunities to use the `Starry`
methods in practice. We are comfortable enough with applicative style,
through which we see most `t (a -> b)` arrows as intermediates generated
on demand, rather than truly meaningful values. Furthermore, the
`Starry` laws are not really easier to prove (though they are
certainly easier to remember!). Still, it was an interesting exercise to
do, and it eases my mind to know that there is a neat presentation of
the `Applicative` laws that I can relate to.

This post is Literate Haskell, in case you wish to play with `Starry` in
GHCi (here is [the raw .lhs file
](https://raw.githubusercontent.com/duplode/duplode.github.io/sources/src/posts/applicative-archery.lhs)
).

> instance Starry Maybe where
> instance Starry [] where
> instance Starry ((->) a) where
> instance Starry IO where

As for proper implementations in libraries, the closest I found was
[`Data.Semigroupoid.Static`](https://hackage.haskell.org/package/semigroupoids-5.0.0.2/docs/Data-Semigroupoid-Static.html),
which lives in Edward Kmett's [`semigroupoids`
](https://hackage.haskell.org/package/semigroupoids-5.0.0.2)
package. *"Static arrows"* is the actual technical term for the
`t (a -> b)` arrows. The module provides...

< newtype Static f a b = Static { runStatic :: f (a -> b) }

... which uses the definitions shown here for `idA` and `(.*)` as `id`
and `(.)` of its `Category` instance.
