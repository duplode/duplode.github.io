---
title: What Does fmap Preserve?
published: 2014-06-02T06:00:00Z
license: CC-BY-SA
reddit: 2dmfvx
gh-issue: 2
---

A common way of introducing `fmap` is saying that it only changes the
values in a container, and not its structure. Leaving behind the the
functors-as-containers metaphor, we can convey the same idea by saying
that `fmap` leaves the context of the values in a `Functor` unchanged.
But what, exactly, is the "context" or "structure" being preserved? "It
depends on the functor", though correct, is not an entirely satisfactory
answer. The functor laws, after all, are highly abstract, and make no
mention of anything a programmer would be inclined to call "structure"
(say, the skeleton of a list); and yet the preservation we alluded to
follows from them. After struggling a bit with this question, I realised
that the incompatibility is only apparent. This post shows how the
tension can be resolved through the mediation of *parametricity* and
*naturality*, two concepts from different domains that are intertwined
in Haskell.

<div></div><!--more-->

Categorical Cautionary Comment
------------------------------

A correct, if rather cruel, answer to "Why does `fmap` preserve
structure?" would be "By definition, you silly!" To see what would be
meant by that, let's have a look at the functor laws.

``` haskell
fmap id = id                   -- 1st functor law
fmap (g . f) = fmap g . fmap f -- 2nd functor law
```
`fmap` is a mapping of functions that takes identity to identity, and
composed functions to the corresponding composed functions. Identity and
composition make up the structure, in the mathematical sense, of a
category. In category theory, a functor is a mapping between categories
that preserves category structure. Therefore, the functor laws ensure
that Haskell `Functor`s are indeed functors; more precisely, functors
from **Hask** to **Hask**, **Hask** being the category with Haskell
types as objects and Haskell functions as arrows.[^ct]

[^ct]: A category theory primer would be too big a detour for this
post. If the category theory concepts I just mentioned are new to you,
I suggest the following gentle introductions for Haskellers, which have
very different approaches: [Haskell Wikibook chapter on category
theory](https://en.wikibooks.org/wiki/Haskell/Category_theory), and
Gabriel Gonzalez's posts [The category design
pattern](http://www.haskellforall.com/2012/08/the-category-design-pattern.html)
and [The functor design
pattern](http://www.haskellforall.com/2012/09/the-functor-design-pattern.html).

That functors preserve category structure is evident. However, our
question is not directly about "structure" in the mathematical sense,
but with the looser acception it has in programmer parlance. In what
follows, our goal will be clarifying this casual meaning.

What Can You Do With a Function?
--------------------------------

As an intial, fuzzy characterisation, we can say that, given a
functorial value, the `Functor` context is everything in it other than
the wrapped values. Starting from that, a straightforward way of showing
why `fmap` preserves context involves *parametric polymorphism*; more
specifically, the preservation is ensured by the wild generality of the
types in the signature of `fmap`.

``` haskell
fmap :: (Functor t) => (a -> b) -> (t a -> t b)
```

We will look at `fmap` as a function of one argument which converts a
plain `a -> b` function into a function which operates on functorial
values. The key fact is that there is very little we can do with the `a
-> b` function when defining `fmap`. Composition is not an option, as
choosing a function other than `id` to compose it with would require
knowledge about the `a` and `b` types. The only thing that can be done
is applying the function to any `a` values we can retrieve from the `t
a` functorial value. Since the context of a `t a` value, whatever it is,
does not include the `a` values, it follows that changes to the context
cannot depend on the `a -> b` function.  Given that `fmap` takes no
other arguments, any changes in the context must happen for any `a -> b`
arguments uniformly. The first functor law, however, says that `fmap id
= id`, and so there is one argument, `id`, which leads to no changes in
the context. Therefore, `fmap` never changes the context.

The informal argument above can be made precise through a proper type
theory treatment of parametricity. Philip Wadler's *[Theorems for
free!](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html#free)*
is a well-known example of such work. However, a type theory approach,
while entirely appropriate, would have us taking concrete Haksell types
for granted and only incidentally concluding they are functors; in
contrast, our problem begins with functors. For that reason, we will
follow a different path and look at the issue from a primarily
categorical point of view.

What Is a Context, After All?
-----------------------------

In the spirit of category theory, we will now focus not on the types but
on the functions between them. After all, given functional purity any
interesting properties of a Haskell value can be verified with suitable
functions. Let's start with a few concrete examples of how the context
of a `Functor` can be probed with functions.

``` haskell
length :: [a] -> Int
```

The length of a list is perhaps the most obvious example of a structural
property. It depends only on the list skeleton, and not at all on the
values in it. The type of `length`, with a fully polymorphic element
type which is not mentioned by the result type, reflects such an
independence. An obvious consequence is that `fmap`, which only affects
the list elements, cannot change the length of a list. We can state that
like this:

``` haskell
length xs = length (fmap f xs)
```

Or, in a more categorical fashion:

``` haskell
length = length . fmap f
```

Our second example of a structure-probing function will be `reverse`:

``` haskell
reverse :: [a] -> [a]
```

While the result value of `reverse` obviously depends on the list
elements, `reverse` cannot actually modify the elements, given that the
function is fully polymorphic on the element type. `fmap` applied to a
list after reversing it will thus affect the same element values there
were before the reversal; they will only have been rearranged. In other
words, `fmap` *commutes* with `reverse`:

``` haskell
fmap f . reverse = reverse . fmap f
```

Our final example will be `listToMaybe` from `Data.Maybe`:

``` haskell
listToMaybe :: [a] -> Maybe a
```

Operationally, `listToMaybe` is a safe version of `head`, which returns
`Nothing` when given an empty list. Again, the function is fully
polymorphic in the element type, and so the value of the first element
cannot be affected by it. The scenario is very similar to what we have
seen for `reverse`, and an analogous property holds, with the only
difference being that `fmap` is instantiated at a different `Functor` at
each side of the equation:

``` haskell
-- Maybe-fmap on the left, []-fmap on the right.
fmap f . listToMaybe = listToMaybe . fmap f
```

Earlier we said that the `Functor` context consists of everything but
the wrapped values. Our examples illustrate how parametric polymorphism
makes it possible to keep that general idea while putting functions
rather than values under the spotlight. The context is all that can be
probed with functions fully polymorphic on the type parameter of the
`Functor`; or, taking the abstraction further, the context *is* the
collection of functions fully polymorphic on the type parameter of the
`Functor`. We now have done away with the fuzziness of our preliminary,
valure-centric definition. The next step is clarifying how that
definition relates to `fmap`.

Your Freedom Comes Naturally
----------------------------

By identifying the `Functor` context with polymorphic functions, we can
also state the context-preserving trait of `fmap` through commutativity
equations like those shown in the above examples. For an arbitrary
context-probing function `r`, the equation is:

``` haskell
-- f is arbitrary, and so are the involved functors.
fmap f . r = r . fmap f
```

The equations for `reverse` and `listToMaybe` clearly have that shape.
`length` does not seem to fit at first sight, but that can be easily
solved by lifting it to a constant functor such as the one provided by
`Control.Applicative`.

``` haskell
lengthC :: [a] -> Const Int a
lengthC = Const . length
-- length = getConst . lengthC

-- For constant functors, fmap f = id regardless of f.
fmap f . lengthC = lengthC . fmap f
```

A similar trick can be done with the `Identity` functor to make
functions in which the type parameter of the `Functor` appears bare,
such as `Just :: a -> Maybe a`, fit our scheme.

It turns out that there is a category theory concept that captures the
commutativity property we are interested in. A *natural transformation*
is a translation between functors which preserves arrows being mapped
through them. For Haskell `Functor`s, that amounts to preserving
functions being mapped via `fmap`. We can display the relation through a
diagram:

![*Naturality for Haskell `Functor`s. Example instantation:
`T = []; U = Maybe; r = listToMaybe`.*
](/images/posts/what-does-fmap-preserve/naturality-diagram.png)

The naturality condition matches our commuativity property. Indeed,
*polymorphic functions are natural transformations between Haskell
`Functors`*. The proof of this appealing result is not trivial, and
requires some theoretical work, just like in the case of the closely
related results about parametricity we alluded to earlier. In any case,
all it takes to go from "natural transformations preserve `fmap`" to
"`fmap` preserves natural transformations" is tilting our heads while
looking at the diagram above!

Given how we identified `Functor` contexts, polymorphic functions and
natural transformations, we can finally give a precise answer to our
question. The context consists of natural transformations between
functors, and therefore `fmap` preserves it.

Structures and Structures
-------------------------

Earlier on, we have said that we would not be directly concerned with
structure in the sense mathematicians use the word, but only with the
fuzzy Haskell concept that sometimes goes by the same name. To wrap
things up, we will now illustrate the fact that both acceptions are not
worlds apart. Let's have another look at the second functor law, which
states that `fmap` preserves composition:

``` haskell
fmap (g . f) = fmap g . fmap f
```

Structure, in the mathematical sense, refers to some collection of
interesting operations and distinguished elements. In this example, the
relevant operation is function composition, which is part of the
structure of the **Hask** category. Besides that, however, we are now
able to note the uncanny resemblance between the shapes of the law,
which says that it does not matter whether we compose `f` and `g` before
applying `fmap`, and of the commutativity properties we used to
characterise functorial contexts. The upshot is that by identifying
context and structure of a `Functor` with polymorphic functions, we
retain much of the spirit of the mathematical usage of structure. The
interesting operations, in our case, are the polymorphic functions with
which the context is probed.  Perhaps it even makes sense to keep
talking of structure of a `Functor` even after dropping the container
metaphor.

fmap Preserves fmap
-------------------

Speaking of the second law, we will, just for kicks, use it to show how
to turn things around and look at `fmap` as a natural transformation
between `Functor`s. In order to do so, we have to recall that `(.)` is
`fmap` for the function functor:

``` haskell
-- First, we rewrite the second law in a more suggestive form:
fmap (g . f) = fmap g . fmap f
fmap (((.) g) f) = (.) (fmap g) (fmap f)
fmap . (.) g = ((.) . fmap) g . fmap

-- Next, some synonyms to indicate the Functors fmap leads to.

-- fmap from identity to t
fmap_t :: (Functor t) => (->) a b -> (->) (t a) (t b)
fmap_t = fmap

-- fmap from identity to ((->) a)
fmap_fun :: (b -> c) -> ((->) a b -> (->) a c)
fmap_fun = (.)

-- fmap from identity to the composite functor ((->) (t a)) . t
fmap_fun_t :: (Functor t)
           => (b -> c) -> ((->) (t a) (t b) -> (->) (t a) (t c))
fmap_fun_t = fmap_fun . fmap_t

-- The second law then becomes:
fmap_t . fmap_fun g = fmap_fun_t g . fmap_t

-- That, however, shows fmap_t is a natural transformation:
fmap . fmap g = fmap g . fmap
```
By fixing `t` and `a` in the signature of `fmap_t` above, we get one
functor on either side of the outer function arrow: `((->) a)` on the
left and `((->) (t a)) . t` on the right. `fmap` is a natural
transformation between these two functors.

Further Reading
---------------

- In *[The Holy  Trinity
  ](http://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/)*,
  Robert Harper comments on the deep connection between logic, type
  theory and category theory that allows us to shift seamlessly between
  the categorical and the type theoretical perspectives, as we have done
  here.

- *[You Could Have Defined Natural
  Transformations](http://blog.sigfpe.com/2008/05/you-could-have-defined-natural.html)*
  by Dan Piponi is a very clear introduction to natural transformations
  in a Haskell context.

- We have already mentioned Philip Wadler's *[Theorems for
  free!](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html#free)*,
  which is a reasonably accessible introduction to the *free theorems*.
  *Free theorems* are results about functions that, thanks to parametric
  polymorphism, can be deduced from the type of the function alone.
  Given suitable generalisations, free theorems and naturality
  conditions provide two parallel ways of reaching the same results
  about Haskell functions.

- *[Free  Theorems Involving Type Constructor Classes
   ](http://www.janis-voigtlaender.eu/Voi09b.html)*,
  a functional pearl by Janis Voigtländer that illustrates how free
  theorem generation can be generalised to types parametric on type
  constructors and type classes.

- For an explicitly categorical perspective on parametricity, a good
  place to start if you are willing to dig into theory is the section on
  parametricity in *[Some Aspects of Categories in Computer Science
  ](http://www.site.uottawa.ca/~phil/papers/)* by Philip J. Scott.

