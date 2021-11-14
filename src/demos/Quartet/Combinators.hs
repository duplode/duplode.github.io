#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , contravariant
-}
-- |
-- Module      :  Quartet.Combinators
-- Author      :  Daniel Mlot
-- License     :  CC0 (see https://creativecommons.org/publicdomain/zero/1.0/ )
--
-- Key definitions from "Divisible and the monoidal quartet"
-- https://duplode.github.io/posts/divisible-and-the-monoidal-quartet.html

module Quartet.Combinators where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
    (Divisible(..), divided, conquered, Decidable(..), chosen, lost)
import Control.Applicative (liftA2, Alternative(..))
import Control.Arrow
import Data.Void

-- Helpers

dup :: a -> (a, a)
dup = id &&& id

forget :: Either a a -> a
forget = id ||| id

-- Applicative

zipped :: Applicative f => f a -> f b -> f (a, b)
zipped = liftA2 (,)

(&*&) :: Applicative f => f a -> f b -> f (a, b)
(&*&) = zipped
infixr 5 &*&

lizip :: Applicative f => ((a, b) -> c) -> f a -> f b -> f c
lizip f u v = f <$> u &*& v

unit :: Applicative f => f ()
unit = pure ()

-- zipped = lizip id
-- (<*) = lizip fst
-- (*>) = lizip snd

-- Divisible

(>*<) :: Divisible k => k a -> k b -> k (a, b)
(>*<) = divided
infixr 5 >*<

dplus :: Divisible k => k a -> k a -> k a
dplus = divide dup

(>+<) :: Divisible k => k a -> k a -> k a
(>+<) = dplus
infixl 3 >+<

tdivide :: Divisible k => (a -> b) -> (a -> c) -> k b -> k c -> k a
tdivide f g = divide (f &&& g)

-- divided = divide id
-- divide f u v = f >$< u >*< v
-- tdivide f g u v = f >$< u >+< g >$< v

-- Alternative

combined :: Alternative f => f a -> f b -> f (Either a b)
combined u v = Left <$> u <|> Right <$> v

(-|-) :: Alternative f => f a -> f b -> f (Either a b)
(-|-) = combined
infixr 5 -|-

combine :: Alternative f => (Either a b -> c) -> f a -> f b -> f c
combine f u v = f <$> u -|- v

zero :: Alternative f => f Void
zero = empty

aplus :: Alternative f => f a -> f a -> f a
aplus = (<|>)

tcombine :: Alternative f => (a -> c) -> (b -> c) -> f a -> f b -> f c
tcombine f g = combine (f ||| g)

-- combined = combine id
-- tcombine f g u v = f <$> u <|> g <$> v

-- Decidable

(|-|) :: Decidable k => k a -> k b -> k (Either a b)
(|-|) = chosen
infixr 5 |-|

(<|-) :: Decidable k => k a -> k b -> k a
(<|-) = choose Left

(-|>) :: Decidable k => k a -> k b -> k b
(-|>) = choose Right

-- choose f u v = f >$< u |-| v
-- chosen = choose id
