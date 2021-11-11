#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , contravariant
             , lens
-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
-- |
-- Module      :  Quartet.EndNote
-- Author      :  Daniel Mlot
-- License     :  CC0 (see https://creativecommons.org/publicdomain/zero/1.0/ )
--
-- Examples from the "Handling nested Either" end note to "Divisible
-- and the monoidal quartet"
-- https://duplode.github.io/posts/divisible-and-the-monoidal-quartet.html

module Quartet.EndNote where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Control.Lens hiding (chosen)
import Control.Lens.TH
import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

import Quartet.Combinators

main :: IO ()
main = return ()

data Foo = Bar String | Baz Bool | Quux Int
    deriving Show
makePrisms ''Foo

pString :: Predicate String
pString = Predicate (const False)

pBool :: Predicate Bool
pBool = Predicate id

pInt :: Predicate Int
pInt = Predicate (>= 0)

-- Strategy one: prisms, outside, and chosen

anonSum :: APrism' s a -> (s -> b) -> s -> Either a b
anonSum p cases = set (outside p) Left (Right . cases)

decidableOutside :: Predicate Foo
decidableOutside = analyse >$< pString |-| pBool |-| pInt
    where
    analyse = _Bar `anonSum` (_Baz `anonSum` (^?! _Quux))

-- Strategy two: prisms, outside, and choose

matchingL :: APrism' s a -> s -> Either a s
matchingL p = view swapped . matching p

decidableMatching :: Predicate Foo
decidableMatching =
    choose (matchingL _Bar) pString $
    choose (matchingL _Baz) pBool $
    choose (matchingL _Quux) pInt $
    error "Missing case in decidableMatching"

-- Strategy three: faking it with arrows

newtype Pipecade a b = Pipecade { getPipecade :: a -> (Bool, b) }

instance Category Pipecade where
    id = Pipecade (True,)
    Pipecade q . Pipecade p = Pipecade $ \x ->
        let (bx, y) = p x
            (by, z) = q y
        in (bx && by, z)

instance Arrow Pipecade where
    arr f = Pipecade (const True &&& f)
    first (Pipecade p) = Pipecade $ \(x, o) ->
         let (bx, y) = p x
         in (bx, (y, o))

instance ArrowChoice Pipecade where
    left (Pipecade p) = Pipecade $ \case
        Left x ->
            let (bx, y) = p x
            in (bx, Left y)
        Right o -> (True, Right o)

fromPred :: Predicate a -> Pipecade a ()
fromPred (Predicate p) = Pipecade (p &&& const ())

toPred :: Pipecade a x -> Predicate a
toPred (Pipecade p) = Predicate (fst . p)

decidableArrowised :: Predicate Foo
decidableArrowised = toPred $ proc foo -> case foo of
    Bar s -> fromPred pString -< s
    Baz b -> fromPred pBool -< b
    Quux n -> fromPred pInt -< n
