{-|
Module      : Calculator.Ring
Description : Defines ring, field classes and relevant implementation
Copyright   : Siyang Ling, 2015
License     : GPL-3
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Defines the Ring, Field class, implement rings of integer mod p. Everything are assumed to be commutative.
-}
module Calculator.Ring where

import Data.List

-- | The class Ring: a ring contains zero, one,
-- additive inverse (neg), multiplicative inverse (inv)
class (Num a) => Ring a where
  {-# MINIMAL zero, one, neg, inv #-}
  zero :: a
  one :: a
  neg :: a -> a
  inv :: a -> a

-- | ModArith represents arithmetic mod p
-- Mod n a represents a mod n
data ModArith = Mod Integer Integer

-- | Shows Mod n a in a (mod n)
instance Show ModArith where
  show (Mod n a) = show (a `mod` n)

-- | Natural Eq instance
instance Eq ModArith where
  (Mod n a) == (Mod m b) = if n==m
                           then (a-b) `mod` n == 0
                           else error "Different ring"


