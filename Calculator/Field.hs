--
--asdfadsf
--
module Field where

import Data.List

class (Num a) => Field a where
  {-# MINIMAL zero, one, inverse #-}
  zero :: a
  one :: a
  inverse :: a -> a

data PrimeField = Mod Integer Integer

instance Show PrimeField where
  show (Mod p a) = show (a `mod` p)

instance Eq PrimeField where
  (Mod p a) == (Mod q b) = (p==q) && ((a-b) `mod` p == 0)

instance Num PrimeField where
  (Mod p a) + (Mod q b) = if p==q
                          then Mod p ((a+b) `mod` p)
                          else Mod 0 0
  (Mod p a) * (Mod q b) = if p==q
                          then Mod p ((a*b) `mod` p)
                          else Mod 0 0
  abs = id
  signum (Mod p a) = if a==0 then Mod p 0 else Mod p 1
  fromInteger p = Mod p 0
  negate (Mod p a) = Mod p ((-a) `mod` p)
  

