module Matrix where

import Prelude hiding ((+),(*))

import Data.List
import Control.Monad
import Control.Monad.State

class (Eq a) => Field a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  zero :: a
  one :: a
  a * b = b * a
  a + b = b + a

data F5 = Zero|One|Two|Three|Four deriving (Show, Eq, Enum)

instance Field F5 where
  zero = Zero
  one = One

  Zero + Zero = Zero
  Zero + One = One
  Zero + Two = Two
  Zero + Three = Three
  Zero + Four = Four
  One + Zero = One
  Two + Zero = Two
  Three + Zero = Three
  Four + Zero = Four
  
  One + One = Two
  One + Two = Three
  One + Three = Four
  One + Four = Zero
  Two + One = Three
  Three + One = Four
  Four + One = Zero

  Two + Two = Four
  Two + Three = Zero
  Two + Four = One
  Three + Two = Zero
  Four + Two = One

  Three + Three = One
  Three + Four = Two
  Four + Three = Two

  Four + Four = Three
  
  Zero * _ = Zero
  _ * Zero = Zero
  
  One * a = a
  a * One = a
  
  Two * Two = Four
  Two * Three = One
  Two * Four = Three
  Three * Two = One
  Four * Two = Three

  Three * Three = Four
  Three * Four = Two
  Four * Three = Two

  Four * Four = One


minus :: F5 -> F5
minus a = case a of
  Zero -> Zero
  One -> Four
  Two -> Three
  Three -> Two
  Four -> One

data Vector a = Vec [a] deriving (Show, Eq)

add :: (Field a) => Vector a -> Vector a -> Vector a
add (Vec a) (Vec b) = Vec $ zipWith (+) a b

dotProd :: (Field a) => Vector a -> Vector a -> a
dotProd (Vec a) (Vec b) = foldr (+) zero $ zipWith (*) a b

scalProd :: (Field a) => a -> Vector a -> Vector a
scalProd a (Vec v) = Vec $ map (*a) v

type Matrix a = [Vector a]

act :: (Field a) => Matrix a -> Vector a -> Vector a
act mat (Vec v) = foldr add (Vec $ map (const zero) v) $ zipWith scalProd v mat

mult :: (Field a) => Matrix a -> Matrix a -> Matrix a
mult a b = map (a `act`) b

allElem = enumFrom Zero

identity :: (Field a) => Int -> Matrix a
identity n = map (\k -> Vec $ map (\i -> if i==k then one else zero) [0..n-1]) [0..n-1]

order :: (Field a) => Matrix a -> Int
order mat = succ $ head.init $ elemIndices (identity $ length mat) (iterate (mat `mult`) mat)



prime_factors n =
  case factors of
   [] -> [n]
   _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
