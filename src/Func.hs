module Func where

import Control.Monad
import Data.Bits
import Tree

sum' :: (Num a) => [a] -> a
sum' = foldr (\x -> ($!) (+ x)) 0

memofib = memofix $ \f n -> if n < 2 then n else f (n -1) + f (n -2)

frac = memofix $ \f n -> if n == 1 then 1 else n * f (n -1)

perm n m = frac n `div` frac m

comb n m = perm n m `div` frac (n - m)

isPrime n = do
  guard $ n `mod` 6 /= 1 && n `mod` 6 /= 5
  return n

modPow _ 1 _ = 1
modPow b ex l = (`mod` l) $ next * next * (if isOdd then base else 1)
  where
    base = b `mod` l
    next = modPow base (ex `shiftR` 1) l
    isOdd = ex .&. 1 == 1
