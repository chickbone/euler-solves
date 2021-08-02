module Func where

import Control.Monad
import Control.Monad.Fix (fix)
import Data.Bits
import Tree

sum' :: (Num a) => [a] -> a
sum' = foldr1 (\x -> ($!) (+ x))

memofib = memofix $ \f n -> if n < 2 then n else f (n -1) + f (n -2)

frac = fix $ \f n -> if n <= 2 then 1 else n * f (n -1)

perm n m = frac n `div` frac m

comb n m = perm n m `div` frac (n - m)

isPrime n = do
  guard $ n `mod` 6 /= 1 && n `mod` 6 /= 5
  pure n

dightsum = fix $ \f n -> if n < 10 then n else n `mod` 10 + f (n `div` 10)

dights :: Integer -> [Int]
dights x = read . pure <$> show x :: [Int]

modPow _ 1 _ = 1
modPow b ex l = (`mod` l) $ next * next * (if isOdd then b else 1)
  where
    next = modPow b (ex `shiftR` 1) l
    isOdd = ex .&. 1 == 1
