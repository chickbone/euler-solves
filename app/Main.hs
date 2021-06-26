module Main where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Function
import Data.Maybe
import Data.Monoid

main :: IO ()
main = print $ ans14 1

ans1 :: Int -> Int
ans1 max = fromMaybe 0 $ max <$ guard (max `mod` 3 == 0 || max `mod` 5 == 0)

ans2 :: Int -> Int
ans2 x = undefined

ans5 x = undefined

ans14 :: Int -> Int
ans14 x = fix trans
  where
    trans :: Int -> Int
    trans n
      | n == 1 = 1
      | n .&. 1 == 0 = n `shiftL` 1
      | n .&. 1 == 1 = 3 * n + 1

fib a b = a : b : zipWith (+) (fib a b) (tail $ fib a b)
