module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Bits
import Data.Maybe
import Data.Monoid
import Debug.Trace
import Func
import Tree

ans1 :: Int -> Int
ans1 max = fromMaybe 0 $ max <$ guard (max `mod` 3 == 0 || max `mod` 5 == 0)

ans2 :: Integer -> Integer
ans2 x = sum' . takeWhile (<= x) $ map memofib [(3 :: Integer), 5 ..]

ans4 :: [Int] -> [Int]
ans4 xs = do
  x <- xs
  y <- xs
  let xy = x * y
  xy <$ guard (xy == revInt xy)
  where
    revInt = read . reverse . show :: (Int -> Int)

ans5 :: Integer -> Integer
ans5 x = foldr lcm 1 [1 .. x]

-- | Sum square difference
ans6 :: Int -> Int
ans6 x = sumId x ^ 2 - sumSquared x
  where
    sumSquared a = a * (a + 1) * (a * 2 + 1) `div` 6
    sumId a = a * (a + 1) `div` 2

-- | 10001st prime
ans7 :: Int -> [Int]
ans7 = const []

-- | Special Pythagorean triplet
-- Pythagorean triplet : a^2 + b^2 = c^2
-- a = (n^2 - m^2) ,b = (4n^2m^2) ,c = (n^2 + m^2)
-- a + b + c = (4m^2+2)n^2
ans9 :: Int -> [Int]
ans9 n = do
  n <- [1 .. 32]
  m <- [1 .. 32]
  guard (n > m)
  let a = n ^ 2 - m ^ 2
  let b = 2 * n * m
  let c = n ^ 2 + m ^ 2
  guard $ a + b + c == n
  pure $ a * b * c

ans14 = fix $ \f y -> (trans y 0, y)
  where
    trans :: Integer -> Int -> Int
    trans x n
      | x == 1 = n
      | x .&. 1 == 0 = trans (x `shiftR` 1) ((+ 1) $! n)
      | otherwise = trans ((3 * x + 1) `shiftR` 1) ((+ 2) $! n)

-- | Lattice paths
ans15 x = (2 * x) `comb` x

-- | Fractional dight sum
ans20 = dightsum . frac

ans30 ex = filter (\x -> x == f x) [2 .. 10 ^ (ex + 1)]
  where
    f = fromIntegral . sum' . map (^ ex) . dights

-- | Dice Game
-- Peter has nine four-sided pyramidal dice, each with faces numbered 1, 2, 3, 4.
-- Colin has six six-sided cubic dice, each with faces numbered 1, 2, 3, 4, 5, 6.
--
-- Peter and Colin roll their dice and compare totals: the highest total wins. The result is a draw if the totals are equal.
--
-- What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to seven decimal places in the form 0.abcdefg
ans205 :: Int -> Int -> Int
ans205 a b = length $ paterns a b

paterns c4 c6 = do
  peters <- replicateM c4 [1 .. 4]
  colins <- replicateM c6 [1 .. 6]
  guard $ sum' peters > sum' colins
  return (peters, colins)
