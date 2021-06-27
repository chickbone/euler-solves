module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Bits
import Data.Maybe
import Data.Monoid
import Debug.Trace

ans1 :: Int -> Int
ans1 max = fromMaybe 0 $ max <$ guard (max `mod` 3 == 0 || max `mod` 5 == 0)

ans2 :: Int -> Int
ans2 x = undefined

ans4 :: [Int] -> [Int]
ans4 xs = do
  x <- xs
  y <- xs
  let xy = x * y
  guard (xy == revInt xy)
  return xy
  where
    revInt = read . reverse . show :: (Int -> Int)

-- | Sum square difference
ans6 :: Int -> Int
ans6 x = sumId x ^ 2 - sumSquared x
  where
    sumSquared a = (a * (a + 1) * (a * 2 + 1)) `div` 6
    sumId a = (a * (a + 1)) `div` 2

-- | 10001st prime
ans7 :: Int -> [Int]
ans7 = const []

-- | Special Pythagorean triplet
-- Pythagorean triplet : a^2 + b^2 = c^2
-- a = (n^2 - m^2) ,b = (4n^2m^2) ,c = (n^2 + m^2)
-- a + b + c = (4m^2+2)n^2
ans9 :: [Int]
ans9 = do
  n <- [1 .. 32]
  m <- [1 .. 32]
  guard (n > m)
  let a = n ^ 2 - m ^ 2
  let b = 2 * n * m
  let c = n ^ 2 + m ^ 2
  guard $ a + b + c == 1000
  return $ a * b * c

ans14 :: Int -> Int
ans14 x = fix trans
  where
    trans :: Int -> Int
    trans n
      | n == 1 = 1
      | n .&. 1 == 0 = n `shiftL` 1
      | n .&. 1 == 1 = 3 * n + 1

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

sum' :: (Num a) => [a] -> a
sum' = foldr (\x -> ($!) (+ x)) 0

fib a b = a : b : zipWith (+) (fib a b) (tail $ fib a b)

data Tree a = Tree a (Tree a) (Tree a) deriving (Show)

vertex (Tree c _ _) = c

left (Tree _ l _) = l

right (Tree _ _ r) = r

instance Functor Tree where
  fmap f (Tree c l r) = Tree (f c) (fmap f l) (fmap f r)

instance Applicative Tree where
  pure a = genTree (const a)
  liftA2 op (Tree c1 l1 r1) (Tree c2 l2 r2) = Tree (c1 `op` c2) (liftA2 op l1 l2) (liftA2 op r1 r2)

Tree c _ _ !!! 0 = c
Tree _ tl tr !!! n =
  if n .&. 1 == 0
    then tl !!! top
    else tr !!! (top -1)
  where
    top = n `shiftR` 1

genTree :: (Integer -> b) -> Tree b
genTree f = gen 0
  where
    gen n = Tree (f n) (gen $ n `shiftL` 1 + 1) (gen $ n `shiftL` 1 + 2)

memofix :: Show b => ((Integer -> b) -> (Integer -> b)) -> (Integer -> b)
memofix f = memof
  where
    memof = f (tdl !!!)
    tdl = genTree memof
