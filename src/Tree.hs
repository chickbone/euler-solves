module Tree (Tree, memofix) where

import Control.Applicative
import Data.Bits

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
  if n .&. 1 == 1
    then tl !!! top
    else tr !!! (top -1)
  where
    top = n `shiftR` 1

genTree :: (Integer -> b) -> Tree b
genTree f = fmap f nat
  where
    nat = Tree 0 (fmap ((+ 1) . (`shiftL` 1)) nat) (fmap ((`shiftL` 1) . (+ 1)) nat)

memofix :: ((Integer -> b) -> (Integer -> b)) -> (Integer -> b)
memofix f = memof
  where
    memof = f (tdl !!!)
    tdl = genTree memof
