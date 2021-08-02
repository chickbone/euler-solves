{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Tree (Tree, memofix) where

import Control.Applicative
import Control.Lens
import Data.Bits

data Tree a = Tree
  { _vertex :: a,
    _leftT :: Tree a,
    _rightT :: Tree a
  }
  deriving (Show, Functor)

makeLenses ''Tree

instance Applicative Tree where
  pure a = genTree (const a)
  liftA2 op (Tree c1 l1 r1) (Tree c2 l2 r2) = Tree (c1 `op` c2) (liftA2 op l1 l2) (liftA2 op r1 r2)

(!!!) :: Tree a -> Integer -> a
t !!! 0 = t ^. vertex
t !!! n =
  if n .&. 1 == 1
    then (t ^. leftT) !!! top
    else (t ^. rightT) !!! (top -1)
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
