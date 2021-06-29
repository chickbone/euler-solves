{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Galois where

import GHC.TypeLits

newtype ModRing (m :: Nat) = MR {getMR :: Integer} deriving (Show, Eq, Ord)

instance KnownNat n => Num (ModRing n) where
  (+) t@(MR x) (MR y) = MR ((x + y) `mod` natVal t)
  (*) t@(MR x) (MR y) = MR ((x * y) `mod` natVal t)
  abs t@(MR x) = MR $ x `mod` natVal t
  negate t@(MR x) = MR $ (- x) `mod` natVal t
  signum x = MR $ if abs x == 0 then 0 else 1
  fromInteger x = MR x
