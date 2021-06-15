module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Lib

main :: IO ()
main = print $ sum $ ans1 <$> [1 .. 999]

ans1 :: Int -> Int
ans1 max = fromMaybe 0 $ max <$ guard (max `mod` 3 == 0 || max `mod` 5 == 0)
