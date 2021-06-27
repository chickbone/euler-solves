module Main where

import Lib

main :: IO ()
main = print $ map memofib [(0 :: Integer) .. 100]

memofib = memofix $ \f n -> if n < 2 then n else f (n -1) + f (n -2)
