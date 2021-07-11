module Main where

import Lib

main :: IO ()
main = print $ maximum $ map ans14 [(103 :: Integer), 105 .. 1000000]
