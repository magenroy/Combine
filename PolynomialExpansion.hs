module PolynomialExpansion where

import Combinations

polyExp :: Int -> Int -> [[Int]]
polyExp terms exp = combine $ replicate exp [1..terms]

simplex :: (Ord n, Enum n, Num n) => n -> [[Int]]
simplex = struct sum . repeat . enumFromTo 1
simplex' = struct' sum . repeat . enumFromTo 1
