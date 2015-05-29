module Sandbox where

import Prelude hiding (gcd)


gcd :: Int -> Int -> Int
gcd x y | x > y = gcd (x-y) y
        | y > x = gcd (y-x) x
        | otherwise = x