{--
    Smallest free number 
--}
module Pearls.SFN where

import Data.List

minfree :: [Int] -> Int
minfree = minfree2 0


minfree2 :: Int -> [Int] -> Int
minfree2 a [] = a
--minfree2 a [b] = if a < b then a else a+1
minfree2 a xs = if length us == (b-a) then minfree2 b vs else minfree2 a us 
    where b = a + (length xs + 1) `div` 2
          (us, vs) = partition (b >) xs         


-- ----------------------------------------------------------------------------
-- Demo
-- ----------------------------------------------------------------------------

demo1 :: Int
demo1 = minfree [8, 23, 9, 0, 12, 11, 1, 10, 13, 7, 41, 4, 14, 21, 5, 17, 3, 19, 2, 6]
