{--
    Pearl 2. Maximum surpassing count
--}
module Pearls.Surpass where

import Data.List

-- Divide and conquer
msc2 :: Ord a => [a] -> Int
msc2 [] = 0
msc2 [_] = 0
msc2 xs = 0
    where b = (length xs) `div` 2
          (us, vs) = splitAt b xs
          
                   
join :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join xs ys = xs ++ ys



table :: Ord a => [a] -> [(a, Int)]
table xs = [(y, scount y ys) | (y:ys) <- tails xs] 


-- Naive implementation with complexity O(n^2)
msc :: Ord a => [a] -> Int
msc xc = maximum (asc xc)


-- surpassing count for all list elements. complexity O(n^2)
asc :: Ord a => [a] -> [Int]
asc [] = []
asc (x:xs) = (scount x xs):asc xs 

  
-- surpassing count for first array element Complexity O(n)
scount :: Ord a => a -> [a] -> Int
scount _ [] = 0
scount x ys = length $ filter (x <) ys


-- ----------------------------------------------------------------------------
-- Demo
-- ----------------------------------------------------------------------------

demo1 :: Bool
demo1 = msc "generating" == 6
