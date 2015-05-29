module Algorithms.ThreeSum where


count3 :: [Int] -> Int
count3 = length . combine
    
    
-- | Combinations without repetitions
combine :: [Int] -> [Int]
combine xs = {-# SCC "combine" #-} [1 | (x1, t1) <- ht xs, (x2, t2) <- ht t1, x3 <- t2, x1 + x2 == - x3]
        where ht :: [a] -> [(a, [a])]
              ht [] = []
              ht (y:ys) = (y, ys) : ht ys
