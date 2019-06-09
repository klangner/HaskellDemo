module Algorithms.Sorting where

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = mergeSorted (msort kx) (msort lx)
    where b = (length xs) `div` 2
          (kx, lx) = splitAt b xs

-- | join 2 sorted array in one also sorted
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted xs ys | null xs = ys
                  | null ys = xs
                  | (head xs) < (head ys) = (head xs: mergeSorted (tail xs) ys)
                  | otherwise = (head ys: mergeSorted xs (tail ys)) 

