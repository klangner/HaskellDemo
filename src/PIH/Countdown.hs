module PIH.Countdown where

import Data.List


data Op = Plus | Minus | Multi | Div deriving( Show )
data Expr = Val Int | App Op Expr Expr deriving( Show )
          
type Result = (Expr, Int)          
          

-- Solve countdown game
solve :: [Int] -> Int -> [Result]
solve xs n = filter p $ concatMap build (permute xs)
    where p (_,v) = v == n

-- Check if given expression is valid
valid :: Op -> Int -> Int -> Bool
valid Plus a b  = a > 0 && b > 0 && a >= b
valid  Minus a b = a > b 
valid  Multi a b = a > 1 && b > 1 && a >= b
valid Div a b = a `mod` b == 0 && b > 1

-- Apply operation
apply :: Op -> Int -> Int -> Int
apply Plus a b  = a+b
apply  Minus a b = a - b 
apply  Multi a b = a*b
apply Div a b = a `div` b           


-- Combine 2 expressions
combine :: Result -> Result ->[Result]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- [Plus, Minus, Multi, Div], valid op v1 v2 ]


-- Build all possible expressions from given numbers without changing order
build :: [Int] -> [Result]
build [] = []
build [x] = [(Val x, x)]
build xs = concatMap f (split xs)
    where f (a, b) = concat [ combine exp1 exp2 | exp1 <- build a, exp2 <- build b]

-- get all permutation with all lengths
permute :: [a] -> [[a]]
permute xs = concatMap permutations (subsequences xs)

-- Split array at any position
split :: [Int] -> [([Int],[Int])]
split [x] = [([x],[])]
split xs = map (\x -> splitAt x xs) [1 .. (length xs - 1)]

-- ----------------------------------------------------------------------------
-- Demo
-- ----------------------------------------------------------------------------

-- 780 solutions
demo1 :: IO ()
demo1 = do
    let solution = solve [1,3,7,10,25,50] 765
    print (length solution)