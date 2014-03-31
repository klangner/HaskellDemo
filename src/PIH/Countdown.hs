module PIH.Countdown where

import Control.Applicative
import Data.List


data Expr = Num Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div Expr Expr
          deriving (Eq)
          
instance Show Expr where
    show (Num value) = show value
    show (Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Multi a b) = show a ++ "*" ++ show b
    show (Div a b) = show a ++ "/" ++ show b           


-- Solve countdown game
solve :: [Int] -> Int -> [Expr]
solve xs n = filter p $ concatMap build (permute xs)
    where p e = eval e == [n]


-- Evaluate data. 
-- At every step the result has to be positive natural number
eval :: Expr -> [Int]
eval (Num value) = pure value
eval (Plus a b) = (+) <$> (eval a) <*> (eval b)
eval (Minus a b) = case (-) <$> (eval a) <*> (eval b) of
                    [x] -> if x >= 0 then pure x else []
                    _ -> []
eval (Multi a b) = (*) <$> (eval a) <*> (eval b)
eval (Div a b) = case mod <$> x <*> y' of           
                    [0] -> div <$> x <*> y'
                    _ -> []
    where x = eval a
          y = eval b
          y' = if y == [0] then [] else y

-- Check if given expression is valid
valid :: Expr -> Bool
valid expr = eval expr /= [] 

-- Combine 2 expressions
combine :: Expr -> Expr ->[Expr]
combine e1 e2 = filter valid [Plus e1 e2, Minus e1 e2, Multi e1 e2, Div e1 e2]
--combine e1 e2 = [Plus e1 e2, Minus e1 e2, Multi e1 e2, Div e1 e2]

-- Build all possible expressions from given numbers without changing order
build :: [Int] -> [Expr]
build [] = []
build [x] = [Num x]
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