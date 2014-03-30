module PIH.Countdown where

import Control.Applicative

data Expr = Num Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Multi Expr Expr
          | Div Expr Expr
          
instance Show Expr where
    show (Num value) = show value
    show (Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Minus a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Multi a b) = show a ++ "*" ++ show b
    show (Div a b) = show a ++ "/" ++ show b           


-- Solve countdown game
solve :: [Int] -> Int -> [Expr]
solve _ _ = []


-- Evaluate data. 
-- At every step the result has to be positive natural number
eval :: Expr -> [Int]
eval (Num value) = pure value
eval (Plus a b) = (+) <$> (eval a) <*> (eval b)
eval (Minus a b) = case (-) <$> (eval a) <*> (eval b) of
                    [x] -> if x >= 0 then pure x else []
                    _ -> []
eval (Multi a b) = (*) <$> (eval a) <*> (eval b)
eval (Div a b) = case mod <$> x <*> y of           
                    [0] -> div <$> x <*> y
                    _ -> []
    where x = eval a
          y = eval b


-- ----------------------------------------------------------------------------
-- Demo
-- ----------------------------------------------------------------------------

demo1 :: IO ()
demo1 = do
    let solution = solve [1,3,7,10,25,50] 765
    print solution