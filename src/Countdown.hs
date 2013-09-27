module Countdown where

data Op = Add | Sub | Mul | Div deriving (Show)

valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x >= y && x /= 1 && y /= 1
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr deriving (Show)

test = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
test2 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 100))
numbers :: [Int]
numbers = [1,3,7,10,25,50]

values :: Expr -> [Int]
values (Val x) = [x]
values (App _ x y) = values x ++ values y

eval :: Expr -> [Int]
eval (Val x) = [x | x > 0]
eval (App o x y) = [apply o a b |
                        a <- eval x,
                        b <- eval y,
                        valid o a b]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave n [] = [[n]]
interleave n (x:xs) = (n:x:xs):(map (x:) (interleave n xs))

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e vs n = eval e == [n] && elem (values e) (choices vs)

-- Create all possible splits
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:y,rest) | (y, rest) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine x y = [App o x y | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions vs n = [e | cs <- choices vs,
                      e <- exprs cs,
                      eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [r | (ls, rs) <- split ns,
                  x <- results ls,
                  y <- results rs,
                  r <- combine' x y]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y ]
                             
solutions' :: [Int] -> Int -> [Expr]
solutions' vs n = [e | cs <- choices vs,
                       (e, m) <- results cs,
                       m == n]

