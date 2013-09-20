{- Calculator -}
module Calc where

type Parser a = String -> [(a, String)]

{-
Grammar:
  * Term = Product '+' Product
  * Product = Expression '*' Expression
  * Expression = '(' Term ')' | Val
  * Val = Float
-}
data Element = Term 
             | Product
             | Expression
             | Val

parser :: Parser Char
parser cs = case cs of
    [] -> []
    (x:xs) -> [(x, xs)]

failure :: Parser a
failure = \inp -> []

success :: a -> Parser a
success v = \inp -> [(v, inp)]
