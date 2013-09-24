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

-- Parse single character
item :: Parser Char
item cs = case cs of
                [] -> []
                (x:xs) -> [(x, xs)]

-- Always fail
failure :: Parser a
failure = \inp -> []

-- Return v without consuming input
return :: a -> Parser a
return v = \inp -> [(v, inp)]

-- Sequencing parser
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                        [] -> []
                        [(v,rest)] -> parse (f v) rest

-- Return first if parsed input
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
            [] -> q inp
            [(c, rest)] -> [(c, rest)]

-- Apply parser
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

--p :: Parser (Char,Char)
p = do x <- item
       z <- item
       y <- item
       return (x, y)


p1 = do x <- item
        return x

p2 = do x <- item
        return2 x

p3 = item
     

