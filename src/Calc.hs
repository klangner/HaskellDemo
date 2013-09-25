module Calc where

import Data.Char
import Control.Monad


-- To samo co:
-- data Parser a = P (String -> [(a, String)])
newtype Parser a = P (String -> [(a, String)])

-- Sequencing parser
instance Monad Parser where
    return v = P (\inp -> [(v, inp)])
    p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v,rest)] -> parse (f v) rest)

instance MonadPlus Parser where
    mzero =  P (\inp -> [])
    p `mplus` q =  P (\inp -> case parse p inp of
                               []        -> parse q inp
                               [(v,out)] -> [(v,out)])

-- Apply parser
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp


-- Always fail
failure :: Parser a
failure = mzero


-- Parse single character
item :: Parser Char
item = P (\inp -> case inp of
                [] -> []
                (x:xs) -> [(x, xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower =  sat isLower
upper :: Parser Char
upper =  sat isUpper
letter :: Parser Char
letter =  sat isAlpha
alphanum :: Parser Char
alphanum =  sat isAlphaNum
char :: Char -> Parser Char
char x =  sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs) 

-- Apply parser 0 or more times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- Apply at least once
many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

-- Identifier
ident :: Parser String
ident = do v  <- lower
           vs <- many alphanum
           return (v:vs)

-- Natural numbers
nat :: Parser Int
nat = do 
        do (char '-')
           vs <- many1 digit
           return (- (read vs))
           +++ do vs <- many1 digit
                  return (read vs)

space :: Parser () -- Empty tuple
space = do vs <- many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Strip spaces
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

p :: Parser [Int]
p = do symbol "["
       n <- natural
       ns <- many (do symbol ","
                      natural)
       symbol "]"
       return (n:ns)  

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t+e)
             +++ do symbol "-"
                    e <- expr
                    return (t-e)
                    +++ return t

term :: Parser Int
term = do f <- pow
          do symbol "*"
             t <- term
             return (f*t)
             +++ do symbol "/"
                    t <- term
                    return (div f t)
                    +++ return f

pow :: Parser Int
pow = do b <- factor
         symbol "^"
         e <- factor
         return (b^e)
         +++ factor

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(v, [])] -> v
            [(_, out)] -> error ("Not consumed: " ++ out)
            [] -> error "invalid input"
