module Konsola where

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then return []
              else do xs <- getLine'
                      return (x:xs)

strlen :: IO Int
strlen = do xs <- getLine
            return (length xs)
