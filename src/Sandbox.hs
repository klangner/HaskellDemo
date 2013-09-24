{-# LANGUAGE NPlusKPatterns #-}
module Sandbox where

import Control.Monad

type Parser a              =  P (String -> [(a,String)])
