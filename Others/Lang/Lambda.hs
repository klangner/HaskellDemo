module Lang.Lambda where

import Prelude hiding (id)

type Name = String
data Expression = ExpName Name
                | Fun Name Expression
                | App Expression Expression
                deriving (Show)


id :: Expression
id = Fun "x" (ExpName "x")


run :: Expression -> Expression
run (App (Fun x e1) e2) = run (apply x e1 e2)
run x = x


-- Replace variable in expression with given argument
apply :: Name -> Expression -> Expression -> Expression
apply x (ExpName n) arg = if x == n then arg else ExpName n
apply x (Fun a b) arg = Fun a (apply x b arg)
apply x (App a b) arg = App (apply x a arg) (apply x b arg)