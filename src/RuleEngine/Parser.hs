module RuleEngine.Parser where

{--
 Grammar:
 String : '"' Character* '"'
 NumericRelation : ">" | "<" | "="
 Variable : letter letterOrDigit*
 NumericTerm : Variable NumericRelation Number
 StringTerm : Variable "=" String
 InTerm : Variable "in" "{" String ("," String)* "}"
 Term : (NumericTerm | StringTerm | InTerm)+
--}

-- Utils definitions
data Name = String

-- Define rule
data Str = Str String
data NumericRelation = GreaterThen
                     | LessThen
                     |EqualsTo
data Variable = Var String
data Term = NumTerm Variable NumericRelation Float
          | StrTerm Variable String
          | InTerm Variable [String]
data Rule = Rule Name [Term]

-- Define data record
data Field = NumericField Name Float
           | StrField Name String
data Record = Record [Field]

-- Rule parser
parseRule :: String -> Either String Rule

-- Check single rule. True if record passes rule
checkRule :: Rule -> Record -> Bool

-- Return all rules which given record passes
applyAllRules :: [Rule] -> Record -> [Rule]


