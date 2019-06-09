module RuleEngine.Parser where

{--
 Grammar Syntax:
 String : '"' Character* '"'
 NumericRelation : ">" | "<" | "="
 Variable : letter letterOrDigit*
 NumericTerm : Variable NumericRelation Number
 StringTerm : Variable "=" String
 InTerm : Variable "in" "{" String ("," String)* "}"
 Term : (NumericTerm | StringTerm | InTerm)+
 Rule : Term ("," Term)*

 Semantics:
 amount : Number
 location : String

 CASL spec
 spec RULE_ENGINE
    sort String
    sort Rule

    op parse: String -> Rule
 end

--}

-- Utils definitions
data Name = String

-- Define rule syntax
data Str = Str String
data NumericRelation = GreaterThen
                     | LessThen
                     |EqualsTo
data Identifier = Var String
data Term = NumTerm Identifier NumericRelation Float
          | StrTerm Identifier String
          | InTerm Identifier [String]
data Rule = Rule Name [Term]

-- Define rule semantics
data Variable = StrVar Name
              | NumVar Name

-- Define data record
data Field = NumericField Name Float
           | StrField Name String
data Record = Record [Field]

-- Rule parser
parseRule :: String -> Either String Rule

-- Check semantics
checkSemantics :: Rule -> [Variable] -> Bool

-- Check single rule. True if record passes rule
checkRule :: Rule -> Record -> Bool

-- Return all rules which given record passes
applyAllRules :: [Rule] -> Record -> [Rule]


parseRule s = Left "Not defined"

