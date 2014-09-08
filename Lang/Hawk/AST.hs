module Lang.Hawk.AST where

type Op = String

data Literal = LitNumeric Integer
             | LitStr String
             deriving (Eq, Show)

data Expression = Arith Op Expression Expression
               | Const Literal
               | FieldRef Integer
               | VariableRef String
               | BuiltInVar String
               | Assignment Op Expression Expression
               | Relation Op Expression Expression
               | FunCall String [Expression]
               deriving (Eq, Show)

data Pattern = BEGIN
             | END
             | EXPR Expression
             | RE String
             | RANGE Pattern Pattern
             deriving (Eq, Show)

data Section = Section (Maybe Pattern) (Maybe [Expression])
             deriving (Eq, Show)
