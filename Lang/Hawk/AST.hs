module Lang.Hawk.AST where

type Op = String

data Literal = LitNumeric Integer
             | LitStr String
             | LitRE String
             deriving (Eq, Show)

data Compound = Combine String Pattern Pattern
              | Negate Pattern
              deriving (Eq, Show)

data Notation = Pre
              | Post
              deriving (Eq, Show)

data Expression = Arith Op Expression Expression
                | Const Literal
                | FieldRef Expression
                | VariableRef String
                | BuiltInVar String
                | Assignment Op Expression Expression
                | Incr Notation Expression
                | Decr Notation Expression
                | Relation Op Expression Expression
                | FunCall String [Expression]
                | Not Expression
                | Neg Expression
                | Concat Expression Expression
                | In Expression Expression
                | Logic Op Expression Expression
                | Match Expression Expression
                | NoMatch Expression Expression
                deriving (Eq, Show)

data Statement = Expression Expression
               | Block [Statement]
               | IF Expression Statement (Maybe Statement)
               | WHILE Expression Statement
               | FOR (Maybe Expression) (Maybe Expression) (Maybe Expression)
               | DO Statement Expression
               | PRINT [Expression]
               | BREAK
               | CONT
               | NEXT
               | EXIT (Maybe Expression)
                 deriving (Eq, Show)

data Pattern = BEGIN
             | END
             | EXPR Expression
             | RE String
             | RANGE Pattern Pattern
             deriving (Eq, Show)

data Section = Section (Maybe Pattern) (Maybe Statement)
             deriving (Eq, Show)
