module Lang.Hawk.AST where

data ArithOp = Add | Sub | Mul | Div | Mod | Pow
               deriving (Eq, Show)

data ModOp = ModSet | ModAdd | ModSub | ModMul | ModDiv | ModMod | ModPow
             deriving (Eq, Show)

data CmpOp = CmpEQ | CmpNE | CmpGT | CmpGE | CmpLT | CmpLE
             deriving (Eq, Show)

data LogOp = AND | OR
             deriving (Eq, Show)

data Literal = LitNumeric Double
             | LitStr String
             | LitRE String
               deriving (Eq, Show)

data Notation = Pre | Post
                deriving (Eq, Show)

data Expression = Arith ArithOp Expression Expression
                | Const Literal
                | FieldRef Expression
                | VariableRef String
                | ArrayRef String Expression
                | BuiltInVar String
                | Assignment ModOp Expression Expression
                | Incr Notation Expression
                | Decr Notation Expression
                | Relation CmpOp Expression Expression
                | Not Expression
                | Neg Expression
                | Id Expression
                | Concat Expression Expression
                | In Expression Expression
                | Logic LogOp Expression Expression
                | Match Expression Expression
                | NoMatch Expression Expression
                | FunCall String [Expression]
                | InlineIf Expression Expression Expression
                  deriving (Eq, Show)

data Statement = Expression Expression
               | Block [Statement]
               | IF Expression Statement (Maybe Statement)
               | WHILE Expression Statement
               | FOR (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | FOREACH Expression String Statement
               | DO Statement Expression
               | PRINT [Expression]
               | BREAK
               | CONT
               | NEXT
               | EXIT (Maybe Expression)
               | NOP
               | DELETE Expression
               | RETURN (Maybe Expression)
                 deriving (Eq, Show)

data Pattern = BEGIN
             | END
             | EXPR Expression
             | RE String
             | RANGE Pattern Pattern
               deriving (Eq, Show)

data TopLevel = Section (Maybe Pattern) (Maybe Statement)
              | Function String [String] Statement
                deriving (Eq, Show)

type AwkSource = [TopLevel]
