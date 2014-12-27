module Lang.Hawk.AST where

import Lang.Hawk.Basic

import Lang.Hawk.Value

data Expression = Arith ArithOp Expression Expression
                | Const Literal
                | FieldRef Expression
                | VariableRef String
                | ArrayRef String Expression
                | BuiltInVar BVar
                | Assignment ArithOp Expression Expression
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
                | FunCall BFunc [Expression]
                | InlineIf Expression Expression Expression
                | Getline
                | GetlineVar Expression
                | FGetline Expression
                | FGetlineVar Expression Expression
                | PGetline Expression
                | PGetlineVar Expression Expression
                  deriving (Eq, Show)

data Statement = Expression Expression
               | Block [Statement]
               | IF Expression Statement (Maybe Statement)
               | WHILE Expression Statement
               | FOR (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | FOREACH Expression String Statement
               | DO Statement Expression
               | PRINT [Expression]
               | FPRINT [Expression] FileMod Expression
               | PPRINT [Expression] Expression
               | BREAK
               | CONT
               | NEXT
               | EXIT (Maybe Expression)
               | NOP
               | DELETE Expression
               | DELARR String
               | DELELM String Expression
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

isBegin (Section (Just BEGIN) _) = True
isBegin _                        = False

isEnd (Section (Just END) _) = True
isEnd _                      = False
