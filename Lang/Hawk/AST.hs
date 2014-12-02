module Lang.Hawk.AST where

import Lang.Hawk.Basic

import Data.IORef
import Lang.Hawk.Value

data Literal = LitNumeric Double
             | LitStr String
             | LitRE String
               deriving (Eq, Show)

instance Show (IORef a) where
   show _ = "<ioref>"

data Expression = Arith ArithOp Expression Expression
                | Const Literal
                | FieldRef Expression
                | VariableRef String
                | Variable (IORef Value) 
                | ArrayRef String Expression
                | Array (IORef Array) Expression
                | Array' (IORef Array)
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
                | In' Expression (IORef Array)
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
               | FOREACH' (IORef Value) (IORef Array) Statement
               | DO Statement Expression
               | PRINT [Expression]
               | BREAK
               | CONT
               | NEXT
               | EXIT (Maybe Expression)
               | NOP
               | DELETE Expression
               | DELARR (IORef Array)
               | DELELM (IORef Array) Expression
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
