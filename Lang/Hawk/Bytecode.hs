module Lang.Hawk.Bytecode where

import Data.IORef
import Data.Sequence

import Lang.Hawk.Basic
import Lang.Hawk.Value

data OpCode = ARITH ArithOp
            | PUSH Value
            | POP
            | FIELD
            | FSET
            | FMOD ArithOp
            | VAR (IORef Value)
            | VSET (IORef Value)
            | VMOD ArithOp (IORef Value)
            | ARR String
            | ASET String
            | AMOD ArithOp String
            | BVAR BVar
            | BSET BVar
            | BMOD ArithOp BVar
            | CMP CmpOp
            | NOT
            | NEG
            | LGC LogOp
            | MATCH
            | CALL String

            | DUP
            | NOOP
            | PRN Int
            | JMP Int
            | JF Int
            | DRP
            deriving (Show)

type Bytecode = Seq OpCode

instance Show (IORef a) where
   show _ = "<ioref>"
