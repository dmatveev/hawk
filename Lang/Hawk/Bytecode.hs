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
            | VAR (IORef Value)
            | MVAR ModOp (IORef Value)
            | VSET (IORef Value)
            | ARR String
            | ASET String
            | BVAR BVar
            | BSET BVar
            | CMP CmpOp
            | NOT
            | NEG
            | LOG LogOp
            | MATCH
            | NOMATCH
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
