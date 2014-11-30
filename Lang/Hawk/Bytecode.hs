module Lang.Hawk.Bytecode where

import Data.IORef
import Data.Sequence

import Lang.Hawk.Basic
import Lang.Hawk.Value

data OpCode = ARITH ArithOp
            | PUSH  Value
            | POP   
            | FIELD 
            | FSET  
            | FMOD  ArithOp
            | VAR   (IORef Value)
            | VSET  (IORef Value)
            | VMOD  ArithOp (IORef Value)
            | ARR   (IORef Array)
            | ASET  (IORef Array)
            | AMOD  ArithOp (IORef Array)
            | BVAR  BVar
            | BSET  BVar
            | BMOD  ArithOp BVar
            | CMP   CmpOp
            | NOT   
            | NEG   
            | LGC   LogOp
            | ANXT (IORef Value)
            | ACHK
            | FETCH (IORef Array)
            | KDRP
            | MATCH 
            | CALL  String
            | IN    (IORef Array)
            | ADEL  (IORef Array)
            | ADRP  (IORef Array)
            | DUP
            | NOOP
            | PRN   Int
            | JMP   Int
            | JF    Int
            | DRP
            deriving (Show)

type Bytecode = Seq OpCode

instance Show (IORef a) where
   show _ = "<ioref>"
