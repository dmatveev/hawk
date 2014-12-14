module Lang.Hawk.Bytecode where

import Data.IORef
import Data.Sequence
import qualified Data.ByteString.Char8 as B
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
            | CALL  BFunc Int
            | IN    (IORef Array)
            | ADEL  (IORef Array)
            | ADRP  (IORef Array)
            | DUP
            | NOOP
            | PRN   Int
            | FPRN  Int FileMod
            | PPRN  Int
            | JMP   Int
            | JF    Int
            | DRP
            | SPLIT (IORef Array)
            | NXT
            | EX
            | GETL
            | GETLV (IORef Value)
            deriving (Show)

type Bytecode = Seq OpCode

instance Show (IORef a) where
   show _ = "<ioref>"
