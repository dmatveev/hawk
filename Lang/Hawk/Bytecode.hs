module Lang.Hawk.Bytecode where

import Data.IORef
import qualified Data.ByteString.Char8 as B
import Lang.Hawk.Basic
import Lang.Hawk.Value

data OpCode = ARITH !ArithOp
            | PUSH  !Value
            | POP   
            | FIELD                | FSET                 | FMOD  !ArithOp
            | VAR   !(IORef Value) | VSET  !(IORef Value) | VMOD  !ArithOp !(IORef Value)
            | VAR'  !String        | VSET' !String        | VMOD' !ArithOp !String
            | ARR   !(IORef Array) | ASET  !(IORef Array) | AMOD  !ArithOp !(IORef Array)
            | ARR'  !String        | ASET' !String        | AMOD' !ArithOp !String
            | BVAR  !BVar          | BSET  !BVar          | BMOD  !ArithOp !BVar
            | CMP   !CmpOp
            | NOT   
            | NEG   
            | LGC   !LogOp
            | ANXT  !(IORef Value) | ANXT' !String
            | ACHK
            | FETCH !(IORef Array) | FETCH' !String
            | KDRP
            | MATCH 
            | CALL  !BFunc !Int
            | IN    !(IORef Array) | IN'   !String
            | ADEL  !(IORef Array) | ADEL' !String
            | ADRP  !(IORef Array) | ADRP' !String
            | DUP
            | NOOP
            | PRN   !Int
            | FPRN  !Int !FileMod
            | PPRN  !Int
            | JMP   !Int
            | JF    !Int
            | JT    !Int
            | DRP
            | SPLIT !(IORef Array) | SPLIT' !String
            | NXT
            | EX
            | CAT
            | GETL
            | GETLV !(IORef Value) | GETLV' !String
            | FGETL
            | FGETLV !(IORef Value) | FGETLV' !String
            | PGETL
            | PGETLV !(IORef Value) | PGETLV' !String
            deriving (Show)

type ProgCode = ([OpCode],[OpCode],[OpCode])

instance Show (IORef a) where
   show _ = "<ioref>"
