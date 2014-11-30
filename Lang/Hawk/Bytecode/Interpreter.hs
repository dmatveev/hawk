module Lang.Hawk.Bytecode.Interpreter where

import Data.IORef
import Data.Fixed (mod')
import Control.Exception
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as B

import Lang.Hawk.Basic
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Interpreter
import Lang.Hawk.Runtime


pop   :: Interpreter (Value, [Value])
pop_  :: Interpreter Value
pop2  :: Interpreter (Value, Value, [Value])
pop2_ :: Interpreter (Value, Value)
popN  :: Int -> Interpreter ([Value], [Value])
popN_ :: Int -> Interpreter [Value]
push  :: Value -> [Value] -> Interpreter ()
push_ :: Value -> Interpreter ()
stack :: Interpreter [Value]
{-# INLINE pop   #-}
{-# INLINE pop_  #-}
{-# INLINE pop2  #-}
{-# INLINE pop2_ #-}
{-# INLINE popN  #-}
{-# INLINE popN_ #-}
{-# INLINE push  #-}
{-# INLINE push_ #-}
{-# INLINE stack #-}

pop       = stack  >>= \(top:st) -> return (top, st)
pop_      = pop    >>= \(top,st) -> modify (\s -> s { hcSTACK = st }) >> return top
pop2      = stack  >>= \(r:l:st) -> return (r,l,st)
pop2_     = pop2   >>= \(r,l,st) -> modify (\s -> s { hcSTACK = st }) >> return (r,l)
popN n    = stack  >>= \st -> return $ splitAt n st
popN_ n   = popN n >>= \(vs,rs) -> modify (\s -> s { hcSTACK = rs }) >> return vs
push v st = modify $ \s -> s { hcSTACK = seq v v:st }
push_ v   = modify $ \s -> s { hcSTACK = seq v v:(hcSTACK s) }
stack     = gets hcSTACK

dbg :: OpCode -> Interpreter ()
{-# INLINE dbg #-}
dbg op = return () -- liftIO $ putStrLn (show op)

bc :: OpCode -> Interpreter ()
bc (ARITH o)       = pop2    >>= \(r,l,st) -> push (calcArith l r o) st
bc (PUSH v)        = push_   v
bc POP             = pop_    >>  return ()
bc FIELD           = pop     >>= \(top,st) -> fref top >>= flip push st
bc FSET            = pop2_   >>= \(i,v) -> assignToField Set i v
bc (FMOD o)        = pop2_   >>= \(i,v) -> assignToField o   i v
bc (VAR r)         = push_   =<< (liftIO $! readIORef r)
bc (VSET r)        = pop_    >>= \top -> liftIO $ writeIORef r top
bc (VMOD o r)      = pop_    >>= \top -> liftIO $ modifyIORef' r (\v -> calcArith v top o)
bc (BVAR b)        = push_   =<< evalBVariableRef b
bc (BSET b)        = pop_    >>= \top -> modBVar b (const top)
bc (BMOD o b)      = pop_    >>= \top -> modBVar b (\v -> calcArith v top o)
bc (ARR r)         = pop     >>= \(idx,st)   -> aref r idx >>= flip push st
bc (ASET r)        = pop2    >>= \(idx,v,st) -> aset r idx v >> push v st
bc (AMOD o r)      = pop2    >>= \(idx,v,st) -> amod r idx v o >>= flip push st
bc (CMP o)         = pop2    >>= \(rv,lv,st) -> push (cmpValues lv rv o) st
bc (LGC o)         = pop2    >>= \(rv,lv,st) -> push (calcLogic o lv rv) st
bc NOT             = pop     >>= \(top,st)   -> push (vNot top) st
bc NEG             = pop     >>= \(top,st)   -> push (vNeg top) st
bc (CALL "length") = pop     >>= \(top,st)   -> push (calcLength top) st
bc DUP             = stack   >>= \st@(top:_) -> push top st
bc (PRN n)         = popN_ n >>= prn
bc MATCH           = pop2    >>= \(rv,lv,st) -> push (match lv rv) st
bc (ADEL r)        = pop_    >>= \idx        -> adel r idx
bc (ADRP r)        = adrp r 
bc DRP             = modify $ \s -> s { hcSTACK = [] }
bc op              = liftIO $ putStrLn $ "UNKNOWN COMMAND " ++ show op
execBC :: [OpCode] -> Interpreter () 
execBC []             = return ()
execBC (op@(JF  n):r) = dbg op >> pop_    >>= \top -> if toBool top then execBC r else jmp n
execBC (op@(JMP n):r) = dbg op >> jmp n
execBC (op:ops)       = dbg op >> bc op >> execBC ops 

key :: Value -> String
{-# INLINE key #-}
key = B.unpack . toString

aref :: IORef Array -> Value -> Interpreter Value
aref r i = liftIO $ readIORef r >>= \arr -> return $! arr *! (key i)

aset :: IORef Array -> Value -> Value -> Interpreter ()
aset r i v = liftIO $ modifyIORef' r $ \arr -> M.insert (key i) v arr

amod :: IORef Array -> Value -> Value -> ArithOp -> Interpreter Value
amod r i v o = liftIO $ do
   arr <- liftIO $ readIORef r
   let idx      = key i
       newValue = calcArith (arr *! idx) v o
       newArray = M.insert idx newValue arr
   liftIO $ writeIORef r (seq newArray newArray)
   return $! newValue

adel :: IORef Array -> Value -> Interpreter ()
adel r i = liftIO $ modifyIORef' r $ \arr -> M.delete (key i) arr

adrp :: IORef Array -> Interpreter ()
adrp r = liftIO $ writeIORef r M.empty

fref :: Value -> Interpreter Value
fref v = do
   let i = toInt v 
   if i == 0
   then gets hcThisLine >>= (return . valstr)
   else liftM (*!! i) $ gets hcFields

jmp :: Int -> Interpreter ()
jmp n = gets hcOPCODES >>= \src -> execBC (drop n src)

prn [] =  gets hcThisLine >>= (liftIO . B.putStrLn)
prn vs = do
   ofs <- liftM toString $ gets hcOFS
   ors <- liftM toString $ gets hcORS
   let str = B.intercalate ofs $ map toString $ reverse vs
   liftIO $ B.putStr $ B.append str ors
