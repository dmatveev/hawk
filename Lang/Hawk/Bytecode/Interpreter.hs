module Lang.Hawk.Bytecode.Interpreter where

import Data.IORef

import Control.Exception

import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as B

import Lang.Hawk.Basic
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Interpreter
import Lang.Hawk.Runtime

import Data.Fixed (mod')

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

bc :: OpCode -> Interpreter ()
bc (ARITH o)       = pop2    >>= \(r,l,st) -> push (calcArith l r o) st
bc (MVAR m r)      = pop_    >>= \t -> liftIO $ modifyIORef' r $ \v -> calcNewValue v m t
bc (PUSH v)        = push_   v
bc POP             = pop_    >>  return ()
bc FIELD           = pop     >>= \(top,st) -> fref top >>= flip push st
bc FSET            = pop2_   >>= \(i,v) -> assignToField ModSet i v
bc (VAR r)         = push_   =<< (liftIO $! readIORef r)
bc (VSET r)        = pop_    >>= \top -> liftIO $ writeIORef r top
bc (BVAR b)        = push_   =<< evalBVariableRef b
bc (BSET b)        = pop_    >>= \top -> modBVar b (\old -> calcNewValue old ModSet top)
bc (CMP o)         = pop2    >>= \(rv,lv,st) -> push (cmpValues lv rv o) st
bc (LGC o)         = pop2    >>= \(rv,lv,st) -> push (calcLogic o lv rv) st
bc (CALL "length") = pop     >>= \(top,st)   -> push (calcLength top) st
bc DUP             = stack   >>= \st@(top:_) -> push top st
bc (PRN n)         = popN_ n >>= prn
bc DRP             = modify $ \s -> s { hcSTACK = [] }

execBC :: [OpCode] -> Interpreter () 
execBC []          = return ()
execBC ((JF n):r)  = do (top:st) <- gets hcSTACK
                        modify $ \s -> s {hcSTACK = st}
                        if toBool top then execBC r else jmp n
execBC ((JMP n):r) = jmp n
execBC (op:ops)    = do -- liftIO $ putStrLn $ (show op)
                        bc op
                        execBC ops 

fref :: Value -> Interpreter Value
fref v = do
   let i = toInt v 
   if i == 0
   then gets hcThisLine >>= (return . valstr)
   else do fs <- gets hcFields
           return $! fs IM.! i

jmp :: Int -> Interpreter ()
jmp n = gets hcOPCODES >>= \src -> execBC (drop n src)

prn [] =  gets hcThisLine >>= (liftIO . B.putStrLn)
prn vs = do
   ofs <- liftM toString $ gets hcOFS
   ors <- liftM toString $ gets hcORS
   let str = B.intercalate ofs $ map toString $ reverse vs
   liftIO $ B.putStr $ B.append str ors
