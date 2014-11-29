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

bc :: OpCode -> Interpreter ()
bc (ARITH o)       = do (rhs:lhs:st) <- gets hcSTACK
                        let v = calcArith lhs rhs o
                        modify $ \s -> s {hcSTACK = seq v (v:st)}
bc (MVAR m r)      = do (top:st) <- gets hcSTACK
                        liftIO $ modifyIORef' r $ \v -> calcNewValue v m top
                        modify $ \s -> s {hcSTACK = st}
bc (PUSH v)        = modify $ \s -> s {hcSTACK = v:(hcSTACK s)   }
bc POP             = modify $ \s -> s {hcSTACK = tail(hcSTACK s) }
bc FIELD           = do (top:st) <- gets hcSTACK
                        f <- fref top
                        modify $ \s -> s {hcSTACK = f:st}
bc FSET            = do (i:v:st) <- gets hcSTACK
                        assignToField ModSet i v
                        modify $ \s -> s {hcSTACK = st}
bc (VAR r)         = do v <- liftIO $! readIORef r
                        modify $ \s -> s {hcSTACK = v:(hcSTACK s) }
bc (VSET r)        = do (top:st) <- gets hcSTACK
                        liftIO $ writeIORef r top
                        modify $ \s -> s {hcSTACK = st}
bc (BVAR b)        = do v <- evalBVariableRef b
                        modify $ \s -> s {hcSTACK = v:(hcSTACK s) }
bc (BSET b)        = do (top:st) <- gets hcSTACK
                        modBVar b (\oldVal -> calcNewValue oldVal ModSet top)
                        modify $ \s -> s {hcSTACK = st}
bc (CMP o)         = do (rv:lv:st) <- gets hcSTACK
                        let v = cmpValues lv rv o
                        modify $ \s -> s {hcSTACK = v:st }
bc (LGC o)         = do (rv:lv:st) <- gets hcSTACK
                        let v = calcLogic o lv rv
                        modify $ \s -> s {hcSTACK = v:st }
bc (CALL "length") = do (top:st) <- gets hcSTACK
                        let v = VDouble $! fromIntegral $ B.length $ toString top
                        modify $ \s -> s {hcSTACK = v:st}
bc DUP             = do st@(top:_) <- gets hcSTACK
                        modify $ \s -> s {hcSTACK = top:st}
bc (PRN n)         = do (vs,rs) <- liftM (splitAt n) $ gets hcSTACK
                        prn vs
                        modify $ \s -> s {hcSTACK = rs}
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
