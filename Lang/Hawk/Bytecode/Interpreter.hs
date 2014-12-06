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
pop3  :: Interpreter (Value, Value, Value, [Value])
popN  :: Int -> Interpreter ([Value], [Value])
popN_ :: Int -> Interpreter [Value]
push  :: Value -> [Value] -> Interpreter ()
push_ :: Value -> Interpreter ()
stack :: Interpreter [Value]
{-# INLINE pop   #-}
{-# INLINE pop_  #-}
{-# INLINE pop2  #-}
{-# INLINE pop2_ #-}
{-# INLINE pop3  #-}
{-# INLINE popN  #-}
{-# INLINE popN_ #-}
{-# INLINE push  #-}
{-# INLINE push_ #-}
{-# INLINE stack #-}

pop        = stack  >>= \(top:st) -> return (top, st)
pop_       = pop    >>= \(top,st) -> modify (\s -> s { hcSTACK = st }) >> return top
pop2       = stack  >>= \(r:l:st) -> return (r,l,st)
pop2_      = pop2   >>= \(r,l,st) -> modify (\s -> s { hcSTACK = st }) >> return (r,l)
pop3       = stack  >>= \(c:b:a:st) -> modify (\s -> s { hcSTACK = st }) >> return (c,b,a,st)
popN n     = stack  >>= \st -> return $ splitAt n st
popN_ n    = popN n >>= \(vs,rs) -> modify (\s -> s { hcSTACK = rs }) >> return vs
push  v st = modify $ \s -> s { hcSTACK = seq v v:st }
push_ v    = modify $ \s -> s { hcSTACK = seq v v:(hcSTACK s) }
stack      = gets hcSTACK

dbg :: OpCode -> Interpreter ()
{-# INLINE dbg #-}
dbg _  = return ()
-- dbg op = do st <- gets hcSTACK
--             let opStr = show op
--                 opLen = length opStr
--                 spc   = take (50 - opLen) $ repeat ' '
--             liftIO $ putStrLn $ opStr ++ spc ++ show st 

bc :: OpCode -> Interpreter ()
{-# INLINE bc #-}
bc (ARITH o)  = pop2    >>= \(r,l,st) -> push (calcArith l r o) st
bc (PUSH v)   = push_   v
bc POP        = pop_    >>  return ()
bc FIELD      = pop     >>= \(top,st) -> fref top >>= flip push st
bc FSET       = pop2_   >>= \(i,v) -> assignToField Set i v
bc (FMOD o)   = pop2_   >>= \(i,v) -> assignToField o   i v
bc (VAR r)    = push_   =<< (liftIO $! readIORef r)
bc (VSET r)   = pop_    >>= \top -> liftIO $ writeIORef r top
bc (VMOD o r) = pop_    >>= \top -> liftIO $ modifyIORef' r (\v -> calcArith v top o)
bc (BVAR b)   = push_   =<< evalBVariableRef b
bc (BSET b)   = pop_    >>= \top -> modBVar b (const top)
bc (BMOD o b) = pop_    >>= \top -> modBVar b (\v -> calcArith v top o)
bc (ARR r)    = pop     >>= \(idx,st)   -> aref r idx >>= flip push st
bc (ASET r)   = pop2    >>= \(idx,v,st) -> aset r idx v >> push v st
bc (AMOD o r) = pop2    >>= \(idx,v,st) -> amod r idx v o >>= flip push st
bc (CMP o)    = pop2    >>= \(rv,lv,st) -> push (cmpValues lv rv o) st
bc (LGC o)    = pop2    >>= \(rv,lv,st) -> push (calcLogic o lv rv) st
bc NOT        = pop     >>= \(top,st)   -> push (vNot top) st
bc NEG        = pop     >>= \(top,st)   -> push (vNeg top) st
bc (CALL f n) = funcall f n
bc (SPLIT a)  = pop2    >>= \(fs,s,st)  -> calcSplit s fs a >>= flip push st
bc DUP        = stack   >>= \st@(top:_) -> push top st
bc (PRN n)    = popN_ n >>= prn
bc MATCH      = pop2    >>= \(rv,lv,st) -> push (match lv rv) st
bc (IN r)     = pop     >>= \(idx,st)   -> alkp r idx >>= flip push st 
bc (ADEL r)   = pop_    >>= \idx        -> adel r idx
bc (ADRP r)   = liftIO $ writeIORef r M.empty 
bc (FETCH r)  = afetch r
bc (ANXT r)   = anxt r
bc ACHK       = push_   =<< (gets hcKEYS >>= \ks -> return $! vBool (not $ null ks))
bc KDRP       = modify $ \s -> s { hcKEYS   = head (hcKSTACK s)
                                 , hcKSTACK = tail (hcKSTACK s) }
bc DRP        = modify $ \s -> s { hcSTACK = [] }
bc op         = liftIO $ putStrLn $ "UNKNOWN COMMAND " ++ show op

execBC :: [OpCode] -> Interpreter () 
{-# INLINE execBC #-}
execBC []             = return ()
execBC (op@(JF  n):r) = dbg op >> pop_    >>= \top -> if toBool top then execBC r else jmp n
execBC (op@(JMP n):r) = dbg op >> jmp n
execBC (op:ops)       = dbg op >> bc op >> execBC ops 

funcall :: BFunc -> Int -> Interpreter ()
{-# INLINE funcall #-}
funcall Atan2  2 = pop2  >>= \(vx,vy,st)    -> push (calcAtan2 vy  vx) st
funcall Cos    1 = pop   >>= \(top,st)      -> push (proxyFcn cos  top) st
funcall Exp    1 = pop   >>= \(top,st)      -> push (proxyFcn exp  top) st
funcall Int    1 = pop   >>= \(top,st)      -> push (proxyFcn (fromIntegral.truncate) top) st
funcall Log    1 = pop   >>= \(top,st)      -> push (proxyFcn log  top) st
funcall Sin    1 = pop   >>= \(top,st)      -> push (proxyFcn sin  top) st
funcall Sqrt   1 = pop   >>= \(top,st)      -> push (proxyFcn sqrt top) st
funcall Srand  0 = push_ =<< (intSRand >> return (VDouble 0))  
funcall Srand  1 = pop   >>= \(top,st)      -> push (VDouble 0) st >> intSRand' top
funcall Rand   0 = push_ =<< evalRand       
funcall Index  2 = pop2  >>= \(vt,vs,st)    -> push (calcIndex vs vt) st
funcall Length 1 = pop   >>= \(top,st)      -> push (calcLength top) st
funcall Substr 2 = pop2  >>= \(vp,vs,st)    -> push (calcSubstr vs vp) st
funcall Substr 3 = pop3  >>= \(vn,vp,vs,st) -> push (calcSubstr2 vs vp vn) st
funcall FMatch 2 = pop2  >>= \(vr,vs,st)    -> fmatch vs vr >>= flip push st
funcall FSub   3 = pop3  >>= \(vl,vs,vr,_)  -> intFSub calcSub  vr vs (toString vl)
funcall GSub   3 = pop3  >>= \(vl,vs,vr,_)  -> intFSub calcGSub vr vs (toString vl)

fmatch :: Value -> Value -> Interpreter Value
fmatch s r = do
   let (rS,rL) = calcMatch s r
   modify $ \s -> s { hcRSTART  = VDouble (fromIntegral rS)
                    , hcRLENGTH = VDouble (fromIntegral $ if rL == 0 then -1 else rL)
                    }
   return $! vBool (rS /= 0)

intFSub f vr vs vl = do
   let (r,s) = f vr vs vl
   push_ (VDouble $ fromIntegral r)
   push_ (valstr s)

key :: Value -> String
{-# INLINE key #-}
key = B.unpack . toString

aref :: IORef Array -> Value -> Interpreter Value
aref r i = liftIO $ readIORef r >>= \arr -> return $! arr *! (key i)

aset :: IORef Array -> Value -> Value -> Interpreter ()
aset r i v = liftIO $ modifyIORef' r $ \arr -> M.insert (key i) v arr

alkp :: IORef Array -> Value -> Interpreter Value
alkp r i = liftIO (readIORef r) >>= \arr -> return $! VDouble $!
   if M.member (key i) arr then 1 else 0

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

afetch :: IORef Array -> Interpreter ()
afetch r = do
   ks <- liftM (map fst . M.toList) $ liftIO (readIORef r)
   modify $ \s -> s { hcKSTACK = (hcKEYS s):(hcKSTACK s)
                    , hcKEYS   = ks
                    }

anxt :: IORef Value -> Interpreter ()
anxt r = do
   (k:ks) <- gets hcKEYS
   liftIO $ writeIORef r (valstr $ B.pack k)
   modify $ \s -> s { hcKEYS = ks }

fref :: Value -> Interpreter Value
{-# INLINE fref #-}
fref v = do
   let i = toInt v 
   if i == 0
   then gets hcThisLine >>= (return . valstr)
   else liftM (*!! i) $ gets hcFields

jmp :: Int -> Interpreter ()
{-# INLINE jmp #-}
jmp n = gets hcOPCODES >>= \src -> execBC (drop n src)

prn [] =  gets hcThisLine >>= (liftIO . B.putStrLn)
prn vs = do
   ofs <- liftM toString $ gets hcOFS
   ors <- liftM toString $ gets hcORS
   let str = B.intercalate ofs $ map toString vs
   liftIO $ B.putStr $ B.append str ors
