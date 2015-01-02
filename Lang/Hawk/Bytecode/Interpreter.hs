module Lang.Hawk.Bytecode.Interpreter (execBC', setupContext) where

import GHC.IO.Exception (ExitCode(..))

import Data.IORef
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as B

import System.IO
import System.Process
import System.Random (mkStdGen, getStdGen, randomR)

import Lang.Hawk.Basic
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Interpreter
import Lang.Hawk.Runtime
import Lang.Hawk.Runtime.Input
import Lang.Hawk.Runtime.Printf (sprintf)

-- dbg :: [Value] -> OpCode -> Interpreter ()
-- {-# INLINE dbg #-}
-- dbg st op = do
--    let opStr = show op
--        opLen = length opStr
--        spc   = take (50 - opLen) $ repeat ' '
--    liftIO $ putStrLn $ opStr ++ spc ++ show st 

(*:) :: a -> [a] -> [a]
{-# INLINE (*:) #-}
x *: xs = seq x x:xs

bc :: [Value] -> OpCode -> Interpreter [Value]
bc (r:l:st)   (ARITH o)  = {-# SCC "ARITH" #-} return $ (calcArith l r o)*:st
bc st         (PUSH v)   = {-# SCC "PUSH"  #-} return $ v*:st
bc (top:st)   POP        = {-# SCC "POP"   #-} return $ st
bc (top:st)   FIELD      = {-# SCC "FIELD" #-} fref top >>= \v -> return $ v*:st
bc (i:v:st)   FSET       = {-# SCC "FSET"  #-} assignToField Set i v >> (return $ v*:st)
bc (i:v:st)   (FMOD o)   = {-# SCC "FMOD"  #-} assignToField o   i v >> (return $ st)
bc st         (VAR r)    = {-# SCC "VAR"   #-} (liftIO $ readIORef r) >>= \v -> return $ v*:st
bc (top:st)   (VSET r)   = {-# SCC "VSET"  #-} (liftIO $ writeIORef r top) >> (return $ top*:st)
bc (top:st)   (VMOD o r) = {-# SCC "VMOD"  #-} (liftIO $ modifyIORef' r (\v -> calcArith v top o)) >> (return $ st)
bc st         (BVAR b)   = {-# SCC "BVAR"  #-} evalBVariableRef b >>= \v -> return $ v*:st
bc (top:st)   (BSET b)   = {-# SCC "BSET"  #-} modBVar b (const top) >> (return $ top*:st)
bc (top:st)   (BMOD o b) = {-# SCC "BMOD"  #-} modBVar b (\v -> calcArith v top o) >> (return $ st)
bc (idx:st)   (ARR r)    = {-# SCC "ARR"   #-} aref r idx >>= \v -> return $ v*:st
bc (idx:v:st) (ASET r)   = {-# SCC "ASET"  #-} aset r idx v >> (return $ v*:st) -- TODO: previously, value was pushed on stack
bc (idx:v:st) (AMOD o r) = {-# SCC "AMOD"  #-} amod r idx v o >>= \v -> return $ v*:st
bc (rv:lv:st) (CMP o)    = {-# SCC "CMP"   #-} return $ (cmpValues lv rv o)*:st
bc (top:st)   NOT        = {-# SCC "NOT"   #-} return $ (vNot top)*:st
bc (top:st)   NEG        = {-# SCC "NEG"   #-} return $ (vNeg top)*:st
bc st         (CALL f n) = {-# SCC "CALL"  #-} funcall st f n
bc (fs:s:st)  (SPLIT a)  = {-# SCC "SPLIT" #-} liftIO (calcSplit s fs a) >>= \v -> return $ v*:st
bc st@(top:_) DUP        = {-# SCC "DUP"   #-} return $ top*:st
bc st         (PRN n)    = {-# SCC "PRN"   #-} let (vs,r) = splitAt n     st in (prn vs    >> return r)
bc st         (FPRN n m) = {-# SCC "FPRN"  #-} let (vs,r) = splitAt (n+1) st in (fprn m vs >> return r)
bc st         (PPRN n)   = {-# SCC "PPRN"  #-} let (vs,r) = splitAt (n+1) st in (pprn vs   >> return r)
bc (rv:lv:st) MATCH      = {-# SCC "MATCH" #-} return $ (match lv rv)*:st
bc (idx:st)   (IN r)     = {-# SCC "IN"    #-} alkp r idx >>= \v -> return $ v*:st
bc (idx:st)   (ADEL r)   = {-# SCC "ADEL"  #-} adel r idx >> (return $ st)
bc st         (ADRP r)   = {-# SCC "ADRP"  #-} (liftIO $ writeIORef r M.empty) >> (return $ st)
bc st         (FETCH r)  = {-# SCC "FETCH" #-} afetch r >> (return $ st)
bc st         (ANXT r)   = {-# SCC "ANXT"  #-} anxt r >> (return $ st)
bc st         ACHK       = {-# SCC "ACHK"  #-} gets hcKEYS >>= \ks -> return $ (vBool (not $ null ks))*:st
bc st         KDRP       = {-# SCC "KDRP"  #-} do modify $ \s -> s { hcKEYS   = head (hcKSTACK s)
                                                                   , hcKSTACK = tail (hcKSTACK s) }
                                                  return $ st
bc st         DRP        = {-# SCC "DRP"   #-} return $! []
bc (rv:lv:st) CAT        = {-# SCC "CAT"   #-} return $ (vConcat lv rv)*:st
bc st         GETL       = {-# SCC "GETL"  #-} getline         >>= \v -> return $ v*:st
bc st         (GETLV r)  = {-# SCC "GETLV" #-} getlineV r      >>= \v -> return $ v*:st
bc (top:st)   FGETL      = {-# SCC "FGETL" #-} fgetline top    >>= \v -> return $ v*:st
bc (top:st)   (FGETLV r) = {-# SCC "FGETLV"#-} fgetlineV r top >>= \v -> return $ v*:st
bc (top:st)   PGETL      = {-# SCC "PGETL" #-} pgetline top    >>= \v -> return $ v*:st
bc (top:st)   (PGETLV r) = {-# SCC "PGETLV"#-} pgetlineV r top >>= \v -> return $ v*:st 
bc st         op         = (liftIO $ putStrLn $ "UNKNOWN COMMAND") >> return st

execBC' :: [OpCode] -> Interpreter (Bool, [Value]) 
{-# INLINE execBC' #-}
execBC' src = execBC src [] src

execBC :: [OpCode] -> [Value] -> [OpCode] -> Interpreter (Bool, [Value]) 
execBC src st         []             = return (True, st)
execBC src st         (op@EX:_)      = {-# SCC "EX"  #-} return (False, st)
execBC src st         (op@NXT:_)     = {-# SCC "NXT" #-} return (True, st)
execBC src s@(top:st) (op@(JF  n):r) = {-# SCC "JF"  #-} if toBool top then execBC src st r else jmp src n st
execBC src s@(top:st) (op@(JT  n):r) = {-# SCC "JT"  #-} if toBool top then jmp src n st else execBC src st r
execBC src s@st       (op@(JMP n):_) = {-# SCC "JMP" #-} jmp src n st
execBC src st         (op:ops)       = {-# SCC "OP"  #-} bc st op   >>= \st' -> execBC src st' ops

jmp :: [OpCode] -> Int -> [Value] -> Interpreter (Bool, [Value])
{-# INLINE jmp #-}
jmp src n st = let r = drop n src in (execBC src st r)


funcall :: [Value] -> BFunc -> Int -> Interpreter [Value]
{-# INLINE funcall #-}
funcall (vx:vy:st)    Atan2   2 = return $ (calcAtan2 vy vx)*:st
funcall (top:st)      Cos     1 = return $ (proxyFcn cos top)*:st
funcall (top:st)      Exp     1 = return $ (proxyFcn exp top)*:st
funcall (top:st)      Int     1 = return $ (proxyFcn (fromIntegral.truncate) top)*:st
funcall (top:st)      Log     1 = return $ (proxyFcn log top)*:st
funcall (top:st)      Sin     1 = return $ (proxyFcn sin top)*:st
funcall (top:st)      Sqrt    1 = return $ (proxyFcn sqrt top)*:st
funcall st            Srand   0 = intSRand >> (return $ (VDouble 0)*:st)
funcall (top:st)      Srand   1 = intSRand' top >> (return $ (VDouble 0)*:st)
funcall st            Rand    0 = evalRand >>= \v -> return $ v*:st
funcall (vt:vs:st)    Index   2 = return $ (calcIndex vs vt)*:st
funcall (top:st)      Length  1 = return $ (calcLength top)*:st
funcall (vp:vs:st)    Substr  2 = return $ (calcSubstr vs vp)*:st
funcall (vn:vp:vs:st) Substr  3 = return $ (calcSubstr2 vs vp vn)*:st
funcall (vr:vs:st)    FMatch  2 = fmatch vs vr >>= \v -> return $ v*:st
funcall (vl:vs:vr:st) FSub    3 = intFSub calcSub vr vs (toString vl) st
funcall (vl:vs:vr:st) GSub    3 = intFSub calcGSub vr vs (toString vl) st
funcall st            Printf  n = intPrintf (reverse $ take n st) >> return ((VDouble 0)*:st) 
funcall st            SPrintf n = let (rvs,r)  = splitAt n st
                                      (fmt:vs) = reverse rvs
                                  in return $ (valstr $ sprintf (toString fmt) vs)*:r
funcall (top:st)      Close   1 = intClose (toString top) >>= \v -> return (v*:st)

fmatch :: Value -> Value -> Interpreter Value
fmatch s r = do
   let (rS,rL) = calcMatch s r
   modify $ \s -> s { hcRSTART  = VDouble (fromIntegral rS)
                    , hcRLENGTH = VDouble (fromIntegral $ if rL == 0 then -1 else rL)
                    }
   return $! vBool (rS /= 0)

{-# INLINE intFSub #-}
intFSub f vr vs vl st = do
   let (r,s) = f vr vs vl
   return $ (valstr s):(VDouble $ fromIntegral r)*:st

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

prn :: [Value] -> Interpreter ()
prn [] =  gets hcThisLine >>= (liftIO . B.putStrLn)
prn vs = do
   ofs <- liftM toString $ gets hcOFS
   ors <- liftM toString $ gets hcORS
   let str = B.intercalate ofs $ map toString vs
   liftIO $ B.putStr $ B.append str ors

fprn :: FileMod -> [Value] -> Interpreter ()
fprn m (f:[]) = gets hcThisLine >>= writeToHandle m (toString f)
fprn m (f:vs) = do
   ofs <- liftM toString $ gets hcOFS
   let str = B.intercalate ofs $ map toString vs
   writeToHandle m (toString f) str

pprn :: [Value] -> Interpreter ()
pprn (f:[]) = gets hcThisLine >>= writeToProcess (toString f)
pprn (f:vs) = do
   ofs <- liftM toString $ gets hcOFS
   let str = B.intercalate ofs $ map toString vs
   writeToProcess (toString f) str

writeToHandle :: FileMod -> B.ByteString -> B.ByteString -> Interpreter ()
writeToHandle m f str = do
   h <- intGetHandle f m
   ors <- liftM toString $ gets hcORS
   liftIO $ B.hPutStr h (B.append str ors)

writeToProcess :: B.ByteString -> B.ByteString -> Interpreter ()
writeToProcess f str = do
   h <- intGetProcessHandle f
   ors <- liftM toString $ gets hcORS
   liftIO $ B.hPutStr h (B.append str ors)

intGetHandle :: B.ByteString -> FileMod -> Interpreter Handle
intGetHandle f m = do
   mh <- liftM (M.lookup f) $ gets hcHandles
   case mh of 
     Just h  -> return h
     Nothing -> do h <- liftIO $ openFile (B.unpack f) (toMode m)
                   modify $ \s -> s { hcHandles = M.insert f h (hcHandles s) }
                   return h

intGetProcessHandle :: B.ByteString -> Interpreter Handle
intGetProcessHandle cmd = do
   mh <- liftM (M.lookup cmd) $ gets hcPHandles
   case mh of 
     Just (_,h) -> return h
     Nothing    -> do
       (Just hin, _, _, ph) <- liftIO $ createProcess $ (shell cmd') {std_in = CreatePipe}
       modify $ \s -> s { hcPHandles = M.insert cmd (ph,hin) (hcPHandles s) }
       return hin
 where cmd' = B.unpack cmd

{-# INLINE toMode #-}
toMode ModAppend  = AppendMode
toMode ModRewrite = WriteMode

setupContext :: Record -> [Record] ->Interpreter ()
{-# INLINE setupContext #-}
setupContext (Record _ l nf flds) rs = {-# SCC "CTXMOD" #-} modify $ \s ->
   s { hcThisLine = l
     , hcFields   = flds
     , hcNF       = VDouble (fromIntegral $ nf)
     , hcNR       = VDouble (succ $ toDouble $ hcNR  s)
     , hcFNR      = VDouble (succ $ toDouble $ hcFNR s)
     , hcWorkload = rs
     }

getline :: Interpreter Value
getline = do
      w <- gets hcWorkload
      case w of
        [] -> do i  <- gets hcInput
                 mw <- liftIO $ fetch i
                 case mw of
                    Nothing                  -> return $ VDouble $! -1
                    Just (Workload _ (r:rs)) -> handleRecord r rs
        (r:rs) -> handleRecord r rs 
 where handleRecord r rs = setupContext r rs >> return (VDouble $! 1)

getlineV :: (IORef Value) -> Interpreter Value
getlineV ref = do
      w <- gets hcWorkload
      case w of
        [] -> do i  <- gets hcInput
                 mw <- liftIO $ fetch i
                 case mw of
                    Nothing                  -> return $ VDouble $! -1
                    Just (Workload _ (r:rs)) -> handleRecord r rs
        (r:rs) -> handleRecord r rs 
 where handleRecord (Record _ l nf flds) rs = do
         liftIO $ writeIORef ref (valstr l)
         modify $ \s -> s { hcNR  = VDouble (succ $ toDouble $ hcNR  s)
                          , hcFNR = VDouble (succ $ toDouble $ hcFNR s)
                          }
         return $ VDouble $! 1

fgetline :: Value -> Interpreter Value
fgetline vf = do
   is <- fGetInput (toString vf)
   rs <- gets hcRS
   ml <- liftIO $ nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do flds <- splitIntoFields l
                     let fldm = IM.fromList (zip [1,2..] (map valstr flds))
                     modify $ \s -> s { hcThisLine = l
                                      , hcFields   = fldm
                                      , hcNF       = VDouble $ fromIntegral $ length flds
                                      }
                     return $ VDouble 1

fgetlineV :: IORef Value -> Value -> Interpreter Value
fgetlineV r vf = do
   is <- fGetInput (toString vf)
   rs <- gets hcRS
   ml <- liftIO $ nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do liftIO $ writeIORef r (valstr l)
                     return $ VDouble 1

fGetInput :: B.ByteString -> Interpreter InputSource
fGetInput f = do 
   mi <- liftM (M.lookup f) $ gets hcFInputs
   case mi of 
     Just is  -> return is
     Nothing  -> do is <- liftIO $ openInputFile f
                    modify $ \s -> s { hcFInputs = M.insert f is (hcFInputs s)}
                    return is


pgetline :: Value -> Interpreter Value
pgetline vcmd = do
   (_,is) <- intPopen (toString vcmd)
   rs <- gets hcRS
   ml <- liftIO $ nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do flds <- splitIntoFields l
                     let fldm = IM.fromList (zip [1,2..] (map valstr flds))
                     modify $ \s -> s { hcThisLine = l
                                      , hcFields   = fldm
                                      , hcNF       = VDouble $ fromIntegral $ length flds
                                      }
                     return $ VDouble 1

pgetlineV :: IORef Value -> Value -> Interpreter Value
pgetlineV r vcmd = do
   (_,is) <- intPopen (toString vcmd)
   rs <- gets hcRS
   ml <- liftIO $ nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do liftIO $ writeIORef r (valstr l)
                     return $ VDouble 1 

intPopen :: B.ByteString -> Interpreter (ProcessHandle, InputSource)
intPopen cmd = do
   mp <- liftM (M.lookup cmd) $ gets hcIPHandles
   case mp of
      (Just p) -> return p
      Nothing  -> do
         (_, Just hout, _, ph) <- liftIO $ createProcess $ (shell cmd') {std_out = CreatePipe}
         is <- liftIO $ fromHandle hout
         let p = (ph,is)
         modify $ \s -> s { hcIPHandles = M.insert cmd p (hcIPHandles s) }
         return p
  where cmd' = B.unpack cmd

-- TODO: What about input files/pipes? The book does not say anything about that.
intClose :: B.ByteString -> Interpreter Value
intClose name = do
   mp <- liftM (M.lookup name) $ gets hcPHandles
   case mp of
      (Just (p,h)) -> do
         modify $ \s -> s { hcPHandles = M.delete name (hcPHandles s) }
         ex <- liftIO $ hClose h >> waitForProcess p
         case ex of
           ExitSuccess     -> return $ VDouble 0
           (ExitFailure i) -> return $ VDouble $ fromIntegral i
      Nothing -> do
        mf <- liftM (M.lookup name) $ gets hcHandles
        case mf of
           (Just h) -> do modify $ \s -> s { hcHandles = M.delete name (hcHandles s) }
                          liftIO (hClose h)
                          return $ VDouble $ -1
           Nothing  -> return $ VDouble $ -1

intPrintf :: [Value] -> Interpreter ()
{-# INLINE intPrintf #-}
intPrintf (fmt:vs) = liftIO $ B.putStr $ sprintf (toString fmt) vs


intSRand :: Interpreter ()
{-# INLINE intSRand #-}
intSRand = liftIO getStdGen >>= \g -> modify (\s -> s {hcStdGen = g})

intSRand' :: Value -> Interpreter ()
{-# INLINE intSRand' #-}
intSRand' i = modify (\s -> s {hcStdGen = mkStdGen (toInt i)})

evalRand :: Interpreter Value
evalRand = do
     g <- gets hcStdGen
     let (r, g') = randomR (0.0, 1.0) g
     modify $ (\s -> s { hcStdGen = g' })
     return $! VDouble r
