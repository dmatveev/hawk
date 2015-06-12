module Lang.Hawk.Bytecode.Interpreter
       ( wrkInit
       , wrkLoop
       , wrkProcessLine
       , wrkFinish
       ) where

import GHC.IO.Exception (ExitCode(..))

import Data.IORef
import Control.Monad (forM_, liftM, when)
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
import Lang.Hawk.Runtime.Output
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

bc :: HawkContext -> [Value] -> OpCode -> Interpreter [Value]
bc ctx (r:l:st)   (ARITH o)  = {-# SCC "ARITH" #-} return $ (calcArith l r o)*:st
bc ctx st         (PUSH v)   = {-# SCC "PUSH"  #-} return $ v*:st
bc ctx (top:st)   POP        = {-# SCC "POP"   #-} return $ st
bc ctx (top:st)   FIELD      = {-# SCC "FIELD" #-} fref ctx top >>= \v -> return $ v*:st
bc ctx (i:v:st)   FSET       = {-# SCC "FSET"  #-} assignToField ctx Set i v >> (return $ v*:st)
bc ctx (i:v:st)   (FMOD o)   = {-# SCC "FMOD"  #-} assignToField ctx o   i v >> (return $ st)
bc ctx st         (VAR r)    = {-# SCC "VAR"   #-} readIORef r >>= \v -> return $ v*:st
bc ctx (top:st)   (VSET r)   = {-# SCC "VSET"  #-} writeIORef r top >> (return $ top*:st)
bc ctx (top:st)   (VMOD o r) = {-# SCC "VMOD"  #-} modifyIORef' r (\v -> calcArith v top o) >> (return $ st)
bc ctx st         (BVAR b)   = {-# SCC "BVAR"  #-} evalBVariableRef ctx b >>= \v -> return $ v*:st
bc ctx (top:st)   (BSET b)   = {-# SCC "BSET"  #-} modBVar ctx b (const top) >> (return $ top*:st)
bc ctx (top:st)   (BMOD o b) = {-# SCC "BMOD"  #-} modBVar ctx b (\v -> calcArith v top o) >> (return $ st)
bc ctx (idx:st)   (ARR r)    = {-# SCC "ARR"   #-} aref r idx >>= \v -> return $ v*:st
bc ctx (idx:v:st) (ASET r)   = {-# SCC "ASET"  #-} aset r idx v >> (return $ v*:st) -- TODO: previously, value was pushed on stack
bc ctx (idx:v:st) (AMOD o r) = {-# SCC "AMOD"  #-} amod r idx v o >>= \v -> return $ v*:st
bc ctx (rv:lv:st) (CMP o)    = {-# SCC "CMP"   #-} return $ (cmpValues lv rv o)*:st
bc ctx (top:st)   NOT        = {-# SCC "NOT"   #-} return $ (vNot top)*:st
bc ctx (top:st)   NEG        = {-# SCC "NEG"   #-} return $ (vNeg top)*:st
bc ctx st         (CALL f n) = {-# SCC "CALL"  #-} funcall ctx st f n
bc ctx (fs:s:st)  (SPLIT a)  = {-# SCC "SPLIT" #-} calcSplit s fs a >>= \v -> return $ v*:st
bc ctx st@(top:_) DUP        = {-# SCC "DUP"   #-} return $ top*:st
bc ctx st         (PRN n)    = {-# SCC "PRN"   #-} let (vs,r) = splitAt n     st in (prn  ctx vs   >> return r)
bc ctx st         (FPRN n m) = {-# SCC "FPRN"  #-} let (vs,r) = splitAt (n+1) st in (fprn ctx m vs >> return r)
bc ctx st         (PPRN n)   = {-# SCC "PPRN"  #-} let (vs,r) = splitAt (n+1) st in (pprn ctx vs   >> return r)
bc ctx (rv:lv:st) MATCH      = {-# SCC "MATCH" #-} return $ (match lv rv)*:st
bc ctx (idx:st)   (IN r)     = {-# SCC "IN"    #-} alkp r idx >>= \v -> return $ v*:st
bc ctx (idx:st)   (ADEL r)   = {-# SCC "ADEL"  #-} adel r idx >> (return $ st)
bc ctx st         (ADRP r)   = {-# SCC "ADRP"  #-} (writeIORef r M.empty) >> (return $ st)
bc ctx st         (FETCH r)  = {-# SCC "FETCH" #-} afetch ctx r >> (return $ st)
bc ctx st         (ANXT r)   = {-# SCC "ANXT"  #-} anxt ctx r >> (return $ st)
bc ctx st         ACHK       = {-# SCC "ACHK"  #-} (liftM hcKEYS $ readIORef ctx) >>=
                                                      \ks -> return $ (vBool (not $ null ks))*:st
bc ctx st         KDRP       = {-# SCC "KDRP"  #-} do modifyIORef' ctx $ \s -> let ks = hcKSTACK s in s
                                                         { hcKEYS = head ks
                                                         , hcKSTACK = tail ks
                                                         }
                                                      return $ st
bc ctx st         DRP        = {-# SCC "DRP"   #-} return $! []
bc ctx (rv:lv:st) CAT        = {-# SCC "CAT"   #-} return $ (vConcat lv rv)*:st
bc ctx st         GETL       = {-# SCC "GETL"  #-} getline   ctx       >>= \v -> return $ v*:st
bc ctx st         (GETLV r)  = {-# SCC "GETLV" #-} getlineV  ctx r     >>= \v -> return $ v*:st
bc ctx (top:st)   FGETL      = {-# SCC "FGETL" #-} fgetline  ctx top   >>= \v -> return $ v*:st
bc ctx (top:st)   (FGETLV r) = {-# SCC "FGETLV"#-} fgetlineV ctx r top >>= \v -> return $ v*:st
bc ctx (top:st)   PGETL      = {-# SCC "PGETL" #-} pgetline  ctx top   >>= \v -> return $ v*:st
bc ctx (top:st)   (PGETLV r) = {-# SCC "PGETLV"#-} pgetlineV ctx r top >>= \v -> return $ v*:st 
bc ctx st         op         = (putStrLn $ "UNKNOWN COMMAND") >> return st

execBC' :: HawkContext -> [OpCode] -> Interpreter (Bool, [Value]) 
{-# INLINE execBC' #-}
execBC' ctx src = execBC ctx src [] src

execBC :: HawkContext -> [OpCode] -> [Value] -> [OpCode] -> Interpreter (Bool, [Value]) 
execBC ctx src st         []             = return (True, st)
execBC ctx src st         (op@EX:_)      = {-# SCC "EX"  #-} return (False, st)
execBC ctx src st         (op@NXT:_)     = {-# SCC "NXT" #-} return (True, st)
execBC ctx src s@(top:st) (op@(JF  n):r) = {-# SCC "JF"  #-} if toBool top then execBC ctx src st r else jmp ctx src n st
execBC ctx src s@(top:st) (op@(JT  n):r) = {-# SCC "JT"  #-} if toBool top then jmp ctx src n st else execBC ctx src st r
execBC ctx src s@st       (op@(JMP n):_) = {-# SCC "JMP" #-} jmp ctx src n st
execBC ctx src st         (op:ops)       = {-# SCC "OP"  #-} bc ctx st op   >>= \st' -> execBC ctx src st' ops

jmp :: HawkContext -> [OpCode] -> Int -> [Value] -> Interpreter (Bool, [Value])
{-# INLINE jmp #-}
jmp ctx src n st = let r = drop n src in execBC ctx src st r


funcall :: HawkContext -> [Value] -> BFunc -> Int -> Interpreter [Value]
{-# INLINE funcall #-}
funcall ctx (vx:vy:st)    Atan2   2 = return $ (calcAtan2 vy vx)*:st
funcall ctx (top:st)      Cos     1 = return $ (proxyFcn cos top)*:st
funcall ctx (top:st)      Exp     1 = return $ (proxyFcn exp top)*:st
funcall ctx (top:st)      Int     1 = return $ (proxyFcn (fromIntegral.truncate) top)*:st
funcall ctx (top:st)      Log     1 = return $ (proxyFcn log top)*:st
funcall ctx (top:st)      Sin     1 = return $ (proxyFcn sin top)*:st
funcall ctx (top:st)      Sqrt    1 = return $ (proxyFcn sqrt top)*:st
funcall ctx st            Srand   0 = intSRand ctx >> (return $ (VDouble 0)*:st)
funcall ctx (top:st)      Srand   1 = intSRand' ctx top >> (return $ (VDouble 0)*:st)
funcall ctx st            Rand    0 = evalRand ctx >>= \v -> return $ v*:st
funcall ctx (vt:vs:st)    Index   2 = return $ (calcIndex vs vt)*:st
funcall ctx (top:st)      Length  1 = return $ (calcLength top)*:st
funcall ctx (vp:vs:st)    Substr  2 = return $ (calcSubstr vs vp)*:st
funcall ctx (vn:vp:vs:st) Substr  3 = return $ (calcSubstr2 vs vp vn)*:st
funcall ctx (vr:vs:st)    FMatch  2 = fmatch ctx vs vr >>= \v -> return $ v*:st
funcall ctx (vl:vs:vr:st) FSub    3 = intFSub calcSub vr vs (toString vl) st
funcall ctx (vl:vs:vr:st) GSub    3 = intFSub calcGSub vr vs (toString vl) st
funcall ctx st            Printf  n = intPrintf ctx (reverse $ take n st) >> return ((VDouble 0)*:st) 
funcall ctx st            SPrintf n = let (rvs,r)  = splitAt n st
                                          (fmt:vs) = reverse rvs
                                      in return $ (valstr $ sprintf (toString fmt) vs)*:r
funcall ctx (top:st)      Close   1 = intClose ctx (toString top) >>= \v -> return (v*:st)

fmatch :: HawkContext -> Value -> Value -> Interpreter Value
fmatch ctx s r = do
   let (rS,rL) = calcMatch s r
   modifyIORef' ctx $ \s -> s
      { hcRSTART  = VDouble (fromIntegral rS)
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
aref r i = readIORef r >>= \arr -> return $! arr *! (key i)

aset :: IORef Array -> Value -> Value -> Interpreter ()
aset r i v = modifyIORef' r $ \arr -> M.insert (key i) v arr

alkp :: IORef Array -> Value -> Interpreter Value
alkp r i = readIORef r >>= \arr -> return $! VDouble $!
   if M.member (key i) arr then 1 else 0

amod :: IORef Array -> Value -> Value -> ArithOp -> Interpreter Value
amod r i v o = do
   arr <- readIORef r
   let idx      = key i
       newValue = calcArith (arr *! idx) v o
       newArray = M.insert idx newValue arr
   writeIORef r (seq newArray newArray)
   return $! newValue

adel :: IORef Array -> Value -> Interpreter ()
adel r i = modifyIORef' r $ \arr -> M.delete (key i) arr

afetch :: HawkContext -> IORef Array -> Interpreter ()
afetch ctx r = do
   ks <- liftM (map fst . M.toList) $ readIORef r
   modifyIORef' ctx $ \s -> s
          { hcKSTACK  = (hcKEYS s):(hcKSTACK s)
          , hcKEYS    = ks
          }

anxt :: HawkContext -> IORef Value -> Interpreter ()
anxt ctx r = do
   (k:ks) <- liftM hcKEYS $ readIORef ctx
   writeIORef r (valstr $ B.pack k)
   modifyIORef' ctx $ \s -> s {hcKEYS = ks}

fref :: HawkContext -> Value -> Interpreter Value
{-# INLINE fref #-}
fref ctx v = do
   let i = toInt v 
   if i == 0
   then (liftM hcThisLine $ readIORef ctx) >>= (return . valstr)
   else liftM ((*!! i) . hcFields) $ readIORef ctx

prn :: HawkContext -> [Value] -> Interpreter ()
prn ctx [] = do
   cc <- readIORef ctx
   write (hcOutput cc) $ WRqRaw 0 (hcThisLine cc)
prn ctx vs = do
   cc <- readIORef ctx
   write (hcOutput cc) $ WRqPrint 0 vs (hcOFS cc) (hcORS cc)

fprn :: HawkContext -> FileMod -> [Value] -> Interpreter ()
fprn ctx m (f:[]) = (liftM hcThisLine $ readIORef ctx) >>= writeToHandle ctx m (toString f)
fprn ctx m (f:vs) = do
   ofs <- liftM ( toString . hcOFS) $ readIORef ctx
   let str = B.intercalate ofs $ map toString vs
   writeToHandle ctx m (toString f) str

pprn :: HawkContext -> [Value] -> Interpreter ()
pprn ctx (f:[]) = (liftM hcThisLine $ readIORef ctx) >>= writeToProcess ctx (toString f)
pprn ctx (f:vs) = do
   ofs <- liftM (toString . hcOFS) $ readIORef ctx
   let str = B.intercalate ofs $ map toString vs
   writeToProcess ctx (toString f) str

writeToHandle :: HawkContext -> FileMod -> B.ByteString -> B.ByteString -> Interpreter ()
writeToHandle ctx m f str = do
   h <- intGetHandle ctx f m
   ors <- liftM (toString . hcORS) $ readIORef ctx
   B.hPutStr h (B.append str ors)

writeToProcess :: HawkContext -> B.ByteString -> B.ByteString -> Interpreter ()
writeToProcess ctx f str = do
   h <- intGetProcessHandle ctx f
   ors <- liftM (toString . hcORS) $ readIORef ctx
   B.hPutStr h (B.append str ors)

intGetHandle :: HawkContext -> B.ByteString -> FileMod -> Interpreter Handle
intGetHandle ctx f m = do
   mh <- liftM (M.lookup f . hcHandles) $ readIORef ctx
   case mh of 
     Just h  -> return h
     Nothing -> do h <- openFile (B.unpack f) (toMode m)
                   modifyIORef' ctx $ \s -> s { hcHandles = M.insert f h (hcHandles s)}
                   return h

intGetProcessHandle :: HawkContext -> B.ByteString -> Interpreter Handle
intGetProcessHandle ctx cmd = do
   mh <- liftM (M.lookup cmd . hcPHandles) $ readIORef ctx
   case mh of 
     Just (_,h) -> return h
     Nothing    -> do
       (Just hin, _, _, ph) <- createProcess $ (shell cmd') {std_in = CreatePipe}
       modifyIORef' ctx $ \s -> s {hcPHandles = M.insert cmd (ph,hin) (hcPHandles s)}
       return hin
 where cmd' = B.unpack cmd

{-# INLINE toMode #-}
toMode ModAppend  = AppendMode
toMode ModRewrite = WriteMode

setupContext :: HawkContext -> Record -> [Record] ->Interpreter ()
{-# INLINE setupContext #-}
setupContext ctx (Record _ l nf flds) rs = {-# SCC "CTXMOD" #-} do
   modifyIORef' ctx $ \s -> s
     { hcThisLine = l
     , hcFields   = flds
     , hcNF       = VDouble (fromIntegral $ nf)
     , hcNR       = VDouble (succ $ toDouble $ hcNR s)
     , hcFNR      = VDouble (succ $ toDouble $ hcFNR s)
     , hcWorkload = rs
     }

getline :: HawkContext -> Interpreter Value
getline ctx = do
      cc <- readIORef ctx
      case hcWorkload cc of
        [] -> do mw <- fetch (hcInput cc)
                 case mw of
                    Nothing                  -> return $ VDouble $! -1
                    Just (Workload _ (r:rs)) -> handleRecord r rs
        (r:rs) -> handleRecord r rs 
 where handleRecord r rs = setupContext ctx r rs >> return (VDouble $! 1)

getlineV :: HawkContext -> (IORef Value) -> Interpreter Value
getlineV ctx ref = do
      cc <- readIORef ctx
      case hcWorkload cc of
        [] -> do mw <- fetch (hcInput cc)
                 case mw of
                    Nothing                  -> return $ VDouble $! -1
                    Just (Workload _ (r:rs)) -> handleRecord r rs
        (r:rs) -> handleRecord r rs 
 where handleRecord (Record _ l nf flds) rs = do
         writeIORef ref (valstr l)
         modifyIORef' ctx $ \s -> s
            { hcNR  = VDouble (succ $ toDouble $ hcNR s)
            , hcFNR = VDouble (succ $ toDouble $ hcFNR s)
            }
         return $ VDouble $! 1

fgetline :: HawkContext -> Value -> Interpreter Value
fgetline ctx vf = do
   is <- fGetInput ctx (toString vf)
   rs <- liftM hcRS $ readIORef ctx
   ml <- nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do flds <- splitIntoFields ctx l
                     let fldm = IM.fromList (zip [1,2..] (map valstr flds))
                     modifyIORef' ctx $ \s -> s
                         { hcThisLine = l
                         , hcFields   = fldm
                         , hcNF       = VDouble $ fromIntegral $ length flds
                         }
                     return $ VDouble 1

fgetlineV :: HawkContext -> IORef Value -> Value -> Interpreter Value
fgetlineV ctx r vf = do
   is <- fGetInput ctx (toString vf)
   rs <- liftM hcRS $ readIORef ctx
   ml <- nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do writeIORef r (valstr l)
                     return $ VDouble 1

fGetInput :: HawkContext -> B.ByteString -> Interpreter InputSource
fGetInput ctx f = do 
   mi <- liftM (M.lookup f . hcFInputs) $ readIORef ctx
   case mi of 
     Just is  -> return is
     Nothing  -> do is <- openInputFile f
                    modifyIORef' ctx $ \s -> s { hcFInputs = M.insert f is (hcFInputs s)}
                    return is


pgetline :: HawkContext -> Value -> Interpreter Value
pgetline ctx vcmd = do
   (_,is) <- intPopen ctx (toString vcmd)
   rs <- liftM hcRS $ readIORef ctx
   ml <- nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do flds <- splitIntoFields ctx l
                     let fldm = IM.fromList (zip [1,2..] (map valstr flds))
                     modifyIORef' ctx $ \s -> s
                       { hcThisLine = l
                       , hcFields   = fldm
                       , hcNF       = VDouble $ fromIntegral $ length flds
                       }
                     return $ VDouble 1

pgetlineV :: HawkContext -> IORef Value -> Value -> Interpreter Value
pgetlineV ctx r vcmd = do
   (_,is) <- intPopen ctx (toString vcmd)
   rs <- liftM hcRS $ readIORef ctx
   ml <- nextLine is (toString rs)
   case ml of
      Nothing  -> return $ VDouble $ -1
      (Just l) -> do writeIORef r (valstr l)
                     return $ VDouble 1 

intPopen :: HawkContext -> B.ByteString -> Interpreter (ProcessHandle, InputSource)
intPopen ctx cmd = do
   mp <- liftM (M.lookup cmd . hcIPHandles) $ readIORef ctx
   case mp of
      (Just p) -> return p
      Nothing  -> do
         (_, Just hout, _, ph) <- createProcess $ (shell cmd') {std_out = CreatePipe}
         is <- fromHandle hout
         let p = (ph,is)
         modifyIORef' ctx $ \s -> s {hcIPHandles = M.insert cmd p (hcIPHandles s)}
         return p
  where cmd' = B.unpack cmd

-- TODO: What about input files/pipes? The book does not say anything about that.
intClose :: HawkContext -> B.ByteString -> Interpreter Value
intClose ctx name = do
   mp <- liftM (M.lookup name . hcPHandles) $ readIORef ctx
   case mp of
      (Just (p,h)) -> do
         modifyIORef' ctx $ \s -> s { hcPHandles = M.delete name (hcPHandles s) }
         ex <- hClose h >> waitForProcess p
         return $ case ex of
           ExitSuccess     -> VDouble 0
           (ExitFailure i) -> VDouble $ fromIntegral i
      Nothing -> do
        mf <- liftM (M.lookup name . hcHandles) $ readIORef ctx
        case mf of
           (Just h) -> do modifyIORef' ctx $ \s -> s {hcHandles = M.delete name (hcHandles s)}
                          hClose h
                          return $ VDouble $ -1
           Nothing  -> return $ VDouble $ -1

intPrintf :: HawkContext -> [Value] -> Interpreter ()
{-# INLINE intPrintf #-}
intPrintf ctx vs = do
  cc <- readIORef ctx
  write (hcOutput cc) $ WRqPrintf 0 vs


intSRand :: HawkContext -> Interpreter ()
{-# INLINE intSRand #-}
intSRand ctx = getStdGen >>= \g -> modifyIORef' ctx $ \s -> s {hcStdGen = g}

intSRand' :: HawkContext -> Value -> Interpreter ()
{-# INLINE intSRand' #-}
intSRand' ctx i = modifyIORef' ctx $ \s -> s {hcStdGen = mkStdGen (toInt i)}

evalRand :: HawkContext -> Interpreter Value
evalRand ctx = do
     g <- liftM hcStdGen $ readIORef ctx
     let (r, g') = randomR (0.0, 1.0) g
     modifyIORef' ctx $ \s -> s {hcStdGen =  g'}
     return $! VDouble r

wrkInit :: HawkContext -> Interpreter Bool
{-# INLINE wrkInit #-}
wrkInit ctx = liftM fst $ (liftM hcOPCODES $ readIORef ctx) >>= execBC' ctx

wrkLoop :: HawkContext -> Interpreter ()        
{-# INLINE wrkLoop #-}
wrkLoop ctx = do
   q <- (liftM hcInput $ readIORef ctx) >>= fetch
   case q of
      Nothing  -> return ()
      (Just w) -> do modifyIORef' ctx $ \s -> s {hcWorkload = wRS w}
                     wrkProc ctx >>= \cont -> when cont $ wrkLoop ctx

wrkProcessLine :: HawkContext -> B.ByteString -> Interpreter Bool
{-# INLINE wrkProcessLine #-}
wrkProcessLine ctx l = do
   flds <- splitIntoFields ctx l
   let fldm = IM.fromList (zip [1,2..] (map valstr flds))
   modifyIORef' ctx $ \s -> s
     { hcThisLine = l
     , hcFields   = fldm
     , hcNF       = VDouble $ fromIntegral $ length flds
     , hcNR       = VDouble (succ $ toDouble $ hcNR s)
     , hcFNR      = VDouble (succ $ toDouble $ hcFNR s)
     }
   liftM fst $ (liftM hcOPCODES $ readIORef ctx) >>= execBC' ctx
   
wrkProc :: HawkContext -> Interpreter Bool
{-# INLINE wrkProc #-}
wrkProc ctx = do
   w <- liftM hcWorkload $ readIORef ctx
   case w of
     [] -> return True
     (r:rs) -> do
        setupContext ctx r rs
        (cont, _) <- (liftM hcOPCODES $ readIORef ctx) >>= execBC' ctx
        if cont then wrkProc ctx else return False 

wrkFinish :: HawkContext -> Interpreter ()
{-# INLINE wrkFinish #-}
wrkFinish ctx = do
   (liftM hcOPCODES $ readIORef ctx) >>= execBC' ctx
   cc <- readIORef ctx
   mapM_ hClose (M.elems $ hcHandles cc)
   forM_ (M.elems $ hcPHandles cc) $ \(p,h) -> do
       hClose h
       waitForProcess p
   forM_ (M.elems $ hcIPHandles cc) $ \(p,is) -> do
       closeStream is
       waitForProcess p
