{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter where

import qualified Data.ByteString.Char8 as B

import Text.Regex.PCRE

import Data.IORef
import Data.Fixed (mod')

import Data.List (find, intercalate)
import Data.Maybe (fromJust)
import qualified Data.Foldable as F 

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State.Strict

import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import System.Random
import System.IO

import Lang.Hawk.AST
import Lang.Hawk.Basic
import Lang.Hawk.Value
import Lang.Hawk.Bytecode
import Lang.Hawk.Bytecode.Compiler
import Lang.Hawk.Analyzer
import Lang.Hawk.Runtime

data HawkContext = HawkContext
                 { hcFields   :: (IM.IntMap Value)
                 , hcArrays   :: !(M.Map (String, String) Value)

                 , hcThisLine :: B.ByteString
                 , hcStdGen   :: StdGen

                 , hcSTACK    :: [Value]
                 , hcSTARTUP  :: ![OpCode]
                 , hcOPCODES  :: ![OpCode]
                 , hcSHUTDOWN :: ![OpCode]

                 , hcARGC     :: !Value
                 , hcARGV     :: !Value
                 , hcFILENAME :: !Value
                 , hcFNR      :: !Value
                 , hcFS       :: !Value
                 , hcNF       :: !Value
                 , hcNR       :: !Value
                 , hcOFMT     :: !Value
                 , hcOFS      :: !Value
                 , hcORS      :: !Value
                 , hcRLENGTH  :: !Value
                 , hcRS       :: !Value
                 , hcRSTART   :: !Value
                 , hcSUBSEP   :: !Value
                 }

(*!) :: Ord k => M.Map k Value -> k -> Value
m *! k = M.findWithDefault (VDouble 0) k m

(*!!) :: IM.IntMap Value -> Int -> Value
m *!! k = IM.findWithDefault (VDouble 0) k m


emptyContext :: AwkSource -> HawkContext
emptyContext s = HawkContext
                 { hcFields   = IM.empty
                 , hcArrays   = M.empty

                 , hcThisLine = ""
                 , hcStdGen   = mkStdGen 0

                 , hcSTACK    = []
                 , hcSTARTUP  = F.toList startup
                 , hcOPCODES  = F.toList opcodes
                 , hcSHUTDOWN = F.toList shutdown

                 , hcARGC     = defstr  ""
                 , hcARGV     = defstr  ""
                 , hcFILENAME = defstr  ""
                 , hcFNR      = VDouble 0
                 , hcFS       = defstr  " "
                 , hcNF       = VDouble 0
                 , hcNR       = VDouble 0
                 , hcOFMT     = defstr  "%.6f"
                 , hcOFS      = defstr  " "
                 , hcORS      = defstr  "\n"
                 , hcRLENGTH  = VDouble  0
                 , hcRS       = defstr  "\n"
                 , hcRSTART   = VDouble 0
                 , hcSUBSEP   = defstr  "\034"
                 }
  where (startup, opcodes, shutdown) = compile s

newtype Interpreter a = Interpreter (StateT HawkContext IO a)
                        deriving (Monad, MonadIO, MonadState HawkContext,
                                  Applicative, Functor)

runInterpreter :: Interpreter a -> HawkContext -> IO HawkContext
runInterpreter (Interpreter stt) c = execStateT stt c

unsup s = fail $ s ++ " are not yet supported"


-- Evaluate an expression, return the result
eval :: Expression -> Interpreter Value
eval (Arith op le re)                = evalArith op le re
eval (Const (LitNumeric i))          = return $! VDouble i
eval (Const (LitStr     s))          = return $! valstr $ B.pack s
eval (Const (LitRE      s))          = return $! valstr $ B.pack s
eval (Id          e)                 = eval e
eval (FieldRef    e)                 = evalFieldRef e
-- eval (VariableRef s)                 = evalVariableRef s
eval (Variable ref)                  = (liftIO $ readIORef ref) >>= (return $!)
eval (BuiltInVar  s)                 = evalBVariableRef s 
eval (ArrayRef    s e)               = evalArrRef s e
eval (Incr n f@(FieldRef      e))    = undefined -- incrField n f
eval (Incr n v@(Variable    ref))    = incrVar n v
eval (Incr n a@(ArrayRef    s e))    = incrArr n a
eval (Decr n f@(FieldRef      e))    = undefined -- decrField n f
eval (Decr n v@(Variable    ref))    = decrVar n v
eval (Decr n a@(ArrayRef    s e))    = decrArr n a
eval (Relation op le re)             = evalCmp op le re
eval (Not e)                         = evalNot e 
eval (Neg e)                         = evalNeg e
eval (Concat _ _ )                   = unsup "Concatenations"
eval (In s (VariableRef arr))        = evalArrTest s arr
eval (In _ _)                        = fail $ "Incorrect membership test syntax"
eval (Logic op le re)                = evalLogic op le re
eval (Match s re)                    = evalMatch s re
eval (NoMatch s re)                  = evalNoMatch s re
eval (FunCall "atan2"  [vy, vx])     = evalAtan2 vy vx
eval (FunCall "cos"    [vx])         = proxyFcn cos vx
eval (FunCall "exp"    [vx])         = proxyFcn exp vx
eval (FunCall "int"    [vx])         = proxyFcn (fromIntegral . truncate) vx
eval (FunCall "log"    [vx])         = proxyFcn log vx
eval (FunCall "sin"    [vx])         = proxyFcn sin vx
eval (FunCall "sqrt"   [vx])         = proxyFcn sqrt vx
eval (FunCall "srand"  vss)          = evalSRand vss
eval (FunCall "rand"   [])           = evalRand
eval (FunCall "index"  [vs, vt])     = evalIndex vs vt
eval (FunCall "length" [vs])         = evalLength vs
eval (FunCall "split"  [vs, (VariableRef a)])      = evalSplitFS vs a
eval (FunCall "split"  [vs, (VariableRef a), vfs]) = evalSplitVar vs a vfs
eval (FunCall "substr" [vs, vp])     = evalSubstr vs vp
eval (FunCall "substr" [vs, vp, vn]) = evalSubstr2 vs vp vn
eval (FunCall "gsub"   [vr, vs])     = evalGSub vr vs
eval (FunCall "gsub"   [vr, vs, vt]) = evalGSubVar vr vs vt
eval (FunCall "sub"    [vr, vs])     = evalSub vr vs
eval (FunCall "sub"    [vr, vs, vt]) = evalSubVar vr vs vt
eval (FunCall "match"  [vs, vr])     = evalFMatch vs vr
eval (FunCall f args)                = undefined --evalFunCall f args
eval (Assignment op p v)             = evalAssign op p v

proxyFcn :: (Double -> Double) -> Expression -> Interpreter Value
proxyFcn f e = do
     d <- liftM toDouble $ eval e
     return $! VDouble $ f d

assignToField :: ModOp -> Value -> Value -> Interpreter ()
assignToField op vi val = do
     let i = toInt vi
     if i == 0
     then do
          thisLine <- gets hcThisLine
          let newLine    = calcNewValue (valstr thisLine) op val
              newLineStr = toString newLine
          modify (\s -> s {hcThisLine = newLineStr})
          reconstructThisFields newLineStr
     else do
          oldFields <- gets hcFields
          let newValue  = calcNewValue (oldFields *!! i) op val
              newFields = IM.insert i newValue oldFields
          modify (\s -> s { hcFields = newFields })
          reconstructThisLine

splitIntoFields :: B.ByteString -> Interpreter [B.ByteString]
splitIntoFields str = splitIntoFields' <$> (liftM toString $ gets hcFS) <*> pure str 

reconstructThisLine :: Interpreter ()
reconstructThisLine = do
     thisFields <- gets (IM.toList . hcFields)
     ofs        <- liftM toString $ eval (BuiltInVar OFS)
     let line = B.intercalate ofs $ map (toString . snd) thisFields
     modify (\s -> s { hcThisLine = line })
     return ()

reconstructThisFields :: B.ByteString -> Interpreter ()
reconstructThisFields l = do
    oldContext <- get
    thisFields <- liftM (map valstr) $ splitIntoFields l
    let thisFldMap = IM.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcFields = thisFldMap }
    put $! thisContext

-- TODO: delete
assignToRef op ref val = do
     r <- liftIO $ atomicModifyIORef' ref (\v -> let nv = calcNewValue v op val in (nv, nv))
     return $! r

-- TODO: remove
assignToBVar op name val = do
   modBVar name (\oldVal -> calcNewValue oldVal op val)
   evalBVariableRef name

assignToArr op arr ref val = do
   oldArrs <- gets hcArrays
   subscr  <- liftM toString $ eval ref
   let index     = (arr, B.unpack subscr)
       newValue  = calcNewValue (oldArrs *! index) op val
       newArrays = M.insert index newValue oldArrs
   modify $ (\s -> s { hcArrays = newArrays })
   return $! newValue


ppval :: Double -> Double -> Notation -> Value
ppval old new Post = VDouble old
ppval old new Pre  = VDouble new

ppval' old new Post =  old
ppval' old new Pre  =  new


-- incrField n fld@(FieldRef e) = do
--    (VDouble d) <- assignToField ModAdd e (VDouble 1.0)
--    return $! ppval (d-1) d n

-- decrField n fld@(FieldRef e) = do
--    (VDouble d) <- assignToField ModSub e (VDouble 1.0)
--    return $! ppval (d+1) d n

incrVar n var@(Variable s) = do
   (VDouble d) <- assignToRef ModAdd s (VDouble 1.0)
   return $! ppval (d-1) d n

decrVar n var@(Variable s) = do
   (VDouble d) <- assignToRef ModSub s (VDouble 1.0)
   return $! ppval (d+1) d n

incrArr n arr@(ArrayRef name ref) = do
   (VDouble d) <- assignToArr ModAdd name ref (VDouble 1.0)
   return $! ppval (d-1) d n

decrArr n arr@(ArrayRef name ref) = do
   (VDouble d) <- assignToArr ModSub name ref (VDouble 1.0)
   return $! ppval (d+1) d n

-- Execute a statement
exec = undefined
-- exec :: KBlock -> Statement -> Interpreter ()
-- exec _ (Expression e) = {-# SCC "execEXPR"  #-} eval e >> return ()
-- exec k (Block es)     = {-# SCC "execBLOCK" #-} mapM_ (exec k) es
-- exec k (IF c t me)    = {-# SCC "execIF"    #-} execIF k c t me
-- exec k w@(WHILE c s)  = {-# SCC "execWHILE" #-} execWHILE k w c s 
-- exec k (FOR i c st s) = {-# SCC "execFOR"   #-} execFOR k i c st s
-- exec k d@(DO s c)     = {-# SCC "execDO"    #-} execDO k d s c 
-- exec k f@(FOREACH v@(Variable ref) arr st) = {-# SCC "execFE" #-} execFOREACH k f v ref arr st
-- exec _ (PRINT es)     = {-# SCC "execPRINT" #-} execPRINT es
-- exec k (NEXT)         = {-# SCC "execNEXT"  #-} (kNext  k) ()
-- exec k (EXIT _)       = {-# SCC "execEXIT"  #-} (kExit  k) () -- TODO argument
-- exec k (RETURN me)    = {-# SCC "execRET"   #-} execRET k me
-- exec _ (NOP)          = {-# SCC "execNOP"   #-} return ()
-- exec _ (DELETE e)     = {-# SCC "execDEL"   #-} execDEL e


evalArith op le re = calcArith <$> eval le <*> eval re <*> pure op

evalFieldRef e = do
     i <- liftM toInt $ eval e
     if i == 0
     then gets hcThisLine >>= (return . valstr)
     else do fs <- gets hcFields
             return $! fs IM.! i

evalBVariableRef ARGC     = gets hcARGC    
evalBVariableRef ARGV     = gets hcARGV    
evalBVariableRef FILENAME = gets hcFILENAME
evalBVariableRef FNR      = gets hcFNR     
evalBVariableRef FS       = gets hcFS      
evalBVariableRef NF       = gets hcNF      
evalBVariableRef NR       = gets hcNR      
evalBVariableRef OFMT     = gets hcOFMT    
evalBVariableRef OFS      = gets hcOFS     
evalBVariableRef ORS      = gets hcORS     
evalBVariableRef RLENGTH  = gets hcRLENGTH 
evalBVariableRef RS       = gets hcRS      
evalBVariableRef RSTART   = gets hcRSTART  
evalBVariableRef SUBSEP   = gets hcSUBSEP  

modBVar ARGC     f = modify $ \s -> s { hcARGC    = f (hcARGC     s)}
modBVar ARGV     f = modify $ \s -> s { hcARGV    = f (hcARGV     s)}
modBVar FILENAME f = modify $ \s -> s { hcFILENAME= f (hcFILENAME s)}
modBVar FNR      f = modify $ \s -> s { hcFNR     = f (hcFNR      s)}
modBVar FS       f = modify $ \s -> s { hcFS      = f (hcFS       s)}
modBVar NF       f = modify $ \s -> s { hcNF      = f (hcNF       s)}
modBVar NR       f = modify $ \s -> s { hcNR      = f (hcNR       s)}
modBVar OFMT     f = modify $ \s -> s { hcOFMT    = f (hcOFMT     s)}
modBVar OFS      f = modify $ \s -> s { hcOFS     = f (hcOFS      s)}
modBVar ORS      f = modify $ \s -> s { hcORS     = f (hcORS      s)}
modBVar RLENGTH  f = modify $ \s -> s { hcRLENGTH = f (hcRLENGTH  s)}
modBVar RS       f = modify $ \s -> s { hcRS      = f (hcRS       s)}
modBVar RSTART   f = modify $ \s -> s { hcRSTART  = f (hcRSTART   s)}
modBVar SUBSEP   f = modify $ \s -> s { hcSUBSEP  = f (hcSUBSEP   s)}


evalArrRef s e = do
     idx <- liftM toString $! eval e
     ars <- gets hcArrays
     return $! ars *! (s, B.unpack idx)

evalCmp op le re = cmpValues <$> eval le <*> eval re <*> pure op

evalNot e = do
     b <- liftM toBool $! eval e
     return $! VDouble (if b then 0.0 else 1.0)

evalNeg e = do
     d <- liftM toDouble $! eval e
     return $! VDouble (- d)


-- In a membership test, array name is parsed as an ordinary variable reference.
-- TODO: Check in grammar
evalArrTest s arr = do
     arrs   <- gets hcArrays
     subscr <- liftM toString $ eval s
     return $! VDouble $ test (M.member (arr,B.unpack subscr) arrs)
  where test b = if b then 1 else 0

evalLogic op le re = calcLogic <$> pure op <*> eval le <*> eval re

evalMatch s re = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 1.0 else 0.0
     return $! VDouble rv

evalNoMatch s re = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 0.0 else 1.0
     return $! VDouble rv

evalAtan2  vx vy = calcAtan2  <$> eval vy <*> eval vx
evalIndex  vs vt = calcIndex  <$> eval vs <*> eval vt
evalLength vs    = calcLength <$> eval vs

evalSRand vss = do
     g <- case vss of
       [vs] -> liftM (mkStdGen . toInt) $! eval vs
       []   -> liftIO getStdGen
     modify $ (\s -> s { hcStdGen = g })
     return $! VDouble 0 -- TODO: srand return value?

evalRand = do
     g <- gets hcStdGen
     let (r, g') = randomR (0.0, 1.0) g
     modify $ (\s -> s { hcStdGen = g' })
     return $! VDouble r

evalSplitFS vs a = do
     fs <- liftM toString $ eval (BuiltInVar FS)
     evalSplit vs fs a

evalSplitVar vs a vfs = do
     fs <- liftM toString $ eval vfs
     evalSplit vs fs a

evalSplit :: Expression -> B.ByteString -> String -> Interpreter Value
evalSplit vs fs arr = do
   s <- liftM toString $ eval vs
   let ss = s `splitWithSep` fs
       is = [1, 2..]
   ars <- gets hcArrays
   let -- at first, clear the array from its previous contents
       -- TODO: very slow, when we have all arrays in a single Data.Map
       ars'  = M.filterWithKey (\(a,_) _ -> a /= arr) ars
       -- Form a new array containing extracted values
       keys  = map (arr,)  $ (map show is)
       strs  = map valstr  $ ss
       res   = M.fromList $ zip keys strs
       -- Put our new values then
       ars'' = M.union ars' res
   modify $ (\s -> s { hcArrays = ars'' })
   return $! VDouble $ fromIntegral $ length ss

evalSubstr  vs vp = calcSubstr <$> eval vs <*> eval vp
evalSubstr2 vs vp vn = calcSubstr2 <$> eval vs <*> eval vp <*> eval vn

evalGSub vr vs = do
     (m, str) <- calcGSub <$> eval vr <*> eval vs <*> gets hcThisLine
     modify $ \s -> s { hcThisLine = str }
     reconstructThisFields str
     return $! VDouble $ fromIntegral m

evalGSubVar vr vs vt = do
     (m, str) <- calcGSub <$> eval vr <*> eval vs <*> (liftM toString $ eval vt)
     let vstr = valstr str
     case vt of
        (Variable ref)     -> assignToRef   ModSet ref vstr
        (FieldRef ref)     -> undefined -- assignToField ModSet ref vstr
        (ArrayRef arr ref) -> assignToArr   ModSet arr ref vstr 
     return $! VDouble $ fromIntegral m

evalSub vr vs = do
     (n, str) <- calcSub <$> eval vr <*> eval vs <*> gets hcThisLine
     if n == 0
     then do return $! VDouble 0
     else do modify (\s -> s { hcThisLine = str })
             reconstructThisFields str
             return $! VDouble 1

evalSubVar vr vs vt = do
     (n, str) <- calcSub <$> eval vr <*> eval vs <*> (liftM toString $ eval vt)
     if n == 0
     then do return $! VDouble 0
     else do let result = valstr str
             case vt of
                (Variable ref)     -> assignToRef   ModSet ref result
                (FieldRef ref)     -> undefined -- assignToField ModSet ref result
                (ArrayRef arr ref) -> assignToArr   ModSet arr ref result 
             return $! VDouble 1

evalFMatch vs vr = do
     (retS, retL) <- calcMatch <$> eval vs <*> eval vr
     modify $ \s -> s { hcRSTART = retS, hcRLENGTH = retL }
     return $! retS

-- evalFunCall f args = do
--      mfcn <- liftM (find (func f)) $ gets hcCode
--      case mfcn of
--        (Just (Function _ argNames stmt)) -> do
--            -- Build a stack frame for function call first
--            argVals <- mapM eval args
--            let numArgs = length argNames
--                numVals = length argVals
--                numLocs = numArgs - numVals

--                boundArgs = zip argNames argVals
--                localVars = if numLocs > 0
--                            then zip (drop numVals argNames) $ repeat (VDouble 0)
--                            else []
--                newStackFrame = M.fromList $! boundArgs ++ localVars

--            oldStack <- gets hcStack
--            modify $ (\s -> s { hcStack = newStackFrame:oldStack, hcRetVal = VDouble 0 })
--            callCC $ \ret -> do
--               let retHook (Just v) = modify (\s -> s { hcRetVal = v }) >> ret ()
--                   retHook Nothing  = ret ()
--                   k = seq retHook $ emptyKBlock {kRet = retHook}
--               exec k stmt
--            modify $ (\s -> s { hcStack = oldStack })
--            gets hcRetVal
--        Nothing    -> fail $ f ++ " - unknown function"
--        otherwise  -> fail $ "Fatal error when invoking function " ++ f
--   where
--      func s (Function ss _ _) = s == ss
--      func s _                 = False

evalAssign op p v = do
     val <- eval v
     case p of
       (FieldRef ref)     -> undefined -- assignToField op ref  val
       (Variable ref)     -> assignToRef   op ref val
       (ArrayRef arr ref) -> assignToArr   op arr ref val
       (BuiltInVar name)  -> assignToBVar  op name val
       otherwise -> fail "Only to-field and to-variable assignments are supported"

-- -- TODO: The order in which the keys will be traversed may be suprising
-- execFOREACH k f v vname arr st = do
--      arrData <- liftM (filter inArray . map fst . M.toList) $ gets hcArrays
--      callCC $ \br -> do
--        let k' = k {kBreak = br}
--            nextFor kk []         = br ()
--            nextFor kk ((_,s):ss) = do
--              assignToRef ModSet vname (valstr $ B.pack s)
--              let kk' = kk {kCont = \_ -> nextFor kk' (tail ss)}
--              seq kk' $ exec kk' st
--              nextFor kk' ss
--        nextFor k' arrData
--        br ()
--    where inArray (a, _) = a == arr

-- execRET k me = case me of
--       Nothing   -> (kRet k) Nothing
--       Just expr -> eval expr >>= (kRet k . Just)

execDEL e = case e of
    (ArrayRef arr idx) -> do
       oldArrs <- gets hcArrays
       subscr  <- liftM toString $ eval idx
       let index = (arr, B.unpack subscr)
       modify $ \s -> s {hcArrays = M.delete index oldArrs}
       return ()
    otherwise -> fail $ "Syntax error: delete element"
