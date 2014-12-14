{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter where

import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F 
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import System.Random
import System.IO (Handle)
import System.Process (ProcessHandle)
import Lang.Hawk.AST
import Lang.Hawk.Basic
import Lang.Hawk.Value
import Lang.Hawk.Bytecode
import Lang.Hawk.Bytecode.Compiler
import Lang.Hawk.Runtime
import Lang.Hawk.Runtime.Input

data HawkContext = HawkContext
                 { hcInput    :: !InputSource
                 , hcWorkload :: ![Record]

                 , hcFields   :: (IM.IntMap Value)
                 , hcHandles  :: !(M.Map B.ByteString Handle)
                 , hcPHandles :: !(M.Map B.ByteString (ProcessHandle,Handle))
                 , hcIPHandles:: !(M.Map B.ByteString (ProcessHandle,InputSource))
                 , hcFInputs  :: !(M.Map B.ByteString InputSource)

                 , hcThisLine :: !B.ByteString
                 , hcStdGen   :: StdGen

                 , hcSTARTUP  :: [OpCode]
                 , hcSHUTDOWN :: [OpCode]
                 , hcOPCODES  :: [OpCode]

                 , hcKEYS     :: ![String]
                 , hcKSTACK   :: ![[String]]

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

emptyContext :: AwkSource -> InputSource -> HawkContext
emptyContext s i = HawkContext
                 { hcInput    = i
                 , hcWorkload = []

                 , hcFields   = IM.empty
                 , hcHandles  = M.empty
                 , hcPHandles = M.empty
                 , hcIPHandles= M.empty
                 , hcFInputs  = M.empty

                 , hcThisLine = ""
                 , hcStdGen   = mkStdGen 0

                 , hcSTARTUP  = F.toList startup
                 , hcOPCODES  = F.toList opcodes
                 , hcSHUTDOWN = F.toList shutdown

                 , hcKEYS     = []
                 , hcKSTACK   = []

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

(*!) :: Ord k => M.Map k Value -> k -> Value
m *! k = M.findWithDefault (VDouble 0) k m

(*!!) :: IM.IntMap Value -> Int -> Value
m *!! k = IM.findWithDefault (VDouble 0) k m

assignToField :: ArithOp -> Value -> Value -> Interpreter ()
assignToField op vi val = do
     let i = toInt vi
     if i == 0
     then do
          thisLine <- gets hcThisLine
          let newLine    = calcArith (valstr thisLine) val op
              newLineStr = toString newLine
          modify (\s -> s {hcThisLine = newLineStr})
          reconstructThisFields newLineStr
     else do
          oldFields <- gets hcFields
          let newValue  = calcArith (oldFields *!! i) val op
              newFields = IM.insert i newValue oldFields
          modify (\s -> s { hcFields = newFields })
          reconstructThisLine

splitIntoFields :: B.ByteString -> Interpreter [B.ByteString]
splitIntoFields str = splitIntoFields' <$> (liftM toString $ gets hcFS) <*> pure str 


reconstructThisLine :: Interpreter ()
reconstructThisLine = do
     thisFields <- gets (IM.toList . hcFields)
     ofs        <- liftM toString $ gets hcOFS
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


evalBVariableRef :: BVar -> Interpreter Value
{-# INLINE evalBVariableRef #-}
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

modBVar :: BVar -> (Value -> Value) -> Interpreter ()
{-# INLINE modBVar #-}
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

intSRand :: Interpreter ()
intSRand = liftIO getStdGen >>= \g -> modify (\s -> s {hcStdGen = g})

intSRand' :: Value -> Interpreter ()
intSRand' i = modify (\s -> s {hcStdGen = mkStdGen (toInt i)})

evalRand :: Interpreter Value
evalRand = do
     g <- gets hcStdGen
     let (r, g') = randomR (0.0, 1.0) g
     modify $ (\s -> s { hcStdGen = g' })
     return $! VDouble r

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

-- execRET k me = case me of
--       Nothing   -> (kRet k) Nothing
--       Just expr -> eval expr >>= (kRet k . Just)
