{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter
             (Interpreter
             , HawkContext(..)
             , (*!)
             , (*!!)
             , splitIntoFields
             , modBVar
             , evalBVariableRef
             , assignToField
             , runInterpreter
             , emptyContext
             ) where

import qualified Data.ByteString.Char8 as B
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (liftM)
-- import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import System.Random (StdGen, mkStdGen)
import System.IO (Handle)
import System.Process (ProcessHandle)
import Lang.Hawk.Basic
import Lang.Hawk.Value
import Lang.Hawk.Bytecode (OpCode)
import Lang.Hawk.Runtime (calcArith, splitIntoFields')
import Lang.Hawk.Runtime.Input (Record, InputSource)

import Data.IORef
import System.IO.Unsafe

data HawkContext = HawkContext
                 { hcInput    :: IORef InputSource
                 , hcWorkload :: IORef [Record]

                 , hcFields   :: IORef (IM.IntMap Value)
                 , hcHandles  :: IORef (M.Map B.ByteString Handle)
                 , hcPHandles :: IORef (M.Map B.ByteString (ProcessHandle,Handle))
                 , hcIPHandles:: IORef (M.Map B.ByteString (ProcessHandle,InputSource))
                 , hcFInputs  :: IORef (M.Map B.ByteString InputSource)

                 , hcThisLine :: IORef B.ByteString
                 , hcStdGen   :: IORef StdGen

                 , hcOPCODES  :: IORef [OpCode]

                 , hcKEYS     :: IORef [String]
                 , hcKSTACK   :: IORef [[String]]

                 , hcARGC     :: IORef Value
                 , hcARGV     :: IORef Value
                 , hcFILENAME :: IORef Value
                 , hcFNR      :: IORef Value
                 , hcFS       :: IORef Value
                 , hcNF       :: IORef Value
                 , hcNR       :: IORef Value
                 , hcOFMT     :: IORef Value
                 , hcOFS      :: IORef Value
                 , hcORS      :: IORef Value
                 , hcRLENGTH  :: IORef Value
                 , hcRS       :: IORef Value
                 , hcRSTART   :: IORef Value
                 , hcSUBSEP   :: IORef Value
                 }

emptyContext :: [OpCode] -> InputSource -> IO HawkContext
{-# INLINE emptyContext #-}
emptyContext opcodes i = return $! HawkContext
           { hcInput    = unsafePerformIO $ newIORef $ i
           , hcWorkload = unsafePerformIO $ newIORef $ []

           , hcFields   = unsafePerformIO $ newIORef $ IM.empty
           , hcHandles  = unsafePerformIO $ newIORef $ M.empty
           , hcPHandles = unsafePerformIO $ newIORef $ M.empty
           , hcIPHandles= unsafePerformIO $ newIORef $ M.empty
           , hcFInputs  = unsafePerformIO $ newIORef $ M.empty

           , hcThisLine = unsafePerformIO $ newIORef $ ""
           , hcStdGen   = unsafePerformIO $ newIORef $ mkStdGen 0

           , hcOPCODES  = unsafePerformIO $ newIORef $ opcodes

           , hcKEYS     = unsafePerformIO $ newIORef $ []
           , hcKSTACK   = unsafePerformIO $ newIORef $ []

           , hcARGC     = unsafePerformIO $ newIORef $ defstr  ""
           , hcARGV     = unsafePerformIO $ newIORef $ defstr  ""
           , hcFILENAME = unsafePerformIO $ newIORef $ defstr  ""
           , hcFNR      = unsafePerformIO $ newIORef $ VDouble 0
           , hcFS       = unsafePerformIO $ newIORef $ defstr  " "
           , hcNF       = unsafePerformIO $ newIORef $ VDouble 0
           , hcNR       = unsafePerformIO $ newIORef $ VDouble 0
           , hcOFMT     = unsafePerformIO $ newIORef $ defstr  "%.6f"
           , hcOFS      = unsafePerformIO $ newIORef $ defstr  " "
           , hcORS      = unsafePerformIO $ newIORef $ defstr  "\n"
           , hcRLENGTH  = unsafePerformIO $ newIORef $ VDouble  0
           , hcRS       = unsafePerformIO $ newIORef $ defstr  "\n"
           , hcRSTART   = unsafePerformIO $ newIORef $ VDouble 0
           , hcSUBSEP   = unsafePerformIO $ newIORef $ defstr  "\034"
           }

type Interpreter a = IO a

runInterpreter :: (HawkContext -> Interpreter a) -> HawkContext -> IO a
runInterpreter stt c = stt c

defaultValue :: Value
defaultValue = valstr ""

(*!) :: Ord k => M.Map k Value -> k -> Value
m *! k = M.findWithDefault defaultValue k m

(*!!) :: IM.IntMap Value -> Int -> Value
m *!! k = IM.findWithDefault defaultValue k m

assignToField :: HawkContext -> ArithOp -> Value -> Value -> Interpreter ()
assignToField ctx op vi val = do
     let i = toInt vi
     if i == 0
     then do
          thisLine <- readIORef (hcThisLine ctx)
          let newLine    = calcArith (valstr thisLine) val op
              newLineStr = toString newLine
          writeIORef (hcThisLine ctx) newLineStr
          reconstructThisFields ctx newLineStr
     else do
          oldFields <- readIORef (hcFields ctx)
          let newValue  = calcArith (oldFields *!! i) val op
              newFields = IM.insert i newValue oldFields
          writeIORef (hcFields ctx) newFields
          reconstructThisLine ctx

splitIntoFields :: HawkContext -> B.ByteString -> Interpreter [B.ByteString]
splitIntoFields ctx str = do
   fs <- liftM toString $ readIORef (hcFS ctx)
   return $ splitIntoFields' fs str


reconstructThisLine :: HawkContext -> Interpreter ()
reconstructThisLine ctx = do
     thisFields <- liftM IM.toList $ readIORef (hcFields ctx)
     ofs        <- liftM toString  $ readIORef (hcOFS ctx)
     let line = B.intercalate ofs $ map (toString . snd) thisFields
     writeIORef (hcThisLine ctx) line
     return ()

reconstructThisFields :: HawkContext -> B.ByteString -> Interpreter ()
reconstructThisFields ctx l = do
    thisFields <- liftM (map valstr) $ splitIntoFields ctx l
    let thisFldMap = IM.fromList (zip [1,2..] thisFields)
    writeIORef (hcFields ctx) thisFldMap


evalBVariableRef :: HawkContext -> BVar -> Interpreter Value
{-# INLINE evalBVariableRef #-}
evalBVariableRef ctx ARGC     = readIORef (hcARGC     ctx)
evalBVariableRef ctx ARGV     = readIORef (hcARGV     ctx)
evalBVariableRef ctx FILENAME = readIORef (hcFILENAME ctx)
evalBVariableRef ctx FNR      = readIORef (hcFNR      ctx)
evalBVariableRef ctx FS       = readIORef (hcFS       ctx)
evalBVariableRef ctx NF       = readIORef (hcNF       ctx)
evalBVariableRef ctx NR       = readIORef (hcNR       ctx)
evalBVariableRef ctx OFMT     = readIORef (hcOFMT     ctx)
evalBVariableRef ctx OFS      = readIORef (hcOFS      ctx)
evalBVariableRef ctx ORS      = readIORef (hcORS      ctx)
evalBVariableRef ctx RLENGTH  = readIORef (hcRLENGTH  ctx)
evalBVariableRef ctx RS       = readIORef (hcRS       ctx)
evalBVariableRef ctx RSTART   = readIORef (hcRSTART   ctx)
evalBVariableRef ctx SUBSEP   = readIORef (hcSUBSEP   ctx)

modBVar :: HawkContext -> BVar -> (Value -> Value) -> Interpreter ()
{-# INLINE modBVar #-}
modBVar ctx ARGC     f = modifyIORef' (hcARGC     ctx) f 
modBVar ctx ARGV     f = modifyIORef' (hcARGV     ctx) f 
modBVar ctx FILENAME f = modifyIORef' (hcFILENAME ctx) f 
modBVar ctx FNR      f = modifyIORef' (hcFNR      ctx) f 
modBVar ctx FS       f = modifyIORef' (hcFS       ctx) f 
modBVar ctx NF       f = modifyIORef' (hcNF       ctx) f 
modBVar ctx NR       f = modifyIORef' (hcNR       ctx) f 
modBVar ctx OFMT     f = modifyIORef' (hcOFMT     ctx) f 
modBVar ctx OFS      f = modifyIORef' (hcOFS      ctx) f 
modBVar ctx ORS      f = modifyIORef' (hcORS      ctx) f 
modBVar ctx RLENGTH  f = modifyIORef' (hcRLENGTH  ctx) f 
modBVar ctx RS       f = modifyIORef' (hcRS       ctx) f 
modBVar ctx RSTART   f = modifyIORef' (hcRSTART   ctx) f 
modBVar ctx SUBSEP   f = modifyIORef' (hcSUBSEP   ctx) f 


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
