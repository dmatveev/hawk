{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter
             ( Interpreter
             , HawkContext
             , HawkContextPure(..)
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

data HawkContextPure = HawkContextPure
          { hcInput    :: !InputSource
          , hcWorkload :: ![Record]

          , hcFields   :: (IM.IntMap Value)
          , hcHandles  :: !(M.Map B.ByteString Handle)
          , hcPHandles :: !(M.Map B.ByteString (ProcessHandle,Handle))
          , hcIPHandles:: !(M.Map B.ByteString (ProcessHandle,InputSource))
          , hcFInputs  :: !(M.Map B.ByteString InputSource)

          , hcThisLine :: !B.ByteString
          , hcStdGen   :: StdGen

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

type HawkContext = IORef HawkContextPure

emptyContext :: [OpCode] -> InputSource -> IO HawkContext
{-# INLINE emptyContext #-}
emptyContext opcodes i = newIORef $! HawkContextPure
           { hcInput    = i
           , hcWorkload = []

           , hcFields   = IM.empty
           , hcHandles  = M.empty
           , hcPHandles = M.empty
           , hcIPHandles= M.empty
           , hcFInputs  = M.empty

           , hcThisLine = ""
           , hcStdGen   = mkStdGen 0

           , hcOPCODES  = opcodes

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
          thisLine <- liftM hcThisLine $ readIORef ctx
          let newLine    = calcArith (valstr thisLine) val op
              newLineStr = toString newLine
          modifyIORef' ctx $ \s -> s {hcThisLine = newLineStr}
          reconstructThisFields ctx newLineStr
     else do
          oldFields <- liftM hcFields $ readIORef ctx
          let newValue  = calcArith (oldFields *!! i) val op
              newFields = IM.insert i newValue oldFields
          modifyIORef' ctx $ \s -> s {hcFields = newFields}
          reconstructThisLine ctx

splitIntoFields :: HawkContext -> B.ByteString -> Interpreter [B.ByteString]
splitIntoFields ctx str = do
   fs <- liftM (toString.hcFS) $ readIORef ctx
   return $ splitIntoFields' fs str


reconstructThisLine :: HawkContext -> Interpreter ()
reconstructThisLine ctx = do
     cc <- readIORef ctx
     let thisFields =  IM.toList $ hcFields cc
         ofs        =  toString  $ hcOFS cc
         line = B.intercalate ofs $ map (toString . snd) thisFields
     modifyIORef' ctx $ \s -> s {hcThisLine = line}
     return ()

reconstructThisFields :: HawkContext -> B.ByteString -> Interpreter ()
reconstructThisFields ctx l = do
    thisFields <- liftM (map valstr) $ splitIntoFields ctx l
    let thisFldMap = IM.fromList (zip [1,2..] thisFields)
    modifyIORef' ctx $ \s -> s {hcFields = thisFldMap}


evalBVariableRef :: HawkContext -> BVar -> Interpreter Value
{-# INLINE evalBVariableRef #-}
evalBVariableRef ctx ARGC     = liftM hcARGC     $ readIORef ctx
evalBVariableRef ctx ARGV     = liftM hcARGV     $ readIORef ctx
evalBVariableRef ctx FILENAME = liftM hcFILENAME $ readIORef ctx
evalBVariableRef ctx FNR      = liftM hcFNR      $ readIORef ctx
evalBVariableRef ctx FS       = liftM hcFS       $ readIORef ctx
evalBVariableRef ctx NF       = liftM hcNF       $ readIORef ctx
evalBVariableRef ctx NR       = liftM hcNR       $ readIORef ctx
evalBVariableRef ctx OFMT     = liftM hcOFMT     $ readIORef ctx
evalBVariableRef ctx OFS      = liftM hcOFS      $ readIORef ctx
evalBVariableRef ctx ORS      = liftM hcORS      $ readIORef ctx
evalBVariableRef ctx RLENGTH  = liftM hcRLENGTH  $ readIORef ctx
evalBVariableRef ctx RS       = liftM hcRS       $ readIORef ctx
evalBVariableRef ctx RSTART   = liftM hcRSTART   $ readIORef ctx
evalBVariableRef ctx SUBSEP   = liftM hcSUBSEP   $ readIORef ctx

modBVar :: HawkContext -> BVar -> (Value -> Value) -> Interpreter ()
{-# INLINE modBVar #-}
modBVar ctx ARGC     f = modifyIORef' ctx $ \s -> s { hcARGC     = f (hcARGC     s) }
modBVar ctx ARGV     f = modifyIORef' ctx $ \s -> s { hcARGV     = f (hcARGV     s) }
modBVar ctx FILENAME f = modifyIORef' ctx $ \s -> s { hcFILENAME = f (hcFILENAME s) }
modBVar ctx FNR      f = modifyIORef' ctx $ \s -> s { hcFNR      = f (hcFNR      s) }
modBVar ctx FS       f = modifyIORef' ctx $ \s -> s { hcFS       = f (hcFS       s) }
modBVar ctx NF       f = modifyIORef' ctx $ \s -> s { hcNF       = f (hcNF       s) }
modBVar ctx NR       f = modifyIORef' ctx $ \s -> s { hcNR       = f (hcNR       s) }
modBVar ctx OFMT     f = modifyIORef' ctx $ \s -> s { hcOFMT     = f (hcOFMT     s) }
modBVar ctx OFS      f = modifyIORef' ctx $ \s -> s { hcOFS      = f (hcOFS      s) }
modBVar ctx ORS      f = modifyIORef' ctx $ \s -> s { hcORS      = f (hcORS      s) }
modBVar ctx RLENGTH  f = modifyIORef' ctx $ \s -> s { hcRLENGTH  = f (hcRLENGTH  s) }
modBVar ctx RS       f = modifyIORef' ctx $ \s -> s { hcRS       = f (hcRS       s) }
modBVar ctx RSTART   f = modifyIORef' ctx $ \s -> s { hcRSTART   = f (hcRSTART   s) }
modBVar ctx SUBSEP   f = modifyIORef' ctx $ \s -> s { hcSUBSEP   = f (hcSUBSEP   s) }


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
