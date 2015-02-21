{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lang.Hawk.Scheduler (run) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (liftM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict

import Lang.Hawk.Interpreter
import Lang.Hawk.Value
import Lang.Hawk.Bytecode
import Lang.Hawk.Bytecode.Compiler
import Lang.Hawk.Bytecode.Interpreter
import Lang.Hawk.Runtime
import Lang.Hawk.Runtime.Input

import Control.Concurrent

import System.IO

import System.Process

import Data.IORef

-- In the simplest case, we have two threads:
-- READER - reads the input, parses lines and fields
-- WORKER - takes data from reader, executes the AWK code 
--
-- READER and WORKER communicate via queue(s). The basic queue element is Workload.

inThread :: IO () -> IO ()
inThread io = do
   finish <- newEmptyMVar
   forkFinally io $ \_ -> putMVar finish ()
   takeMVar finish 

data ReaderState = ReaderState
                 { rNR  :: Integer
                 , rWID :: Integer
                 , rH   :: !Handle
                 , rRS  :: !B.ByteString
                 , rFS  :: !B.ByteString
                 , rQ   :: !(MVar (Maybe Workload))
                 , rTmp :: ![Record]
                 }

type ReaderThread a = StateT ReaderState IO a

sendWorkload :: ReaderThread ()
sendWorkload = do
    tmp <- gets rTmp
    if (not $ null tmp)
    then do w <- Workload <$> nextWID <*> (pure $ reverse tmp)
            gets rQ >>= (liftIO . flip putMVar (Just w))
            modify $ \s -> s { rTmp = [] }
    else do qq <- gets rQ
            liftIO $ putMVar qq Nothing
  where nextWID = modify (\s -> s { rWID = succ (rWID s)}) >> gets rWID

enqueue :: B.ByteString -> ReaderThread ()
enqueue l = do
   nid <- nextRecID
   fs  <- gets rFS
   let flds = splitIntoFields' fs  l
       fldm = IM.fromList (zip [1,2..] (map valstr flds))
       newR = Record nid l (length flds) fldm
   modify $ \s -> seq newR $ s { rTmp = newR:(rTmp s) }
   tmpSz <- gets (length . rTmp)
   when (tmpSz >= 10) sendWorkload
  where nextRecID = modify (\s -> s { rNR = succ (rNR s)}) >> gets rNR

reader :: ReaderThread ()
reader = do
     h <- gets rH
     is <- liftIO $ fromHandle h
     rs <- gets rRS
     readLoop is rs
     sendWorkload
     sendWorkload
  where
    readLoop is rs = do
      ml <- liftIO $ nextLine is rs
      case ml of
        (Just l) -> enqueue l >> readLoop is rs
        Nothing  -> return ()

runReaderThread st h rs fs q = execStateT st c >> return () where
   c = ReaderState { rNR = 0, rWID = 0, rH = h, rRS = rs, rFS = fs, rQ = q, rTmp = [] }

run :: CompiledSource -> Handle -> String -> IO ()
run (CompiledSource startup actions finalize) h file = inThread $
    let f = case actions of
             (CompiledSync      _)     -> executeSync
             (CompiledIOAsync   _)     -> executeIOAsync
             (CompiledFullAsync _ _ _) -> executeFullAsync
    in f startup actions finalize h file
   
executeSync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeSync startup (CompiledSync actions) finalize h file = do
  ctx <- (fromHandle h) >>= emptyContext startup
  cont <- runInterpreter wrkInit ctx
  when cont $ do
     modifyIORef' ctx $ \s -> s {hcOPCODES = actions}
     runInterpreter syncLoop ctx
  modifyIORef' ctx $ \s -> s {hcOPCODES = finalize}
  runInterpreter wrkFinish ctx
  return ()
 where
  syncLoop ctx = do
   cc <- readIORef ctx
   ml <- nextLine (hcInput cc) (toString $ hcRS cc)
   case ml of
     (Just l) -> wrkProcessLine ctx l >>= \cont -> when cont $ syncLoop ctx
     Nothing  -> return ()

executeIOAsync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeIOAsync startup (CompiledIOAsync actions) finalize h file = do
  q <- newEmptyMVar
  j <- newEmptyMVar
  ctx <- emptyContext startup $ External q
  cont <- runInterpreter wrkInit ctx
  when cont $ do
     cc <- readIORef ctx
     forkIO $ runReaderThread reader h (toString $ hcRS cc) (toString $ hcFS cc) q
     modifyIORef' ctx $ \s -> s {hcOPCODES = actions}
     runInterpreter wrkLoop $ ctx
  modifyIORef' ctx $ \s -> s {hcOPCODES = finalize}
  runInterpreter wrkFinish ctx
  return ()
 
executeFullAsync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeFullAsync = undefined
