{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, CPP #-}

module Lang.Hawk.Scheduler (run) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (liftM, forM, forM_, replicateM_, when)
import Control.Monad.IO.Class (liftIO)

import Lang.Hawk.Interpreter
import Lang.Hawk.Value
import Lang.Hawk.Analyzer (copyValues)
import Lang.Hawk.Bytecode
import Lang.Hawk.Bytecode.Compiler
import Lang.Hawk.Bytecode.Interpreter
import Lang.Hawk.Runtime
import Lang.Hawk.Runtime.Input
import Lang.Hawk.Runtime.Reader
import Lang.Hawk.Runtime.Output

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

run :: CompiledSource -> Handle -> String -> IO ()
run (CompiledSource startup actions finalize) h file = inThread $
    let f = case actions of
             (CompiledSync      _)     -> executeSync
             (CompiledIOAsync   _)     -> executeIOAsync
             (CompiledFullAsync _ _ _) -> executeFullAsync
    in f startup actions finalize h file
   
executeSync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeSync startup (CompiledSync actions) finalize h file = do
  out <- mkOutput
  outSink <- mkNonBufferedSink out
  forkIO $ runWriterThread out
  inp <- fromHandle h
  ctx <- emptyContext startup inp outSink
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
  out <- mkOutput
  outSink <- mkNonBufferedSink out
  outSync <- newEmptyMVar
  forkIO $ runWriterThread out >> putMVar outSync ()
  ctx <- emptyContext startup (External q) outSink
  cont <- runInterpreter wrkInit ctx
  when cont $ do
     cc <- readIORef ctx
     forkIO $ runReaderThread reader h (toString $ hcRS cc) (toString $ hcFS cc) (MVarSink q)
     modifyIORef' ctx $ \s -> s {hcOPCODES = actions}
     runInterpreter wrkLoop $ ctx
     flush outSink
  modifyIORef' ctx $ \s -> s {hcOPCODES = finalize}
  runInterpreter wrkFinish ctx
  closeOutput out
  takeMVar outSync
  return ()
 
executeFullAsync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeFullAsync startup (CompiledFullAsync rt rts nactions) finalize h file = do
#ifdef TRACE
  putStrLn $ "SCHED: FULL ASYNC GO!"
#endif
  out     <- mkSerialOutput
  outSink <- mkNonBufferedSink out
  outSync <- newEmptyMVar
  forkIO $ runWriterThread out >> putMVar outSync ()
  q       <- newEmptyMVar
  ctx     <- emptyContext startup (External q) outSink
  cont    <- runInterpreter wrkInit ctx
  when cont $ do
    cc    <- readIORef ctx
    forkIO $ runReaderThread reader h (toString $ hcRS cc) (toString $ hcFS cc) (MVarSink q)
    mvrs  <- forM (zip rts nactions) $ \(rt', actions) -> do
      copyValues rt rt'
      outSink' <- mkBufferedSink out
      ctx' <- newIORef $ cc { hcOPCODES = actions, hcOutput = outSink' }
      mvr  <- newEmptyMVar
      forkOS $ do
#ifdef TRACE
        putStrLn "SCHED: Launching a new thread..."
#endif
        runInterpreter wrkLoop ctx'
        flush outSink'
        putMVar mvr ()
      return mvr
#ifdef TRACE
    putStrLn "SCHED: Waiting for completion..."
#endif
    mapM_ takeMVar mvrs
    -- TODO: find the thread which processed the last workload, copy variable data from there
  modifyIORef' ctx $ \s -> s {hcOPCODES = finalize}
  runInterpreter wrkFinish ctx
  closeOutput out
  takeMVar outSync
  return ()
