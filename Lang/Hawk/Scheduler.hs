{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lang.Hawk.Scheduler (run) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
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
  ctxStartup <- (fromHandle h) >>= emptyContext startup
  (cont, ctxAfterStartup) <- runInterpreter wrkInit ctxStartup
  ctxAfterProc <-
     if cont
     then liftM snd $ runInterpreter syncLoop $ ctxAfterStartup { hcOPCODES = actions }
     else return ctxAfterStartup
  runInterpreter wrkFinish $ ctxAfterProc { hcOPCODES = finalize }
  return ()
 where
  syncLoop = do
   is <- gets hcInput
   rs <- liftM toString $ gets hcRS
   ml <- liftIO $ nextLine is rs
   case ml of
     (Just l) -> wrkProcessLine l >>= \cont -> when cont syncLoop
     Nothing  -> return ()

executeIOAsync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeIOAsync startup (CompiledIOAsync actions) finalize h file = do
  q <- newEmptyMVar
  j <- newEmptyMVar
  ctxStartup <- emptyContext startup $ External q
  (cont, ctxAfterStartup) <- runInterpreter wrkInit ctxStartup
  ctxAfterProc <-
      if cont
      then do let rs = toString . hcRS $ ctxAfterStartup
                  fs = toString . hcFS $ ctxAfterStartup
              forkIO $ runReaderThread reader h rs fs q
              liftM snd $ runInterpreter wrkLoop $ ctxAfterStartup { hcOPCODES = actions }
      else return ctxAfterStartup
  runInterpreter wrkFinish $ ctxAfterProc { hcOPCODES = finalize }
  return ()
 
executeFullAsync :: [OpCode] -> CompiledActions -> [OpCode] -> Handle -> String -> IO ()
executeFullAsync = undefined
