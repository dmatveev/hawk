{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lang.Hawk.Scheduler where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Exception.Base (evaluate)
import Control.Monad.State.Strict

import Lang.Hawk.AST
import Lang.Hawk.Analyzer
import Lang.Hawk.Interpreter
import Lang.Hawk.Value

import Control.Concurrent

import System.IO

-- In the simplest case, we have two threads:
-- READER - reads the input, parses lines and fields
-- WORKER - takes data from reader, executes the AWK code 
--
-- READER and WORKER communicate via queue(s). The basic queue element is Workload.

data Record   = Record !Integer !B.ByteString ![B.ByteString] deriving Show
data Workload = Workload { wID :: !Integer, wRS :: ![Record] } deriving Show

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
                 , rQ   :: !(MVar [Workload])
                 , rTmp :: ![Record]
                 }

newtype ReaderThread a = ReaderThread (StateT ReaderState IO a)
                         deriving (Monad, MonadIO, MonadState ReaderState,
                                   Applicative, Functor)

sendWorkload :: ReaderThread ()
sendWorkload = do
    tmp <- gets rTmp
    if (not $ null tmp)
    then do
      wid <- nextWID
      let w = Workload wid (reverse tmp)
      qq <- gets rQ
      liftIO $ putMVar qq [w]
      modify $ \s -> s { rTmp = [] }
    else do
      qq <- gets rQ
      liftIO $ putMVar qq []
  where nextWID = modify (\s -> s { rWID = succ (rWID s)}) >> gets rWID

enqueue :: B.ByteString -> ReaderThread ()
enqueue l = do
   nid <- nextRecID
   fs  <- gets rFS
   let flds = splitIntoFields' fs  l
       newR = Record nid l flds
   modify $ \s -> seq newR $ s { rTmp = newR:(rTmp s) }
   tmpSz <- gets (length . rTmp)
   when (tmpSz >= 20) sendWorkload
  where nextRecID = modify (\s -> s { rNR = succ (rNR s)}) >> gets rNR

reader :: ReaderThread ()
reader = do
     h <- gets rH
     rs <- gets rRS
     readLoop h rs B.empty False
     sendWorkload
     sendWorkload
  where
    readLoop h rs thisBuf eof = do
      let nrs = B.length rs
      case B.breakSubstring rs thisBuf of
         (l, rest) | B.null l && B.null rest && eof -> return ()
                   |             B.null rest && eof -> enqueue l
                   | B.null rest && not eof -> do
                        nextChunk <- liftIO $ B.hGet h 8192
                        readLoop h rs (B.append thisBuf nextChunk) (B.null nextChunk)  
                   | otherwise -> enqueue l >> readLoop h rs (B.drop nrs rest) eof

runReaderThread (ReaderThread st) h rs fs q = execStateT st c where
   c = ReaderState { rNR = 0, rWID = 0, rH = h, rRS = rs, rFS = fs, rQ = q, rTmp = [] }

worker :: AwkSource -> MVar [Workload] -> IO ()
worker src mq = do
   ctx  <- runInterpreter wrkInit (emptyContext src)
   ctx' <- workerLoop ctx
   runInterpreter finalize ctx'
   return ()
 where
   workerLoop ctx = do
      w <- takeMVar mq
      jobLoop ctx w

   jobLoop ctx []     = return ctx
   jobLoop ctx (w:ws) = procLoop ctx (wRS w)

   procLoop ctx []     = workerLoop ctx
   procLoop ctx (r:rs) = do ctx' <- runInterpreter (wrkProc r) ctx
                            procLoop ctx' rs

   wrkInit = do
    -- assignToBVar ModSet FILENAME (valstr $ B.pack inputFile)
    assignToBVar ModSet FNR      (VDouble 0)
    initialize

   wrkProc (Record nr l flds) = do
    let thisFldMap = IM.fromList (zip [1,2..] (map valstr flds)) 
    modify $ \s -> s { hcThisLine = l, hcFields = thisFldMap}
    assignToBVar ModSet NF  (VDouble $ fromIntegral $ (length flds))
    assignToBVar ModAdd NR  (VDouble 1)
    assignToBVar ModAdd FNR (VDouble 1)
    -- find matching actions for this line and execute them
    actions <- (gets hcCode >>= filterM matches)
    forM_ actions $ \(Section _ ms) -> exec (emptyKBlock) $
       case ms of
         Nothing  -> (PRINT [])
         (Just s) -> s


run :: AwkSource -> Handle -> String -> IO ()
run src h file = inThread $ do
    q <- newEmptyMVar
    j <- newEmptyMVar
    forkIO $ runReaderThread reader h "\n" " " q >> return ()
    forkFinally (worker src q) $ \_ -> putMVar j ()
    -- runInterpreter (intMain h file) (emptyContext src)
    takeMVar j
    return ()
