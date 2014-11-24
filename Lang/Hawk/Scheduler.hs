{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lang.Hawk.Scheduler where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Exception.Base (evaluate)
import Control.Monad.State.Strict

import Lang.Hawk.AST
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
                 , rQ   :: !(MVar (Maybe Workload))
                 , rTmp :: ![Record]
                 }

newtype ReaderThread a = ReaderThread (StateT ReaderState IO a)
                         deriving (Monad, MonadIO, MonadState ReaderState,
                                   Applicative, Functor)

sendWorkload :: ReaderThread ()
sendWorkload = do
    tmp <- gets rTmp
    if (not $ null tmp)
    then do -- wid <- nextWID
            w <- Workload <$> nextWID <*> (pure $ reverse tmp)
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
       newR = Record nid l flds
   modify $ \s -> seq newR $ s { rTmp = newR:(rTmp s) }
   tmpSz <- gets (length . rTmp)
   when (tmpSz >= 10) sendWorkload
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

worker :: AwkSource -> MVar (Maybe Workload) -> IO ()
worker src mq = runInterpreter wrkMain (emptyContext src) >> return ()
 where
   wrkMain = do
      -- assignToBVar ModSet FILENAME (valstr $ B.pack inputFile)
      modify $ \s -> s { hcFNR = VDouble 0 }
      initialize
      workerLoop
      finalize
 
   workerLoop = do
      q <- liftIO $ takeMVar mq
      case q of
        (Just (Workload _ rs)) -> forM rs wrkProc >> workerLoop
        Nothing                -> return ()

   wrkProc (Record nr l flds) = do
      let thisFldMap = IM.fromList (zip [1,2..] (map valstr flds)) 
      modify $ \s -> s { hcThisLine = l
                       , hcFields   = thisFldMap
                       , hcNF       = VDouble (fromIntegral $ length flds)
                       , hcNR       = VDouble (succ $ toDouble $ hcNR s)
                       , hcFNR      = VDouble (succ $ toDouble $ hcNR s)
                       }
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
    takeMVar j
    return ()