{-# LANGUAGE OverloadedStrings, FlexibleInstances, CPP #-}

module Lang.Hawk.Runtime.Output where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (insert)
import qualified Data.ByteString.Char8 as B

import Lang.Hawk.Value
import Lang.Hawk.Runtime.Printf

data WriteRequest = WRqPrint  [Value] Value Value
                  | WRqPrintf [Value]
                  | WRqRaw    B.ByteString
                    deriving (Eq, Show)

data Bulk = Bulk
            { blkID  :: Integer
            , blkRQs :: [WriteRequest]
            }
            deriving (Eq, Show)

instance Ord Bulk where
  compare (Bulk a _) (Bulk b _) = compare a b

type WriterChan = Chan Bulk


formatBuffer :: [WriteRequest] -> B.ByteString
formatBuffer rqs = (B.concat $ map buildStr rqs)
 where
   buildStr rq = case rq of
     (WRqPrint vs ofs ors) ->
       B.append (B.intercalate (toString ofs) $ map toString vs) (toString ors)
     (WRqPrintf (fmt:vs)) ->
       sprintf (toString fmt) vs
     (WRqRaw    s) -> do
       B.append s "\n"


takeSerial :: Integer -> [Bulk] -> ([Bulk], [Bulk])
takeSerial expected blks =
  let (s, q) = break (\(i, (Bulk bi _)) -> i /= bi) $ zip [expected..] blks
  in  (map snd s, map snd q)


fastWriter :: WriterChan -> IO ()
fastWriter chan = do
  (Bulk _ rqs)  <- readChan chan
  case rqs of
   []        -> return ()
   otherwise -> B.putStr (formatBuffer rqs) >> fastWriter chan


serialWriter :: Integer -> [Bulk] -> WriterChan -> IO ()
serialWriter expected queue chan = do
  p@(Bulk i rqs) <- readChan chan
  case rqs of
    []        -> return ()
    otherwise -> do
#ifdef TRACE
      putStrLn $ "OUTPT: Got something (" ++ show i ++ "), expected " ++ show expected
#endif
     if i /= expected
     then serialWriter expected (insert p queue) chan
     else do let (ready, pending) = takeSerial (succ i) queue
             B.putStr $ formatBuffer $ rqs ++ concat (map blkRQs ready)
             let newExpected = succ $ if null ready then i else (blkID $ last ready)
             serialWriter newExpected pending chan

-- | Server part of the output system. Processes write requests
-- from clients, formats output and actually prints it.
--
-- Output object is usually single for the entire application.
-- There are two types of Output implementations: fast and serial.
--
-- The Fast one simply writes everything it receives without any
-- buffering/reordering. It is suitable for single-thread (single
-- interpreter) execution.
--
-- The Serial one reorders all write requests coming from clients
-- according to the workload ID. It is designed especially for
-- the parallel case (when there are multiple interpreter threads
-- working on different portions of data).
data Output = FastOutput WriterChan
            | SerialOutput Integer [Bulk] WriterChan

runWriterThread :: Output -> IO ()
runWriterThread (FastOutput c)         = fastWriter c
runWriterThread s@(SerialOutput _ _ c) = serialWriter 1 [] c

mkOutput :: IO Output
mkOutput = FastOutput <$> newChan

mkSerialOutput :: IO Output
mkSerialOutput = SerialOutput <$> pure 0 <*> pure [] <*> newChan

closeOutput :: Output -> IO ()
closeOutput (FastOutput c)       = writeChan c (Bulk (-1) [])
closeOutput (SerialOutput _ _ c) = writeChan c (Bulk (-1) [])

-- | Client part of the system - actually used by interpreter(s)
-- during AWK program execution. Clients use this API to enqueue
-- write requests for Output.
--
-- There are two variations: Buffered and NonBuffered. The Buffered
-- one maintains internal queue (Bulk) of write requests - it
-- aggregates all requests coming from the same workload (portion
-- of data processed by an interpreter) and sends it to Output
-- only when the whole portion of input data is processed.
data OutputSink = BufferedSink !(IORef Bulk) !WriterChan
                | NonBufferedSink !WriterChan

mkBufferedSink :: Output -> IO OutputSink
mkBufferedSink (FastOutput c)       = BufferedSink <$> newIORef (Bulk (-1) []) <*> pure c
mkBufferedSink (SerialOutput _ _ c) = BufferedSink <$> newIORef (Bulk (-1) []) <*> pure c

mkNonBufferedSink :: Output -> IO OutputSink
mkNonBufferedSink (FastOutput c)       = NonBufferedSink <$> pure c
mkNonBufferedSink (SerialOutput _ _ c) = NonBufferedSink <$> pure c

write :: OutputSink -> Integer -> WriteRequest -> IO ()
write (BufferedSink s c) i w = do
  (Bulk wid queue) <- readIORef s
  if wid /= i
  then do writeIORef s (Bulk i [w])
          when (wid /= -1) $ writeChan c $ Bulk wid (reverse queue)
  else writeIORef s (Bulk i $ w:queue)
write (NonBufferedSink c) i w = writeChan c $ Bulk i [w]


flush :: OutputSink -> IO ()
flush (BufferedSink s c) = do
  (Bulk wid queue) <- readIORef s
  writeIORef s (Bulk wid []) >> writeChan c (Bulk wid $ reverse queue)
flush (NonBufferedSink _) = return ()


-- test :: IO ()
-- test = do
--   o <- mkSerialOutput
--   s <- mkBufferedSink o
--   forkIO $ runWriterThread o
--   write s 0 $ WRqRaw $ "Lol"
--   write s 1 $ WRqRaw $ "OLOL"
--   write s 3 $ WRqRaw $ "I AM THIRD"
--   write s 4 $ WRqRaw $ "I AM FORTH"
--   write s 2 $ WRqRaw $ "UNBLOCKER"
--   write s 5 $ WRqRaw $ "(next to be put into queue)"
--   write s 7 $ WRqRaw $ "7th"
--   write s 6 $ WRqRaw $ "6th"
--   flush s
