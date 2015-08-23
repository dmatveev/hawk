module Lang.Hawk.Runtime.Queue
       (
         Queue
       , create
       , enqueue
       , dequeue
       ) where

import Control.Concurrent.STM
import Control.Monad (replicateM)
import Data.Array

-- data Queue a = Queue
--                { qhead  :: TVar Int
--                , qtail  :: TVar Int
--                , qempty :: TVar Bool
--                , qdata  :: Array Int (TVar a)
--                , qsize  :: Int
--                }

data Queue a = Queue
               { qstate :: TVar (Int,Int,Bool)
               , qdata  :: Array Int (TVar a)
               , qsize  :: Int
               }

create :: Int -> IO (Queue a)
create s = do
  ts <- newTVarIO (0,0,True)
  d  <- replicateM s (newTVarIO undefined)
  let ar = listArray (0, s) d
  return $ Queue ts ar s

enqueue :: Queue a -> a -> IO ()
enqueue (Queue ts d s) a = atomically $ do
  (h,t,e) <- readTVar ts
  if t == h && not e
  then retry
  else do let newT = (t + 1) `mod` s
          writeTVar (d ! newT) a
          writeTVar ts (h,newT,False)
  return ()


dequeue :: Queue a -> IO a
dequeue (Queue ts d s) = atomically $ do
  (h,t,e) <- readTVar ts
  if t == h && e
  then retry
  else do let newH = (h + 1) `mod` s
          v <- readTVar (d ! newH)
          writeTVar ts (newH, t, newH == t)
          return v

mk :: Int -> IO (Queue Int)
mk n = create n
