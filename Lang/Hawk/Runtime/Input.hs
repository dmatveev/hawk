module Lang.Hawk.Runtime.Input where

import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM
import Control.Concurrent
import Control.Monad.IO.Class

import System.IO

import Lang.Hawk.Value

data Record   = Record !Integer !B.ByteString !Int !(IM.IntMap Value)
                deriving Show

data Workload = Workload { wID :: !Integer, wRS :: ![Record] }
                deriving Show

data InputSource = External (MVar (Maybe Workload))
                 | FromHandle Handle (IORef B.ByteString) (IORef Bool)

fromHandle :: MonadIO m => Handle -> m InputSource
fromHandle h = do
   b <- liftIO $ newIORef B.empty
   e <- liftIO $ newIORef False
   return $ FromHandle h b e


fetch :: MonadIO m => InputSource -> m (Maybe Workload)
{-# INLINE fetch #-}
fetch (External m) = liftIO $ takeMVar m


openInputFile :: MonadIO m => B.ByteString -> m InputSource
openInputFile f = do
   h <- liftIO $ openFile (B.unpack f) ReadMode
   fromHandle h

nextLine :: MonadIO m => InputSource -> B.ByteString -> m (Maybe B.ByteString)
nextLine (FromHandle h rb re) rs = readIter h
  where
   readIter h = do
    thisBuf <- liftIO $ readIORef rb
    eof     <- liftIO $ readIORef re
    let nrs = B.length rs
    case B.breakSubstring rs thisBuf of
      (l, rest) | B.null l && B.null rest && eof -> return Nothing
                |             B.null rest && eof -> return (Just l)
                | B.null rest && not eof -> do
                     nextChunk <- liftIO $ B.hGet h 8192
                     liftIO $ do modifyIORef' rb (\tb -> B.append tb nextChunk)
                                 writeIORef   re (B.null nextChunk) 
                     readIter h
                | otherwise -> do liftIO $ writeIORef rb (B.drop nrs rest)
                                  return (Just l)


closeStream :: MonadIO m => InputSource -> m ()
closeStream (External _)       = return ()
closeStream (FromHandle h _ _) = liftIO $ hClose h
