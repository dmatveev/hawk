{-# LANGUAGE CPP #-}

module Lang.Hawk.Runtime.Input where

import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM
import Control.Concurrent

import System.IO

import Lang.Hawk.Value

data Record   = Record !Integer !B.ByteString !Int !(IM.IntMap Value)
                deriving Show

data Workload = Workload { wID :: !Integer, wRS :: ![Record] }
                deriving Show

data InputSink = MVarSink !(MVar (Maybe Workload))
               | ChanSink !(Chan (Maybe Workload))

supply :: InputSink -> (Maybe Workload) -> IO ()
supply (MVarSink m) mw = putMVar   m mw
supply (ChanSink c) mw = writeChan c mw


data InputSource = External     !(MVar (Maybe Workload))
                 | ExternalChan !(Chan (Maybe Workload))
                 | FromHandle !Handle !(IORef B.ByteString) !(IORef Bool)

fromHandle :: Handle -> IO InputSource
fromHandle h = do
   b <- newIORef B.empty
   e <- newIORef False
   return $ FromHandle h b e

fetch :: InputSource -> IO (Maybe Workload)
fetch (External m)     = takeMVar m
fetch (ExternalChan c) = readChan c

openInputFile :: B.ByteString -> IO InputSource
openInputFile f = fromHandle =<< openFile (B.unpack f) ReadMode

nextLine :: InputSource -> B.ByteString -> IO (Maybe B.ByteString)
{-# INLINE nextLine #-}
nextLine (FromHandle h rb re) rs = do
   -- putStrLn "READR: Reading next line"
   readIter h
  where
   readIter h = do
    thisBuf <- readIORef rb
    eof     <- readIORef re
    let nrs = B.length rs
    case B.breakSubstring rs thisBuf of
      (l, rest) | B.null l && B.null rest && eof -> return $! Nothing
                |             B.null rest && eof -> do writeIORef rb B.empty
                                                       return $! (Just l)
                | B.null rest && not eof -> do
#ifdef TRACE
                     putStrLn "READR: OK, Getting the next 8K..."
#endif
                     nextChunk <- B.hGet h 8192
#ifdef TRACE
                     putStrLn "READR: Done"
#endif
                     modifyIORef' rb (\tb -> B.append tb nextChunk)
                     writeIORef   re (B.null nextChunk)
                     readIter h
                | otherwise -> do writeIORef rb (B.drop nrs rest)
                                  return $! (Just l)


closeStream :: InputSource -> IO ()
closeStream (External _)       = return ()
closeStream (FromHandle h _ _) = hClose h
