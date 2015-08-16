{-# LANGUAGE CPP #-}

module Lang.Hawk.Runtime.Reader
       (
         runReaderThread
       , reader
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Concurrent
import Control.Monad (replicateM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import System.IO (Handle)

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import Lang.Hawk.Runtime
import Lang.Hawk.Runtime.Input
import Lang.Hawk.Value

data ReaderState = ReaderState
                 { rNR  :: !Integer
                 , rWID :: !Integer
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
#ifdef TRACE
            liftIO $ putStrLn $ "Sending workload " ++ show (wID w)
#endif
            gets rQ >>= (liftIO . flip putMVar (Just w))
            modify $ \s -> s { rTmp = [] }
    else do qq <- gets rQ
            liftIO $ replicateM_ 4 $ putMVar qq Nothing
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
