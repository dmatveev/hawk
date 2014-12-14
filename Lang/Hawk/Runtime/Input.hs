module Lang.Hawk.Runtime.Input where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM
import Control.Concurrent
import Control.Monad.IO.Class

import Lang.Hawk.Value

data Record   = Record !Integer !B.ByteString !Int !(IM.IntMap Value)
                deriving Show

data Workload = Workload { wID :: !Integer, wRS :: ![Record] }
                deriving Show

data InputSource = External (MVar (Maybe Workload))

fetch :: MonadIO m => InputSource -> m (Maybe Workload)
{-# INLINE fetch #-}
fetch (External m) = liftIO $ takeMVar m

