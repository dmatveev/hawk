module Lang.Hawk.Runtime.Output where

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B 

import Lang.Hawk.Value
import Lang.Hawk.Runtime.Printf

data WriteRequest = WRqPrint  Integer [Value] Value Value
                  | WRqPrintf Integer [Value]
                  | WRqRaw    Integer B.ByteString

type WriterThread a = ReaderT (Chan WriteRequest) IO a

writer :: WriterThread ()
writer = do
  chan <- ask
  liftIO $ do rq <- readChan chan
              case rq of
               (WRqPrint _ vs ofs ors) -> do
                 let str = B.intercalate (toString $ ofs) $ map toString vs
                 B.putStr $ B.append str (toString $ ors) 
               (WRqPrintf _ (fmt:vs)) -> do
                 B.putStr $ sprintf (toString fmt) vs
               (WRqRaw    _ s) -> do
                 B.putStrLn s
  writer

runWriterThread (Output c) = runReaderT writer c


newtype Output = Output (Chan WriteRequest)

mkOutput :: IO Output
mkOutput = Output <$> newChan

write :: Output -> WriteRequest -> IO ()
write (Output c) w = writeChan c w
