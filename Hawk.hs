import Control.Monad (forM_)
import Control.Monad.Trans
import Data.IORef
import System.Environment (getArgs)
import Text.Parsec (parse)

import Lang.Hawk.Grammar (awk)
import Lang.Hawk.Interpreter

runHawk :: String -> String -> IO ()
runHawk progFile inputFile = do
  source <- readFile progFile
  let ast = parse awk progFile source
  case ast of
    (Left e)  -> putStrLn $ show e
    (Right a) -> do
        contextRef <- newIORef (emptyContext a)
        execInterpreter intMain (emptyContext a)
        return ()
  where
    intMain :: Interpreter ()
    intMain = do
      input <- liftIO $ readFile inputFile
      assignToBVar "=" "FILENAME" (VString inputFile)
      assignToBVar "=" "FNR"      (VDouble 0)
      initialize
      forM_ (lines input) processLine
      finalize

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progFile, inputFile] -> runHawk progFile inputFile
    otherwise -> putStrLn "Usage: Hawk PROGFILE INPUTFILE"
