import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Trans
import System.IO
import System.Environment (getArgs)
import Text.Parsec (parse)

import Lang.Hawk.Grammar (awk)
import Lang.Hawk.Interpreter
import Lang.Hawk.Analyzer

runHawk :: String -> String -> IO ()
runHawk progFile inputFile = do
  source <- readFile progFile
  let ast = parse awk progFile source
  case ast of
    (Left e)  -> putStrLn $ show e
    (Right a) -> bracket (openFile inputFile ReadMode) hClose $ \h -> do
         runInterpreter (intMain h inputFile) (emptyContext a)
         return ()

runTrace :: String -> IO ()
runTrace progFile = do
  source <- readFile progFile
  case parse awk progFile source of
     (Left e)  -> putStrLn $ show e
     (Right a) -> putStrLn $ show (analyze a)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progFile, inputFile] -> runHawk progFile inputFile
    [progFile]            -> runTrace progFile
    otherwise -> putStrLn $ "Usage: Hawk PROGFILE INPUTFILE\n" ++
                            "       Hawk PROGFILE"
