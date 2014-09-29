import Control.Monad (forM_)
import Control.Monad.Trans
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
    (Right a) -> do runInterpreter (intMain inputFile) (emptyContext a)
                    return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progFile, inputFile] -> runHawk progFile inputFile
    otherwise -> putStrLn "Usage: Hawk PROGFILE INPUTFILE"
