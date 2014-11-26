import qualified Data.Sequence as D
import Data.Foldable (toList)
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Trans
import System.IO
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Printf

import Lang.Hawk.Grammar (awk)
import Lang.Hawk.Analyzer
import Lang.Hawk.Scheduler

import Lang.Hawk.Bytecode.Compiler

runHawk :: String -> String -> IO ()
runHawk progFile inputFile = do
  source <- readFile progFile
  let ast = parse awk progFile source
  case ast of
    (Left e)  -> putStrLn $ show e
    (Right a) -> bracket (openFile inputFile ReadMode) hClose $ \h -> run a h inputFile

runTrace :: String -> IO ()
runTrace progFile = do
  source <- readFile progFile
  case parse awk progFile source of
     (Left e)  -> putStrLn $ show e
     (Right a) -> do src <- awkPrepare a
                     let bc = runCompiler (mapM_ compileTL src) csInitial
                     forM_ (zip [0::Int,1..] (toList bc)) $ \(i, c) -> do
                        printf "%4d  %s\n" i (show c)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progFile, inputFile] -> runHawk progFile inputFile
    [progFile]            -> runTrace progFile
    otherwise -> putStrLn $ "Usage: Hawk PROGFILE INPUTFILE\n" ++
                            "       Hawk PROGFILE"
