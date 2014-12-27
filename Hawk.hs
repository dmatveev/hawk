import Data.Foldable (toList)
import Control.Exception (bracket)
import Control.Monad (forM_)
import System.IO
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Printf (printf)

import Lang.Hawk.Options
import Lang.Hawk.Grammar (awk)
import Lang.Hawk.Analyzer
import Lang.Hawk.Scheduler
import Lang.Hawk.Bytecode.Compiler

runHawk :: HawkConfig -> IO ()
runHawk cfg = do
  (progFile, source) <- case (awkProgram cfg) of
     (HawkString s) -> return ("", s)
     (HawkFile f)   -> readFile f >>= \s -> return (f,s)
  let ast = parse awk progFile source
  case ast of
    (Left e)  -> putStrLn $ show e
    (Right a) -> do
      if awkDebug cfg
      then do let bc = runCompiler (mapM_ compileTL a) csInitial
              forM_ (zip [0::Int,1..] (toList bc)) $ \(i, c) -> printf "%4d  %s\n" i (show c)
      else case awkFiles cfg of
        []        -> run a stdin ""
        (file:[]) -> bracket (openFile file ReadMode) hClose $ \h -> run a h file
        _         -> putStrLn "Multiple inpit files are not supported yet"


main :: IO ()
main = parseHawkArgs >>= runHawk

