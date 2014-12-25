import Control.Exception (bracket)
import Control.Monad (forM_)
import System.IO
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Printf (printf)

import Lang.Hawk.Options
import Lang.Hawk.Grammar (awk)
-- import Lang.Hawk.Analyzer
import Lang.Hawk.Scheduler
-- import Lang.Hawk.Bytecode.Compiler

runHawk :: HawkConfig -> IO ()
runHawk cfg = do
  (progFile, source) <- case (awkProgram cfg) of
     (HawkString s) -> return ("", s)
     (HawkFile f)   -> readFile f >>= \s -> return (f,s)
  let ast = parse awk progFile source
  case ast of
    (Left e)  -> putStrLn $ show e
    (Right a) -> do
      case (awkFiles cfg) of
        []        -> run a stdin ""
        (file:[]) -> bracket (openFile file ReadMode) hClose $ \h -> run a h file
        _         -> putStrLn "Multiple inpit files are not supported yet"

-- runTrace :: String -> IO ()
-- runTrace progFile = do
--   source <- readFile progFile
--   case parse awk progFile source of
--      (Left e)  -> putStrLn $ show e
--      (Right a) -> do src <- awkPrepare a
--                      liftIO $ putStrLn $ show src
--                      let bc = runCompiler (mapM_ compileTL src) csInitial
--                      forM_ (zip [0::Int,1..] (toList bc)) $ \(i, c) -> do
--                         printf "%4d  %s\n" i (show c)

main :: IO ()
main = parseHawkArgs >>= runHawk

