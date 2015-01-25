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
import Lang.Hawk.Bytecode
import Lang.Hawk.Bytecode.Compiler

pcode :: [OpCode] -> IO ()
pcode cc = forM_ (zip [0::Int,1..] cc) $ \(i,c) -> printf "%4d  %s\n" i (show c)
       

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
      then do let (st,bc,fi) = compileNoRefs a
              putStrLn ".BEGIN"   >> pcode st
              putStrLn ".ACTIONS" >> pcode bc
              putStrLn ".END"     >> pcode fi
      else do
           code <- compile a 4 -- TODO: Magic number - the number of cores
           case awkFiles cfg of
             []        -> run code stdin ""
             (file:[]) -> bracket (openFile file ReadMode) hClose $ \h -> run code h file
             _         -> putStrLn "Multiple inpit files are not supported yet"


main :: IO ()
main = parseHawkArgs >>= runHawk

