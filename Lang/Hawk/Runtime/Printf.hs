module Lang.Hawk.Runtime.Printf where

import Control.Applicative ((<$>))
import Control.Monad.Writer
import Data.Maybe (maybe)
import Data.Char (chr)
import qualified Data.ByteString.Char8 as B
import Text.Parsec

import Lang.Hawk.Value

data FormatChar = C | D | E | F | G | O | S | X
                  deriving (Show)

data Alignment = L | R
                 deriving (Show)

data FormatSpec = Format FormatChar Alignment Int Bool Int
                | Percent
                | Plain String
                  deriving (Show)

type FormatString = [FormatSpec]

parsePrintf :: B.ByteString -> FormatString
parsePrintf s = case parse printfParser "" s of
   Left e -> [Plain (B.unpack s)]
   Right s -> s

printfParser = many1 formatParser

formatParser = try parseSpec <|> try parsePercent <|> parsePlain

parsePercent = string "%%" >> return Percent

parseSpec = do
   char '%'
   align  <- (char '-' >> return L   ) <|> return R
   zeroes <- (char '0' >> return True) <|> return False
   lpad   <- optionMaybe $ many1 digit
   rpad   <- optionMaybe $ char '.' >> many1 digit
   fchr   <- (char 'c' >> return C)
         <|> (char 'd' >> return D)
         <|> (char 'e' >> return E)
         <|> (char 'f' >> return F)
         <|> (char 'g' >> return G)
         <|> (char 'o' >> return O)
         <|> (char 's' >> return S)
         <|> (char 'x' >> return X)
   let lPadLen = maybe 0 read lpad
       rPadLen = maybe (defllen fchr) read rpad
   return $ Format fchr align lPadLen zeroes rPadLen
 where defllen E = 6
       defllen F = 6
       defllen G = 6
       defllen _ = 0

parsePlain = Plain <$> many1 (noneOf "%")

sprintf :: B.ByteString -> [Value] -> B.ByteString
sprintf fmt vs  = execSPrintf (parsePrintf fmt) vs

execSPrintf :: FormatString -> [Value] -> B.ByteString
execSPrintf fs vs = execWriter (printfWriter fs vs)

printfWriter :: FormatString -> [Value] -> Writer B.ByteString ()
printfWriter []       _           = return ()
printfWriter (f:fmts) vals = case f of
     (Plain s) -> tell (B.pack s)      >> printfWriter fmts vals
     (Percent) -> tell (B.pack "%")    >> printfWriter fmts vals
     otherwise -> if null vals
                  then error "Not enough arguments"
                  else tell (formatVal f $ head vals) >> printfWriter fmts (tail vals)

formatVal :: FormatSpec -> Value -> B.ByteString
formatVal (Format C align l z r) v = pad align l z $ formatChar v
formatVal (Format D align l z r) v = pad align l z $ formatDec  v r
formatVal (Format E align l z r) v = pad align l z $ formatSci  v r 
formatVal (Format F align l z r) v = pad align l z $ formatFlt  v r
formatVal (Format G align l z r) v = pad align l z $ formatG    v r
formatVal (Format O align l z r) v = pad align l z $ formatOct  v r
formatVal (Format S align l z r) v = pad align l z $ formatStr  v r
formatVal (Format X align l z r) v = pad align l z $ formatHex  v r  

pad align l z str = 
   let dl = l - B.length str
   in if dl > 0 then pad' align dl str else str
  where pad' L dl str = B.append str (padstr dl)
        pad' R dl str = B.append (padstr dl) str
        padstr n = B.pack $ take n $ if z then repeat '0' else repeat ' '

prepend r str = if r == 0 then str else p' (r - B.length str)
  where p' dl = B.append (B.pack $ take dl $ repeat '0') str

formatChar v = case v of
   (VDouble d)     -> fmt d
   (VString s d b) -> if b then fmt d else B.take 1 s  
  where fmt d = let i = truncate d
                    r | (i <= 0 || i >= 256) = B.empty
                      | otherwise            = B.singleton (chr i)
                in r

formatDec v r = prepend r $ B.pack $ show $ toInt v 

formatSci = undefined
formatFlt = undefined
formatG   = undefined
formatOct = undefined

formatStr v 0 = toString v
formatStr v i = B.take i $ toString v 

formatHex v r = prepend r $ B.pack $ hexstr $ toInt v
    where hexstr i | i < 16    = [hexchr i]
                   | otherwise = (hexstr $ i `div` 16) ++ [hexchr (i `mod` 16)]
          hexchr i = ['0','1','2','3','4','5','6','7'
                     ,'8','9','a','b','c','d','e','f'
                     ] !! i 
