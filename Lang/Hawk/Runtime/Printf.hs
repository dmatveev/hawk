module Lang.Hawk.Runtime.Printf (sprintf) where

import Control.Applicative ((<$>))
import Control.Monad.Writer
import Data.Maybe (maybe)
import Data.Char (chr, intToDigit)
import GHC.Float (floatToDigits)
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
   rpad   <- optionMaybe $ char '.' >> (many1 digit <|> return "0")
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

formatChar v = case v of
   (VDouble d)     -> fmt d
   (VString s d b) -> if b then fmt d else B.take 1 s  
  where fmt d = let i = truncate d
                    r | (i <= 0 || i >= 256) = B.empty
                      | otherwise            = B.singleton (chr i)
                in r

formatDec v r = B.pack $ lzrs r $ show $ toInt v 

formatSci v r =
   let (b,m,e) | p >=  0 = ( intToDigit (head rs)
                           , map intToDigit (tail rs)
                           , if p == 0 then "0" else show (p-1)
                           )
               | p <   0 = ( intToDigit (head rs)
                           , map intToDigit (tail rs)
                           , show (abs p + 1)
                           )
   in B.pack $ sgn ++ b:(tzrs r ('.':m)) ++ esgn ++ lzrs 2 e
  where d      = toDouble v
        (rs,p) = floatToDigits 10 (abs d)
        sgn    = if d >= 0 then ""  else "-"
        esgn   = if p >  0 then "e+" else "e-"

formatFlt v r =
   let (n,f) | rs == [0] && p == 0 = ("0","")
             | p >= 0 && nrs <= p  = (map intToDigit rs ++ take (p-nrs) (repeat '0'), "")
             | p < 0               = ("0", take (abs p) (repeat '0') ++ map intToDigit rs)
             | otherwise           = (map intToDigit (take p rs), map intToDigit (drop p rs))
   in B.pack $ sgn ++ n ++ tzrs r ('.':f)
  where (rs,p) = floatToDigits 10 (abs d)
        nrs    = length rs
        sgn    = if d >= 0 then "" else "-"
        d      = toDouble v

tzrs r s = let n = r - length s
               t | r <  0 = s
                 | r == 0 = ""
                 | n >= 0 = s ++ take (n + 1) (repeat '0')
                 | n <  0 = take (abs n) s 
           in t

lzrs l s = let n = l - length s
               t | l == 0 = s
                 | n <  0 = s
                 | n >= 0 = take n (repeat '0') ++ s
           in t

formatG v r = let f = formatFlt v (-1)
                  e = formatSci v (-1)
              in if B.length f < B.length e then f else e

formatOct v r = B.pack $ lzrs r $ formatBase (toInt v) 8 hexchr

formatStr v 0 = toString v
formatStr v i = B.take i $ toString v 

formatHex v r = B.pack $ lzrs r $ formatBase (toInt v) 16 hexchr

-- Yes, it is not very efficient, I know
formatBase i base fchr
   | i < base  = [fchr i]
   | otherwise = (formatBase (i `div` base) base fchr) ++ [fchr (i `mod` base)]

hexchr  0 = '0'
hexchr  1 = '1'
hexchr  2 = '2'
hexchr  3 = '3'
hexchr  4 = '4'
hexchr  5 = '5'
hexchr  6 = '6'
hexchr  7 = '7'
hexchr  8 = '8'
hexchr  9 = '9'
hexchr 10 = 'a'
hexchr 11 = 'b'
hexchr 12 = 'c'
hexchr 13 = 'd'
hexchr 14 = 'e'
hexchr 15 = 'f'
