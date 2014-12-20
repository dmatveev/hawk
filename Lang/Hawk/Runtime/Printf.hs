module Lang.Hawk.Runtime.Printf where

import Control.Applicative ((<$>))
import Data.Maybe (maybe)
import qualified Data.ByteString.Char8 as B
import Text.Parsec

data FormatChar = C | D | E | F | G | O | S | X | Percent
                  deriving (Show)

data Alignment = L | R
                 deriving (Show)

data FormatSpec = FormatSpec FormatChar Alignment Int Bool Int
                | Plain String
                  deriving (Show)

type FormatString = [FormatSpec]

parsePrintf :: B.ByteString -> FormatString
parsePrintf s = case parse printfParser "" s of
   Left e -> [Plain (B.unpack s)]
   Right s -> s

printfParser = many1 formatParser

formatParser = try parseSpec <|> parsePlain

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
         <|> (char '%' >> return Percent)
   let lPadLen = maybe 0 read lpad
       rPadLen = maybe (defllen fchr) read rpad
   return $ FormatSpec fchr align lPadLen zeroes rPadLen
 where defllen E = 6
       defllen F = 6
       defllen G = 6
       defllen _ = 0

parsePlain = Plain <$> many1 (noneOf "%")
