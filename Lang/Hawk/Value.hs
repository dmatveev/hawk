{-# LANGUAGE OverloadedStrings #-}

module Lang.Hawk.Value where

import Control.Applicative ((<*))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as B
import Data.Fixed (mod')
import GHC.Float (floatToDigits)

import Lang.Hawk.AST

-- AWK runtime data type representation
data Value = VString !B.ByteString !Double !Bool
           | VDouble !Double
             deriving (Eq, Show)

valstr :: B.ByteString -> Value
valstr s = VString s n b
  where (n,b) = case AP.parse (AP.skipSpace >> AP.double <* AP.skipSpace) s of
                (AP.Partial f) -> case (f "") of
                  (AP.Done _ r) -> (  r,  True) -- The entire string is parsed as number
                  otherwise     -> (0.0, False) -- The impossible case?
                (AP.Done _ r)   -> (  r, False) -- Only a part of string is parsed as number
                (AP.Fail _ _ _) -> (0.0, False) -- Not a number at all

defstr :: B.ByteString -> Value
defstr s = VString s 0.0 False

-- Coercions and conversions
toBool :: Value -> Bool
toBool (VString "" _ _) = False
toBool (VDouble 0)      = False
toBool _                = True

toDouble :: Value -> Double
toDouble (VDouble d)      = d
toDouble (VString "" _ _) = 0.0
toDouble (VString _  d _) = d

toInt :: Value -> Int
toInt (VDouble d)      = truncate d
toInt (VString "" _ _) = 0
toInt (VString _  d _) = truncate d

toString :: Value -> B.ByteString
toString (VString s _ _) = s
toString (VDouble d) =
    let s | rs == [0] && p == 0 = "0"
          | p >= 0 && nrs <= p  = sgn ++ (concat $ map show rs) ++ take (p-nrs) (repeat '0')
          | otherwise           = show d
    in B.pack s
  where (rs,p) = floatToDigits 10 (abs d)
        nrs = length rs
        sgn = if d >= 0 then "" else "-"

calcNewValue oldVal op arg =
     case op of
        ModSet  -> arg
        ModAdd -> VDouble $! toDouble oldVal + toDouble arg
        ModSub -> VDouble $! toDouble oldVal - toDouble arg
        ModMul -> VDouble $! toDouble oldVal * toDouble arg
        ModDiv -> VDouble $! toDouble oldVal / toDouble arg
        ModMod -> VDouble $! toDouble oldVal `mod'` toDouble arg
        otherwise -> undefined