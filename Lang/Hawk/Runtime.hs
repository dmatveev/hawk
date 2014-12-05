{-# LANGUAGE OverloadedStrings #-}

module Lang.Hawk.Runtime where

import Control.Monad.Trans
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Fixed (mod')
import Text.Regex.PCRE

import Lang.Hawk.Basic
import Lang.Hawk.Value

calcArith _    rval Set = rval
calcArith lval rval op = 
   let l = toDouble lval
       r = toDouble rval
   in VDouble $! case op of
       Mul -> l * r
       Div -> l / r
       Add -> l + r
       Sub -> l - r
       Mod -> mod' l r
       Pow -> l ** r
 
cmpValues lval rval op = VDouble $! test $ case (lval, rval) of
    (VString lStr lNum sParsed, VString rStr rNum rParsed) ->
       -- If the both strings represent numbers completely
       if sParsed && rParsed then cmp lNum rNum else cmp lStr rStr
    (VString _ lNum _, VDouble   rNum  ) -> cmp lNum rNum
    (VDouble   lNum  , VString _ rNum _) -> cmp lNum rNum
    (VDouble   lNum  , VDouble   rNum  ) -> cmp lNum rNum
 where 
   cmp l r = case op of
              CmpEQ -> l == r
              CmpNE -> l /= r
              CmpGT -> l >  r
              CmpGE -> l >= r
              CmpLT -> l <  r
              CmpLE -> l <= r
   test b = if b then 1 else 0 


splitIntoFields' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitIntoFields' fs str
    | fs == " " = B.words str                        -- Handles ' ' and '\t'
    | B.null fs = map B.singleton (B.unpack str)     -- Every character is a field
    | otherwise = let ms = getAllMatches (str =~ fs) -- Regular expression
                      rs = invRegions ms (B.length str)
                  in map (\(s,l) -> B.take l (B.drop s str)) rs


invRegions :: [(Int,Int)] -> Int -> [(Int,Int)]
invRegions matches len =  invRegions' matches 0 len []
  where invRegions' [] startVal endVal res = res ++ [(startVal, endVal-startVal)]
        invRegions' ((pStart,pLen):ps) startVal endVal res =
          let thisRegn  = (startVal, pStart-startVal) -- point where valuable data ends
              nextStart = pStart + pLen               -- point where next valuable data starts
          in invRegions' ps nextStart endVal (res ++ [thisRegn])


splitWithSep :: B.ByteString -> B.ByteString -> [B.ByteString]
splitWithSep s fs = reverse $! splitWith' s []
  where
    splitWith' str res = case B.breakSubstring fs str of
       (x, y) | B.null y  -> x:res
              | otherwise -> splitWith' (B.drop nfs y) (x:res)
    nfs = B.length fs

calcLogic :: LogOp -> Value -> Value -> Value
calcLogic op lv rv = 
   let l = toBool lv
       r = toBool rv
   in case op of
      AND -> VDouble $! test (l && r)
      OR  -> VDouble $! test (l || r)
   where test b = if b then 1 else 0

match :: Value -> Value -> Value
match vs vr = let (s, r) = (toString vs, toString vr)
              in VDouble $! if r /= "" && s =~ r then 1 else 0

-- Standard functions
calcAtan2   y x   = VDouble $! atan2 (toDouble y) (toDouble x)
calcIndex   s t   = VDouble $! if B.null y then 0 else fromIntegral (1 + B.length x)
                    where (x, y) = B.breakSubstring (toString t) (toString s)
calcLength  s     = VDouble $! fromIntegral $ B.length (toString s)
calcSubstr  s p   = valstr $ B.drop (toInt p - 1) (toString s)
calcSubstr2 s p n = valstr $ B.take (toInt n) $ B.drop (toInt p - 1) (toString s)

calcGSub    r s l = let (r',s') = (toString r, toString s)
                        matches = getAllMatches (l =~ r') :: [(MatchOffset, MatchLength)]
                        regions = invRegions matches (B.length l)
                        strings = map (\(st,len) -> B.take len (B.drop st l)) regions
                        result  = B.intercalate s' strings
                    in (length matches, result)

calcSub     r s l = let (r', s') = (toString r, toString s)
                    in case (l =~ r') of
                         (-1, _)    -> (0, l) 
                         (off, len) -> (1, B.concat [B.take off l, s', B.drop (off+len) l])

calcMatch   s r   = let (s', r') = (toString s, toString r)
                        (rS, rL) = (s' =~ r') :: (MatchOffset, MatchLength)
                    in (rS + 1, rL)

calcSplit :: MonadIO m => Value -> Value -> IORef Array -> m Value
calcSplit str fs arr = do
   let ss    = (toString str) `splitWithSep` (toString fs)
       is    = [1, 2..]
       keys  = map show [1, 2..]
       strs  = map valstr $ ss
   liftIO $ writeIORef arr (M.fromList $ zip keys strs)
   return $! VDouble $ fromIntegral $ length ss

proxyFcn :: (Double -> Double) -> Value -> Value
proxyFcn f e = VDouble $ f (toDouble e)
