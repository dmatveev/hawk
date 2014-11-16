module Lang.Hawk.Utils where

import qualified Data.ByteString.Char8 as B

-- Helper function - take a range of matches and invert it (to extract the unmatched data)
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
