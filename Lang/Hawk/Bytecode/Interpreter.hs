module Lang.Hawk.Bytecode.Interpreter where

import Data.IORef

import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import qualified Data.ByteString.Char8 as B

import Lang.Hawk.Basic
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Interpreter

import Data.Fixed (mod')

fref :: Value -> Interpreter Value
fref v = do
   let i = toInt v 
   if i == 0
   then gets hcThisLine >>= (return . valstr)
   else do fs <- gets hcFields
           return $! fs IM.! i

bc :: OpCode -> Interpreter ()
bc (ARITH o) = do (rhs:lhs:st) <- gets hcSTACK
                  let l = toDouble lhs
                      r = toDouble rhs
                      v = case o of
                            Mul -> VDouble (l * r)
                            Div -> VDouble (l / r)
                            Add -> VDouble (l + r)
                            Sub -> VDouble (l - r)
                            Mod -> VDouble (mod' l r)
                            Pow -> VDouble (l ** r)
                  modify $ \s -> s {hcSTACK = seq v (v:st)}
bc (MVAR m r) = do (top:st) <- gets hcSTACK
                   liftIO $ modifyIORef' r $ \v -> calcNewValue v m top
                   modify $ \s -> s {hcSTACK = st}
bc (PUSH v) = modify $ \s -> s {hcSTACK = v:(hcSTACK s)   }
bc POP      = modify $ \s -> s {hcSTACK = tail(hcSTACK s) }
bc FIELD    = do (top:st) <- gets hcSTACK
                 f   <- fref top
                 modify $ \s -> s {hcSTACK = f:st}
bc (VAR r)  = do v <- liftIO $! readIORef r
                 modify $ \s -> s {hcSTACK = v:(hcSTACK s) }
bc (VSET r) = do (top:st) <- gets hcSTACK
                 liftIO $ writeIORef r top
                 modify $ \s -> s {hcSTACK = st}
bc (BVAR b) = do v <- evalBVariableRef b
                 modify $ \s -> s {hcSTACK = v:(hcSTACK s) }
bc (CMP o)  = do (rv:lv:st) <- gets hcSTACK
                 let v = VDouble $! test $ case (lv, rv) of
                           (VString lStr lNum sParsed, VString rStr rNum rParsed) ->
                              -- If the both strings represent numbers completely
                             if sParsed && rParsed then cmp lNum rNum else cmp lStr rStr
                           (VString _ lNum _, VDouble   rNum  ) -> cmp lNum rNum
                           (VDouble   lNum  , VString _ rNum _) -> cmp lNum rNum
                           (VDouble   lNum  , VDouble   rNum  ) -> cmp lNum rNum
  
                     cmp l r = case o of
                                CmpEQ -> l == r
                                CmpNE -> l /= r
                                CmpGT -> l >  r
                                CmpGE -> l >= r
                                CmpLT -> l <  r
                                CmpLE -> l <= r
                     test b = if b then 1 else 0
                 modify $ \s -> s {hcSTACK = v:(hcSTACK s) }
bc (CALL "length") = do (top:st) <- gets hcSTACK
                        let v = VDouble $! fromIntegral $ B.length $ toString top
                        modify $ \s -> s {hcSTACK = v:st}
bc DUP             = do st@(top:_) <- gets hcSTACK
                        modify $ \s -> s {hcSTACK = top:st}
bc (PRN n)         = do if n == 0
                        then gets hcThisLine >>= (liftIO . B.putStrLn)
                        else do st <- gets hcSTACK
                                let (vs,rs) = splitAt n st
                                ofs <- liftM toString $ gets hcOFS
                                ors <- liftM toString $ gets hcORS
                                let str = B.intercalate ofs $ map toString $ reverse vs
                                liftIO $ B.putStr $ B.append str ors
                                modify $ \s -> s {hcSTACK = rs}
bc DRP             = modify $ \s -> s { hcSTACK = [] }

execBC :: [OpCode] -> Interpreter () 
execBC []         = return ()
execBC ((JF n):r) = do (top:st) <- gets hcSTACK
                       modify $ \s -> s {hcSTACK = st}
                       if (toBool top)
                       then execBC r
                       else gets hcOPCODES >>= \src -> execBC (drop n src)
execBC (op:ops)   = do -- liftIO $ putStrLn (show op)
                       bc op >> execBC ops 
