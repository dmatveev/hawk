{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.Hawk.Bytecode.Compiler where

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as D
import Data.Sequence ((|>))
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad.State.Strict

import Lang.Hawk.Basic
import Lang.Hawk.AST
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Analyzer

data CompilerState = CompilerState
                     { csBC  :: Bytecode
                     , csCur :: Int
                     , csLS  :: Maybe LoopState
                     }

csInitial :: CompilerState
csInitial = CompilerState D.empty 0 Nothing

data LoopState = LoopState
                 { lsEnter  :: Int
                 , lsBreaks :: [Int]
                 , lsConts  :: [Int]
                 , lsChecks :: [Int]
                 }

initialLoopState :: Int -> LoopState
initialLoopState enter = LoopState enter [] [] [] 

newtype Compiler a = Compiler (State CompilerState a)
                   deriving (Monad, MonadState CompilerState, Applicative, Functor)

runCompiler :: Compiler a -> CompilerState -> Bytecode
runCompiler (Compiler c) b = csBC $ execState c b

loop :: Compiler () -> Compiler ()
loop body = do
   oldState <- gets csLS
   enter <- pos
   modify $ \s -> s { csLS = Just (initialLoopState enter) }
   replicateM_ 2 body >> op (JMP enter)
   leave <- pos
   loopState <- liftM fromJust $ gets csLS
   forM_ (lsBreaks loopState) $ putOP (JMP leave)
   forM_ (lsConts  loopState) $ putOP (JMP enter)
   forM_ (lsChecks loopState) $ putOP (JF  leave)
   modify $ \s -> s { csLS = oldState }

op :: OpCode -> Compiler ()
op c = modify $ \s -> s {csBC = (csBC s) |> c, csCur = succ (csCur s)}

nop :: Compiler Int
nop = do
   i <- gets csCur
   op NOOP
   return i

pos :: Compiler Int
pos = gets csCur

putOP :: OpCode -> Int -> Compiler ()
putOP o i  = modify $ \s -> s{ csBC = D.update i o (csBC s) }

jf :: Compiler () -> Compiler ()
jf c = do
   i <- nop
   c
   pos >>= \p -> putOP (JF p) i

drp :: Compiler () -> Compiler ()
drp c = c >> op DRP

toValue :: Literal -> Value
toValue (LitNumeric i) = VDouble i
toValue (LitStr     s) = valstr $ B.pack s
toValue (LitRE      s) = valstr $ B.pack s


compileE :: Expression -> Compiler ()
compileE (Arith a l r)      = compileE l >> compileE r >> op (ARITH a)
compileE (Const a)          = op (PUSH (toValue a))
compileE (FieldRef e)       = compileE e >> op FIELD
compileE (Variable r)       = op (VAR r)
compileE (ArrayRef s e)     = compileE e >> op (ARR s)
compileE (BuiltInVar b)     = op (BVAR b)
compileE (Assignment m p e) = compileASGN m p e
compileE (Incr n e)         = compileINCR n e
compileE (Decr n e)         = compileDECR n e
compileE (Relation c l r)   = compileE l >> compileE r >> op (CMP c)
compileE (Not e)            = compileE e >> op NOT
compileE (Neg e)            = compileE e >> op NEG
compileE (Id e)             = compileE e 
compileE (Logic o l r)      = compileE l >> compileE r >> op (LOG o)
compileE (Match l r)        = compileE l >> compileE r >> op MATCH
compileE (NoMatch l r)      = compileE l >> compileE r >> op NOMATCH
compileE (FunCall s es)     = mapM_ compileE es >> op (CALL s)

compileASGN ModSet (Variable r  ) e = compileE e >> op (VSET r)
compileASGN ModSet (ArrayRef a i) e = compileE e >> compileE i >> op (ASET a)
compileASGN ModSet (FieldRef i  ) e = compileE e >> compileE i >> op FSET
compileASGN ModSet (BuiltInVar b) e = compileE e >> op (BSET b)

compileINCR Pre (Variable r) = do
   op $ PUSH (VDouble 1)
   op $ MVAR ModAdd r
   op $ VAR r

compileINCR Post (Variable r) = do
   op $ VAR r
   op $ PUSH (VDouble 1)
   op $ MVAR ModAdd r

compileDECR Pre (Variable r) = do
   op $ PUSH (VDouble 1)
   op $ MVAR ModSub r
   op $ VAR r

compileDECT Post (Variable r) = do
   op $ VAR r
   op $ PUSH (VDouble 1)
   op $ MVAR ModSub r

compileS :: Statement -> Compiler ()
compileS (Expression e)    = compileE e
compileS (Block ss)        = drp $ mapM_ compileS ss
compileS (IF t th el)      = compileIF t th el
compileS (WHILE e s)       = compileWHILE e s
compileS (FOR mi mc ms st) = compileFOR mi mc ms st
compileS (DO s e)          = compileDO s e
compileS (PRINT es)        = mapM_ compileE es >> op (PRN (length es))
compileS (BREAK)           = loopBreak
compileS (CONT)            = loopCont

compileIF t th el = do
   compileE t
   jf $ compileS th
   maybe (return ()) compileS el

compileWHILE e s = loop $ do
   compileE e
   loopCheck
   compileS s

compileFOR mi mc ms st = do
   maybe (return ()) compileE mi
   loop $ do
      maybe (return ()) compCheck mc
      compileS st
      maybe (return ()) compileE ms
  where compCheck c = compileE c >> loopCheck


compileDO s e = loop $ do
   compileS s
   compileE e
   loopCheck

loopCheck = gets csLS >>= maybe (return ()) putCheck
  where putCheck ls = do
          i <- nop
          modify $ \s -> s { csLS = Just (ls { lsChecks = i:(lsChecks ls) }) }

loopBreak = gets csLS >>= maybe (return ()) putBreak
  where putBreak ls = do
          i <- nop
          modify $ \s -> s { csLS = Just (ls { lsBreaks = i:(lsBreaks ls) }) }

loopCont = gets csLS >>= maybe (return()) putCont
  where putCont ls = do
          i <- nop
          modify $ \s -> s { csLS = Just (ls { lsConts = i:(lsConts ls) }) }

compileTL :: TopLevel -> Compiler ()
compileTL (Section mp ms) = compileSection mp ms

compileSection mp ms = do
   case mp of
     Nothing         -> compileStmt ms
     (Just BEGIN)    -> compileStmt ms
     (Just (EXPR e)) -> compileE e >> (jf $ compileStmt ms)
     (Just END)      -> compileStmt ms
     otherwise       -> return ()
  where compileStmt = maybe (op $ PRN 0) compileS

compile :: AwkSource -> (Bytecode, Bytecode, Bytecode)
compile src = (cc begins, cc actions, cc ends)
  where cc s    = runCompiler (mapM compileTL s) csInitial
        begins  = filter isBegin src
        actions = procUnits src
        ends    = filter isEnd src
