{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.Hawk.Bytecode.Compiler where

import qualified Data.Sequence as D
import Data.Sequence ((|>))
import Control.Monad.State.Strict

import Lang.Hawk.Basic
import Lang.Hawk.AST
import Lang.Hawk.Bytecode
import Lang.Hawk.Value

data CompilerState = CompilerState {csBC :: Bytecode, csCur :: Int}

csInitial :: CompilerState
csInitial = CompilerState { csBC = D.empty, csCur = 0 } 

newtype Compiler a = Compiler (State CompilerState a)
                   deriving (Monad, MonadState CompilerState)

runCompiler :: Compiler a -> CompilerState -> Bytecode
runCompiler (Compiler c) b = csBC $ execState c b

op :: OpCode -> Compiler ()
op c = modify $ \s -> s {csBC = (csBC s) |> c, csCur = succ (csCur s)}

jf :: Compiler () -> Compiler ()
jf c = do
   i <- gets csCur
   op (JF 0) >> c
   e <- gets csCur
   modify $ \s -> s {csBC = D.update i (JF e) (csBC s)}

drp :: Compiler () -> Compiler ()
drp c = c >> op DRP

toValue :: Literal -> Value
toValue = undefined

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

compileASGN ModSet (Variable r) e = compileE e >> op (VSET r)

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
compileS (Expression e) = compileE e
compileS (Block ss)     = drp $ mapM_ compileS ss
compileS (IF t th el)   = compileIF t th el
compileS (PRINT es)     = mapM_ compileE es >> op (PRN (length es))

compileP :: Pattern -> Compiler ()
compileP (EXPR e) = compileE e
compileP _        = op $ PUSH (VDouble 1)

compileIF t th el = do
   compileE t
   jf $ compileS th
   maybe (return ()) compileS el

compileTL :: TopLevel -> Compiler ()
compileTL (Section mp ms) = compileSection mp ms

compileSection mp ms = do
   case mp of
     Nothing         -> compileStmt ms
     (Just (EXPR e)) -> do compileE e
                           jf $ compileStmt ms
  where compileStmt = maybe (op $ PRN 0) compileS

