{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Lang.Hawk.Bytecode.Compiler (compile, compileNoRefs) where

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as D
import qualified Data.Foldable as F (toList)
import Data.Sequence ((|>))
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (forM_, liftM, replicateM_)
import Control.Monad.Trans.State.Strict

import Lang.Hawk.Basic
import Lang.Hawk.AST
import Lang.Hawk.Bytecode
import Lang.Hawk.Value
import Lang.Hawk.Analyzer

data CompilerState = CompilerState
                     { csBC  :: (D.Seq OpCode)
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

type Compiler a = State CompilerState a

runCompiler :: Compiler a -> CompilerState -> D.Seq OpCode
--runCompiler (Compiler c) b = csBC $ execState c b
runCompiler c b = csBC $ execState c b

loop :: Compiler () -> Compiler ()
loop body = do
   oldState <- gets csLS
   enter <- pos
   modify $ \s -> s { csLS = Just (initialLoopState enter) }
   replicateM_ 1 body >> op (JMP enter)
   leave <- pos
   loopState <- liftM fromJust $ gets csLS
   forM_ (lsBreaks loopState) $ putOP (JMP leave)
   forM_ (lsConts  loopState) $ putOP (JMP enter)
   forM_ (lsChecks loopState) $ putOP (JF  leave)
   modify $ \s -> s { csLS = oldState }

op :: OpCode -> Compiler ()
op c = modify $ \s -> s {csBC = (csBC s) |> seq c c, csCur = succ (csCur s)}

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
drp c = c >> do 
   s <- gets csBC
   case D.viewr s of
      D.EmptyR     -> return ()
      (_ D.:> DRP) -> return ()
      (_ D.:> _)   -> op DRP


compileE :: Expression -> Compiler ()
compileE (Arith a l r)      = compileE l >> compileE r >> op (ARITH a)
compileE (Const a)          = op (PUSH (toValue a))
compileE (FieldRef e)       = compileE e >> op FIELD
compileE (VariableRef r)    = op (VAR' r)
compileE (ArrayRef a e)     = compileE e >> op (ARR' a)
compileE (BuiltInVar b)     = op (BVAR b)
compileE (Assignment m p e) = compileASGN m p e
compileE (Incr n e)         = compileINCR n e
compileE (Decr n e)         = compileDECR n e
compileE (Relation c l r)   = compileE l >> compileE r >> op (CMP c)
compileE (Not e)            = compileE e >> op NOT
compileE (Neg e)            = compileE e >> op NEG
compileE (Id e)             = compileE e
compileE (In e (VariableRef v)) = compileE e >> op (IN' v) 
compileE (Logic AND l r)    = compileAND l r
compileE (Logic OR  l r)    = compileOR  l r
compileE (Match l r)        = compileE l >> compileE r >> op MATCH
compileE (NoMatch l r)      = compileE l >> compileE r >> op MATCH >> op NOT
compileE (FunCall f@GSub vs) = compileFSub f vs
compileE (FunCall f@FSub  vs) = compileFSub f vs
compileE (FunCall Split  vs) = compileSPLIT vs
compileE (FunCall s      vs) = mapM_ compileE vs >> (op $ CALL s (length vs))
compileE (Concat l r)        = compileE l >> compileE r >> op CAT
compileE (Getline)           = op $ GETL
compileE (GetlineVar (VariableRef v)) = op $ GETLV' v
compileE (FGetline f)        = compileE f >> op FGETL
compileE (FGetlineVar (VariableRef v) f) = compileE f >> op (FGETLV' v)
compileE (PGetline cmd)      = compileE cmd >> op PGETL
compileE (PGetlineVar cmd (VariableRef v)) = compileE cmd >> op (PGETLV' v)
compileE (InlineIf c t e)    = compileIIF c t e

compileFSub f [a1,a2] = do
   compileE a1
   compileE a2
   op $ PUSH (VDouble 0)
   op $ FIELD
   op $ CALL f 3
   op $ PUSH (VDouble 0)
   op $ FSET

compileFSub f [a1,a2,(VariableRef a)] = do
   compileE a1
   compileE a2
   op $ VAR' a
   op $ CALL f 3
   op $ VSET' a

compileFSub f [a1,a2,(ArrayRef a i)] = do
   compileE a1
   compileE a2
   compileE i
   op $ ARR' a
   op $ CALL f 3
   compileE i
   op $ ASET' a

compileFSub f [a1,a2,(FieldRef e)] = do
   compileE a1
   compileE a2
   compileE e
   op $ FIELD
   op $ CALL f 3
   compileE e
   op $ FSET

compileSPLIT [a1,(VariableRef a)] = do
   compileE a1
   op $ BVAR FS
   op $ SPLIT' a

compileSPLIT [a1,(VariableRef a),a3] = do
   compileE a1
   compileE a3
   op $ SPLIT' a

compileASGN m p e =
   if m == Set
   then compileE e >> compileSET p
   else compileMOD' Pre m p (compileE e)

compileSET (VariableRef r  )  = op (VSET' r)
compileSET (ArrayRef    a i)  = compileE i >> op (ASET' a)
compileSET (FieldRef      i) = compileE i >> op FSET
compileSET (BuiltInVar  b  ) = op (BSET b)

compileMOD m (VariableRef r  ) = op (VMOD' m r)
compileMOD m (ArrayRef    a i) = compileE i >> op (AMOD' m a)
compileMOD m (FieldRef      i) = compileE i >> op (FMOD m)
compileMOD m (BuiltInVar  b  ) = op (BMOD m b)

compileMOD' Pre m p ce = do
  ce             -- compile the expression, put its value on stack
  compileMOD m p -- modify the <var/bvar/field/arr> using this value
  compileE p     -- read <var/bvar/field/arr> - put new value on stack

compileMOD' Post m p ce = do
  compileE p     -- read <var/bvar/field/arr> - put new value on stack
  ce             -- compile the expression, put its value on stack
  compileMOD m p -- modify the <var/bvar/field/arr>, stack top now is the old value

compileINCR n p = compileMOD' n Add p $ op $ PUSH (VDouble 1)
compileDECR n p = compileMOD' n Sub p $ op $ PUSH (VDouble 1)

compileAND l r = do
  compileE l
  afterL <- nop
  compileE r
  afterR <- nop
  exit <- pos
  op $ PUSH (VDouble 0)
  end <- pos
  putOP (JF exit) afterL
  putOP (JMP end) afterR

compileOR l r = do
  compileE l
  afterL <- nop
  compileE r
  afterR <- nop
  exit <- pos
  op $ PUSH (VDouble 1)
  end <- pos
  putOP (JT exit) afterL
  putOP (JMP end) afterR

compileS :: Statement -> Compiler ()
compileS (Expression e)    = compileE e
compileS (Block ss)        = drp $ mapM_ compileS ss
compileS (IF t th el)      = compileIF t th el
compileS (WHILE e s)       = compileWHILE e s
compileS (FOR mi mc ms st) = compileFOR mi mc ms st
compileS (FOREACH r a st)  = compileFOREACH r a st
compileS (DO s e)          = compileDO s e
compileS (PRINT es)        = mapM_ compileE (reverse es) >> op (PRN (length es))
compileS (FPRINT es m fe)  = mapM_ compileE (reverse es) >> compileE fe >> op (FPRN (length es) m)
compileS (PPRINT es fe)    = mapM_ compileE (reverse es) >> compileE fe >> op (PPRN (length es))
compileS (BREAK)           = loopBreak
compileS (CONT)            = loopCont
compileS (NEXT)            = op $ NXT
compileS (EXIT _)          = op $ EX -- TODO exit code
compileS (NOP)             = op $ DRP
compileS (DELELM r e)      = compileE e >> op (ADEL' r)
compileS (DELARR r)        = op (ADRP' r)

compileIF t th el = do
   compileE t
   check <- nop
   compileS th
   case el of
      Nothing  -> do
          end <- pos
          putOP (JF end) check
      (Just s) -> do
          afterThen <- nop
          enterElse <- pos
          compileS s
          end <- pos
          putOP (JF  enterElse) check
          putOP (JMP end) afterThen

compileIIF c t e = do
   compileE c
   check <- nop
   compileE t
   afterThen <- nop
   enterElse <- pos
   compileE e
   end <- pos 
   putOP (JF  enterElse) check
   putOP (JMP end) afterThen

compileFOR mi mc ms st = do
   maybe (return ()) compileE mi
   loop $ do
      maybe (return ()) compCheck mc
      compileS st
      maybe (return ()) compileE ms
  where compCheck c = compileE c >> loopCheck

compileWHILE e s = loop $ compileE e >> loopCheck >> compileS s
compileDO    s e = loop $ compileS s >> compileE e >> loopCheck

compileFOREACH (VariableRef r) a st = do
   op $ (FETCH' a)
   loop $ do
      op $ ACHK
      loopCheck
      op $ (ANXT' r)
      compileS st
   op $ KDRP

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
          modify $ \s -> s { csLS = Just (ls { lsConts  = i:(lsConts  ls) }) }

compileTL :: TopLevel -> Compiler ()
compileTL (Section mp ms) = compileSection mp ms

compileSection mp ms = do
   case mp of
     Nothing         -> compileStmt ms
     (Just BEGIN)    -> compileStmt ms
     (Just (EXPR e)) -> compileE  e >> (jf $ compileStmt ms)
     (Just (RE s))   -> compileRE s >> (jf $ compileStmt ms)
     (Just END)      -> compileStmt ms
     otherwise       -> return ()
  where compileStmt = maybe (op $ PRN 0) compileS
        compileRE s = do op $ PUSH $ VDouble 0
                         op $ FIELD
                         op $ PUSH $ valstr (B.pack s)
                         op $ MATCH

compileNoRefs :: AwkSource -> ([OpCode], [OpCode], [OpCode])
compileNoRefs src = (cc begins, cc actions, cc ends)
  where cc s    = F.toList $ runCompiler (mapM compileTL s) csInitial
        begins  = filter isBegin src
        actions = procUnits src
        ends    = filter isEnd src

compile :: AwkSource -> IO ([OpCode], [OpCode], [OpCode])
compile src = do
  let (startup, opcodes, shutdown) = compileNoRefs src
  rt <- mkRewriteTable (analyze src)
  return (awkPrepare rt startup, awkPrepare rt opcodes, awkPrepare rt shutdown)

procUnits :: AwkSource -> AwkSource
procUnits s = filter p s
  where p (Section mp _) = not (mp == Just BEGIN || mp == Just END)
        p _              = False
