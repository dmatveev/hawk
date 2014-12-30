{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Lang.Hawk.Analyzer (Effects, analyze, awkPrepare, mkRewriteTable) where

import Data.IORef (IORef, newIORef)
import Data.List (find)
import Data.Maybe (isNothing)
import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Lang.Hawk.Basic
import Lang.Hawk.Value
import Lang.Hawk.AST
import Lang.Hawk.Bytecode

-- Analyze the program's control flow and how state (variables) is accessed.
-- We are interested only in processing actions (i.e. all except BEGIN/END).
-- 
-- In AWK, all variables are global by default (though functions can have its
-- own local variables, but we omit it now for the simplicity).
--
-- Here we will track how each variable is accessed or modified in the source.
-- We introduce three states for a variable: UNDEF, LOCAL, and GLOBAL.
-- 
--           UNDEF --->--->--->--->--->--->--->--->---> GLOBAL
--  (assignment :       (Modification based on            ^
--   to LOCAL   :       previous state or assignment      |
--   value)     V       based on GLOBAL value; reading)   |
--           LOCAL --->--->--->--->--->--->--->--->--->--->
--
--
-- UNDEF is the initial state of any variable.
-- LOCAL variable is a variable with LOCAL (independent from the previous
--   iteration) value.
-- A variable becomes GLOBAL if its value is modified using its previous
--   value (or using other GLOBAL variable), or if it is read-accessed.
--
-- If a variable is accessed in a branch of conditional/control statement,
-- like IF, WHILE, FOR, etc, we can trace the changes in the branch but cannt
-- predict the runtime flow. So, even with LOCAL assignment to UNDEF, we still
-- will receive a GLOBAL variable.
--
-- As it is hard to determine the array subscript in the compile time, all
-- array accesses are considered GLOBAL.
--
-- For the simplicity, we consider all function calls breaking the system state
-- (since a user-defined function can have side effects). Sure, it is possible
-- to track functions too, but we omit it for now.
--
-- Built-in variables are initially LOCAL. Modifications to the built-in variables
-- are tracked in the special way (see below).
--
-- Fields are initially LOCAL. If any of the fields is assigned to a GLOBAL value,
-- *all* fields become GLOBAL.

data Tag = UNDEF | LOCAL | GLOBAL
         deriving (Eq, Show)

data Effects = Effects
             { eVars     :: !(M.Map String Tag)
             , eBVars    :: !(M.Map BVar Tag)
             , eFields   :: Tag
             , eArrays   :: S.Set String
             , eFunCalls :: Bool
             , eRanges   :: Bool
             , eDepth    :: Int
             , eFlow     :: Bool
             } deriving (Show)

emptyEffects :: Effects
emptyEffects = Effects
             { eVars     = M.empty
             , eBVars    = M.empty
             , eFields   = LOCAL
             , eArrays   = S.empty
             , eFunCalls = False
             , eRanges   = False
             , eDepth    = 0
             , eFlow     = False
             }

newtype Tracer a = Tracer (State Effects a)
                   deriving (Monad, MonadState Effects, Applicative, Functor)

runTracer :: Tracer a -> Effects -> Effects
runTracer (Tracer s) e = execState s e

mtry :: (Monad m) => (a -> m b) -> Maybe a -> m ()
mtry _ Nothing  = return ()
mtry f (Just a) = f a >> return ()

cmb' :: Tracer Tag -> Tracer Tag -> Tracer Tag
cmb' p v = cmb <$> gets eDepth <*> p <*> v 

cmb :: Int -> Tag -> Tag -> Tag
cmb _ GLOBAL _      = GLOBAL
cmb _ _      GLOBAL = GLOBAL
cmb 0 UNDEF  LOCAL  = LOCAL
cmb _ UNDEF  LOCAL  = GLOBAL
cmb _ LOCAL  LOCAL  = LOCAL

updVarTag  s t = modify (\e -> e {eVars     = M.insert s t (eVars   e)}) >> return t
updBVarTag b t = modify (\e -> e {eBVars    = M.insert b t (eBVars  e)}) >> return t
updFields    t = modify (\e -> e {eFields   = t})    >> return t
updArr     s   = modify (\e -> e {eArrays   = S.insert s   (eArrays e)}) >> return GLOBAL
updFunCalls    = modify (\e -> e {eFunCalls = True}) >> return GLOBAL
updRange       = modify (\e -> e {eRanges   = True}) >> return GLOBAL
updFlow        = modify (\e -> e {eFlow     = True})

varTag :: String -> Tracer Tag
varTag s = gets (M.findWithDefault UNDEF s . eVars)

trVarRead s = do
   tag <- varTag s
   case tag of
      UNDEF     -> updVarTag s GLOBAL
      otherwise -> return tag

bvarTag :: BVar -> Tracer Tag
bvarTag b = gets (M.findWithDefault LOCAL b . eBVars)

trBlock f = do current <- gets eDepth
               modify $ \s -> s { eDepth = succ current }
               f
               modify $ \s -> s { eDepth = current }

trAsgn Set (VariableRef s) rhs = traceE rhs >>= updVarTag s
trAsgn _   (VariableRef s) rhs = cmb' (varTag s) (traceE rhs) >>= updVarTag s
trAsgn _   (BuiltInVar  b) rhs = updBVarTag b GLOBAL
trAsgn _   (FieldRef _)    rhs = cmb' (gets eFields) (traceE rhs) >>= updFields 
trAsgn _   (ArrayRef s _)  rhs = updArr s 

trMod (VariableRef s) = varTag s >>= \t -> if t == UNDEF then updVarTag s GLOBAL else return t
trMod (FieldRef i)    = gets eFields
trMod (ArrayRef s _)  = updArr s
trMod (BuiltInVar b)  = updBVarTag b GLOBAL

traceE :: Expression -> Tracer Tag
traceE (Arith _ lhs rhs)    = cmb' (traceE lhs) (traceE rhs)
traceE (Const _)            = return LOCAL
traceE (FieldRef _)         = gets eFields
traceE (VariableRef s)      = trVarRead s
traceE (ArrayRef s _)       = updArr s
traceE (BuiltInVar b)       = bvarTag b
traceE (Assignment m p v)   = trAsgn m p v
traceE (Incr _ p)           = trMod p 
traceE (Decr _ p)           = trMod p
traceE (Relation _ lhs rhs) = cmb' (traceE lhs) (traceE rhs)
traceE (Not e)              = traceE e
traceE (Neg e)              = traceE e
traceE (Id e)               = traceE e
traceE (In (VariableRef s) (VariableRef a)) = updArr a >> updVarTag s GLOBAL
traceE (In _               (VariableRef a)) = updArr a
traceE (Logic _ lhs rhs)    = cmb' (traceE lhs) (traceE rhs)
traceE (Match lhs rhs)      = cmb' (traceE lhs) (traceE rhs)
traceE (NoMatch lhs rhs)    = cmb' (traceE lhs) (traceE rhs)
traceE (InlineIf c t f)     = cmb' (traceE c) (cmb' (traceE t) (traceE f))
traceE (FunCall f vs)       = trf f vs >> updFunCalls
traceE (Concat lhs rhs)     = cmb' (traceE lhs) (traceE rhs) 
traceE (Getline)            = return GLOBAL
traceE (GetlineVar (VariableRef s)) = updVarTag s GLOBAL
traceE (FGetline f)         = traceE f >> return GLOBAL
traceE (FGetlineVar v f)    = traceE v >> traceE f >> return GLOBAL
traceE (PGetline cmd)       = traceE cmd >> return GLOBAL
traceE (PGetlineVar cmd v)  = traceE cmd >> traceE v >> return GLOBAL

trf GSub  [a1,a2,(VariableRef a)] = traceE a1 >> traceE a2 >> updVarTag a GLOBAL
trf GSub  [a1,a2,(ArrayRef a  i)] = traceE a1 >> traceE a2 >> updArr a >> traceE i
trf GSub  [a1,a2,(FieldRef    e)] = traceE a1 >> traceE a2 >> traceE e >> updFields GLOBAL
trf FSub  [a1,a2,(VariableRef a)] = traceE a1 >> traceE a2 >> updVarTag a GLOBAL
trf FSub  [a1,a2,(ArrayRef a  i)] = traceE a1 >> traceE a2 >> updArr a >> traceE i
trf FSub  [a1,a2,(FieldRef    e)] = traceE a1 >> traceE a2 >> traceE e >> updFields GLOBAL
trf Split [a1,(VariableRef a)]    = traceE a1 >> updArr a
trf Split [a1,(VariableRef a),a3] = traceE a1 >> traceE a3 >> updArr a
trf _     vs                      = mapM_ traceE vs >> updFunCalls

traceS :: Statement -> Tracer ()
traceS (Expression e)         = traceE e >> return ()
traceS (Block ss)             = mapM_ traceS ss
traceS (IF e s ms)            = traceE e >> (trBlock $ traceS s >> mtry traceS ms)
traceS (WHILE e s)            = trBlock $ traceE e >> traceS s
traceS (FOR mi mc ms s)       = trBlock $ mapM_ (mtry traceE) [mi, mc, ms] >> traceS s
traceS (FOREACH e a s)        = trBlock $ traceE e >> updArr a >> traceS s
traceS (DO s e)               = trBlock $ traceS s >> traceE e >> return ()
traceS (PRINT es)             = mapM_ traceE es
traceS (FPRINT es _ fe)       = mapM_ traceE es >> traceE fe >> return ()
traceS (PPRINT es fe)         = mapM_ traceE es >> traceE fe >> return ()
traceS (EXIT me)              = mtry traceE me >> updFlow
traceS (BREAK)                = return ()
traceS (CONT)                 = return ()
traceS (NEXT)                 = updFlow
traceS (NOP)                  = return ()
traceS (DELETE e)             = traceE e >> return ()
traceS (RETURN me)            = mtry traceE me >> updFlow

traceP :: Pattern -> Tracer ()
traceP (EXPR e)               = traceE e >> return ()
traceP (RANGE _ _)            = updRange >> return ()
traceP _                      =             return ()

traceTL :: TopLevel -> Tracer ()
traceTL (Section mp ms)       = mtry traceP mp >> mtry traceS ms
traceTL _                     = return ()

analyze :: AwkSource -> Effects
analyze ts = runTracer (mapM_ traceTL ts) emptyEffects

awkPure :: AwkSource -> Bool
awkPure s =  noGlobalVars && noBVarsModified && noArrays
          && noFunCalls   && noRangePatterns && noControlFlow
   where 
     noGlobalVars    = isNothing $ find ((== GLOBAL) . snd) $ M.toList $ eVars  efs 
     noBVarsModified = all              ((== LOCAL)  . snd) $ M.toList $ eBVars efs
     noArrays        = S.null $ eArrays   efs
     noFunCalls      = not    $ eFunCalls efs 
     noRangePatterns = not    $ eRanges   efs
     noControlFlow   = not    $ eFlow     efs

     efs = analyze s

awkPrepare :: RewriteTable -> [OpCode] -> [OpCode]
awkPrepare t ops = runRewrite (mapM putRefsOp ops) t

-- TODO: Integrate into Parser (AFAIR Parsec allows it)
data RewriteTable = RewriteTable
                    { rtVars :: M.Map String (IORef Value)
                    , rtArrs :: M.Map String (IORef Array)
                    }

mkRewriteTable :: Effects -> IO RewriteTable
mkRewriteTable efs =
  RewriteTable
    <$> (liftM M.fromList $ mapM (mkRef $ VDouble 0) (map fst $ M.toList $ eVars efs))
    <*> (liftM M.fromList $ mapM (mkRef $ M.empty)   (S.toList $ eArrays efs))
 where mkRef a s = (s,) <$> newIORef a

newtype Rewrite a = Rewrite (Reader RewriteTable a)
                   deriving (Monad, MonadReader RewriteTable, Applicative, Functor)

runRewrite :: Rewrite a -> RewriteTable -> a
runRewrite (Rewrite r) t = runReader r t

var s = asks ((M.! s) . rtVars)
arr s = asks ((M.! s) . rtArrs)

putRefsOp :: OpCode -> Rewrite OpCode
putRefsOp (VAR'    s) = VAR   <$> var s
putRefsOp (VSET'   s) = VSET  <$> var s
putRefsOp (VMOD' m s) = VMOD  <$> pure m <*> var s
putRefsOp (ARR'    s) = ARR   <$> arr s
putRefsOp (ASET'   s) = ASET  <$> arr s
putRefsOp (AMOD' m s) = AMOD  <$> pure m <*> arr s
putRefsOp (ANXT'   s) = ANXT  <$> var s
putRefsOp (FETCH'  s) = FETCH <$> arr s 
putRefsOp (IN'     s) = IN    <$> arr s
putRefsOp (ADEL'   s) = ADEL  <$> arr s
putRefsOp (ADRP'   s) = ADRP  <$> arr s
putRefsOp (SPLIT'  s) = SPLIT <$> arr s
putRefsOp (GETLV'  s) = GETLV <$> var s
putRefsOp (FGETLV' s) = FGETLV <$> var s
putRefsOp (PGETLV' s) = PGETLV <$> var s
putRefsOp op          = return (seq op op)
