{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.Hawk.Analyzer where

import Data.IORef
import Data.List (find)
import Data.Maybe (isNothing)
import Control.Applicative (Applicative, (<*>), (<$>), pure)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Lang.Hawk.Basic
import Lang.Hawk.Value

import Lang.Hawk.AST

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
             , eArrays   :: Bool
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
             , eArrays   = False
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

updVarTag  s t = modify (\e -> e {eVars     = M.insert s t (eVars  e)}) >> return t
updBVarTag b t = modify (\e -> e {eBVars    = M.insert b t (eBVars e)}) >> return t
updFields    t = modify (\e -> e {eFields   = t})    >> return t
updArrs        = modify (\e -> e {eArrays   = True}) >> return GLOBAL
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
trAsgn _   (ArrayRef _ _)  rhs = updArrs 

trMod (VariableRef s) = varTag s >>= \t -> if t == UNDEF then updVarTag s GLOBAL else return t
trMod (FieldRef i)    = gets eFields
trMod (ArrayRef _ _)  = updArrs
trMod (BuiltInVar b)  = updBVarTag b GLOBAL

traceE :: Expression -> Tracer Tag
traceE (Arith _ lhs rhs)      = cmb' (traceE lhs) (traceE rhs)
traceE (Const _)              = return LOCAL
traceE (FieldRef _)           = gets eFields
traceE (VariableRef s)        = trVarRead s
traceE (ArrayRef _ _)         = updArrs
traceE (BuiltInVar b)         = bvarTag b
traceE (Assignment m p v)     = trAsgn m p v
traceE (Incr _ p)             = trMod p 
traceE (Decr _ p)             = trMod p
traceE (Relation _ lhs rhs)   = cmb' (traceE lhs) (traceE rhs)
traceE (Not e)                = traceE e
traceE (Neg e)                = traceE e
traceE (Id e)                 = traceE e
traceE (In (VariableRef s) _) = updVarTag s GLOBAL
traceE (Logic _ lhs rhs)      = cmb' (traceE lhs) (traceE rhs)
traceE (Match lhs rhs)        = cmb' (traceE lhs) (traceE rhs)
traceE (NoMatch lhs rhs)      = cmb' (traceE lhs) (traceE rhs)
traceE (FunCall _ _)          = updFunCalls
traceE (InlineIf c t f)       = cmb' (traceE c) (cmb' (traceE t) (traceE f))

traceS :: Statement -> Tracer ()
traceS (Expression e)         = traceE e >> return ()
traceS (Block ss)             = mapM_ traceS ss
traceS (IF e s ms)            = traceE e >> (trBlock $ traceS s >> mtry traceS ms)
traceS (WHILE e s)            = trBlock $ traceE e >> traceS s
traceS (FOR mi mc ms s)       = trBlock $ mapM_ (mtry traceE) [mi, mc, ms] >> traceS s
traceS (FOREACH e _ s)        = trBlock $ traceE e >> traceS s
traceS (DO s e)               = trBlock $ traceS s >> traceE e >> return ()
traceS (PRINT es)             = mapM_ traceE es
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

procUnits :: AwkSource -> AwkSource
procUnits s = filter p s
  where p (Section mp _) = not (mp == Just BEGIN || mp == Just END)
        p _              = False

awkPure :: AwkSource -> Bool
awkPure s =  noGlobalVars && noBVarsModified && noArrays
          && noFunCalls   && noRangePatterns && noControlFlow
   where 
     noGlobalVars    = isNothing $ find ((== GLOBAL) . snd) $ M.toList $ eVars  efs 
     noBVarsModified = all              ((== LOCAL)  . snd) $ M.toList $ eBVars efs
     noArrays        = not $ eArrays   efs
     noFunCalls      = not $ eFunCalls efs 
     noRangePatterns = not $ eRanges   efs
     noControlFlow   = not $ eFlow     efs

     efs = analyze $ procUnits s

awkPrepare :: AwkSource -> IO AwkSource
awkPrepare src = do
   let efs = analyze src
   m <- liftM M.fromList $ mapM mkRef (map fst $ M.toList $ eVars efs)
   return $ runRewrite (mapM putRefsTL src) m
 where mkRef s = do r <- newIORef (VDouble 0)  
                    return (s,r)

-- TODO: Integrate into Parser (AFAIR Parsec allows it)
type VarMap = M.Map String (IORef Value)

newtype Rewrite a = Rewrite (Reader VarMap a)
                   deriving (Monad, MonadReader VarMap, Applicative, Functor)

runRewrite :: Rewrite a -> (M.Map String (IORef Value)) -> a
runRewrite (Rewrite r) v = runReader r v

putRefsE :: Expression -> Rewrite Expression
putRefsE (Arith a lhs rhs)      = Arith <$> pure a <*> putRefsE lhs <*> putRefsE rhs
putRefsE e@(Const _)            = return e 
putRefsE (FieldRef e)           = FieldRef <$> putRefsE e
putRefsE (VariableRef s)        = Variable <$> asks (M.! s)
putRefsE (ArrayRef s e)         = ArrayRef <$> pure s <*> putRefsE e
putRefsE e@(BuiltInVar _)       = return e
putRefsE (Assignment m p v)     = Assignment <$> pure m <*> putRefsE p <*> putRefsE v
putRefsE (Incr p e)             = Incr <$> pure p <*> putRefsE e
putRefsE (Decr p e)             = Decr <$> pure p <*> putRefsE e
putRefsE (Relation r lhs rhs)   = Relation <$> pure r <*> putRefsE lhs <*> putRefsE rhs
putRefsE (Not e)                = Not <$> putRefsE e
putRefsE (Neg e)                = Neg <$> putRefsE e
putRefsE (Id e)                 = Id  <$> putRefsE e
putRefsE (In v@(VariableRef s) a) = In <$> putRefsE v <*> pure a
putRefsE (Logic l lhs rhs)      = Logic <$> pure l <*> putRefsE lhs <*> putRefsE rhs
putRefsE (Match lhs rhs)        = Match <$> putRefsE lhs <*> putRefsE rhs
putRefsE (NoMatch lhs rhs)      = NoMatch <$> putRefsE lhs <*> putRefsE rhs
putRefsE (FunCall s args)       = FunCall <$> pure s <*> mapM putRefsE args
putRefsE (InlineIf c t f)       = InlineIf <$> putRefsE  c <*> putRefsE t <*> putRefsE f

putRefsS :: Statement -> Rewrite Statement
putRefsS (Expression e)         = Expression <$> putRefsE e
putRefsS (Block ss)             = Block <$> mapM putRefsS ss
putRefsS (IF e s ms)            = IF <$> putRefsE e <*> putRefsS s <*> msrefs ms
putRefsS (WHILE e s)            = WHILE <$> putRefsE e <*> putRefsS s
putRefsS (FOR mi mc ms s)       = FOR <$> mrefs mi <*> mrefs mc <*> mrefs ms <*> putRefsS s
putRefsS (FOREACH e a s)        = FOREACH <$> putRefsE e <*> pure a <*> putRefsS s
putRefsS (DO s e)               = DO <$> putRefsS s <*> putRefsE e
putRefsS (PRINT es)             = PRINT <$> mapM putRefsE es
putRefsS (EXIT me)              = EXIT <$> mrefs me
putRefsS (BREAK)                = return BREAK
putRefsS (CONT)                 = return CONT
putRefsS (NEXT)                 = return NEXT
putRefsS (NOP)                  = return NOP
putRefsS (DELETE e)             = DELETE <$> putRefsE e
putRefsS (RETURN me)            = RETURN <$> mrefs me

putRefsP :: Pattern -> Rewrite Pattern
putRefsP (EXPR e)               = EXPR <$> putRefsE e
putRefsP (RANGE ps pe)          = RANGE <$> putRefsP ps <*> putRefsP pe
putRefsP BEGIN                  = return BEGIN
putRefsP END                    = return END
putRefsP re@(RE _)              = return re

putRefsTL :: TopLevel -> Rewrite TopLevel
putRefsTL (Section mp ms)       = Section <$> mm putRefsP mp <*> mm putRefsS ms
putRefsTL (Function s args ss)  = Function <$> pure s <*> pure args <*> putRefsS ss

-- TODO: FMAP is the next step...
mm :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mm  f (Just a) = f a >>= (return . Just)
mm  f Nothing  = return Nothing

mrefs :: Maybe Expression -> Rewrite (Maybe Expression)
mrefs me = mm putRefsE me

msrefs :: Maybe Statement -> Rewrite (Maybe Statement)
msrefs ms = mm putRefsS ms
