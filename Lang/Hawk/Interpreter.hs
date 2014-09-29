{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Lang.Hawk.Interpreter where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import GHC.Float (floatToDigits)

import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.Trans
import qualified Data.Map.Strict as Map

import Lang.Hawk.AST

data Value = VString String
           | VDouble Double
             deriving (Eq, Show)

data HawkContext = HawkContext
                 { hcCode     :: AwkSource
                 , hcFields   :: Map.Map Int Value
                 , hcVars     :: Map.Map String Value
                 , hcBVars    :: Map.Map String Value
                 , hcThisLine :: String
                 } deriving (Eq, Show)

emptyContext :: AwkSource -> HawkContext
emptyContext s = HawkContext
             { hcCode     = s
             , hcFields   = Map.empty
             , hcVars     = Map.fromList initialVars
             , hcBVars    = Map.fromList initialBuiltInVars
             , hcThisLine = ""
             }
  where initialVars = map (, VString "") $ vars s 
        initialBuiltInVars = [ ("FNR", VDouble 0)
                             , ("NR",  VDouble 0)
                             , ("NF",  VDouble 0)
                             , ("OFS", VString " ")
                             , ("ORS", VString "\n")
                             ]

newtype Interpreter a = Interpreter (StateT HawkContext (ContT HawkContext IO) a)
                        deriving (Monad, MonadIO, MonadCont, MonadState HawkContext)


runInterpreter :: Interpreter a -> HawkContext -> IO HawkContext
runInterpreter (Interpreter stt) c = runContT (execStateT stt c) return


-- Execute all BEGIN actions, if any
initialize :: Interpreter ()
initialize = do
    actions <- (gets hcCode >>= filterM isBegin)
    forM_ actions $ \(Section _ ms) -> do
      case ms of
        Nothing  -> return ()
        (Just s) -> exec emptyKBlock s
  where isBegin (Section (Just BEGIN) _) = return True
        isBegin _                        = return False

-- Execute all END actions, if any
finalize :: Interpreter ()
finalize = do
    actions <- (gets hcCode >>= filterM isEnd)
    forM_ actions $ \(Section _ ms) -> do
      case ms of
        Nothing  -> return ()
        (Just s) -> exec emptyKBlock s
  where isEnd (Section (Just END) _) = return True
        isEnd _                      = return False


-- This is actually an entry point to the Interpreter.
intMain :: String -> Interpreter ()
intMain inputFile = do
    input <- liftIO $ readFile inputFile
    assignToBVar "=" "FILENAME" (VString inputFile)
    assignToBVar "=" "FNR"      (VDouble 0)
    initialize
    callCC $ \ex -> do
      processLines (emptyKBlock {kExit = ex}) $ lines input
      ex ()
    finalize


processLines :: KBlock -> [String] -> Interpreter ()
processLines _ [] = return ()
processLines k (x:xs) = do
    let k' = k { kNext = \_ -> nextLines k' }
        nextLines kk = processLines kk xs >> (kExit k) ()
    processLine k' x
    nextLines k'


-- processLine takes a new (next) line from input stream, prepares
-- the execution context, and evaluates the awk code with this context.
processLine :: KBlock -> String -> Interpreter ()
processLine k s = do
    oldContext <- get
    let thisFields = map VString $ words s
        thisFldMap = Map.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcThisLine = s
                                 , hcFields   = thisFldMap
                                 }
    put thisContext
    assignToBVar "="  "NF"  (VDouble $ fromIntegral $ length thisFields)
    assignToBVar "+=" "NR"  (VDouble 1)
    assignToBVar "+=" "FNR" (VDouble 1)
    -- find matching actions for this line and execute them
    actions <- (gets hcCode >>= filterM matches)
    forM_ actions $ \(Section _ ms) -> exec k $
       case ms of
         Nothing  -> (PRINT [])
         (Just s) -> s


-- Checks if the given top-level form matches the current line
matches :: TopLevel -> Interpreter Bool
matches (Function _ _ _)     = return False
matches (Section Nothing  _) = return True
matches (Section (Just p) _) = patternMatches p


-- Checks if the given pattern matches the current line
patternMatches :: Pattern -> Interpreter Bool
patternMatches BEGIN    = return False
patternMatches END      = return False
patternMatches (EXPR e) = liftM coerceToBool $ eval e
patternMatches _        = return False -- Not supported yet

unsup s = fail $ s ++ " are not yet supported"


-- Evaluate an expression, return the result
eval :: Expression -> Interpreter Value

eval (Arith op le re) = do
     l <- liftM coerceToDouble $ eval le
     r <- liftM coerceToDouble $ eval re
     case op of
          "*" -> return $ VDouble (l * r)
          "/" -> return $ VDouble (l / r)
          "+" -> return $ VDouble (l + r)
          "-" -> return $ VDouble (l - r)
          otherwise -> fail $ "Unsupported arith operator " ++ op

eval (Const (LitNumeric i)) = return $ VDouble (fromInteger i)
eval (Const (LitStr s))     = return $ VString s
eval (Const _)              = fail "Unsupported literal type"

eval (FieldRef e) = do
     i <- liftM coerceToInt $ eval e
     if i == 0
     then gets hcThisLine >>= (return . VString)
     else do fs <- gets hcFields
             return $ fs Map.! i

eval (VariableRef s) = do
     vs <- gets hcVars
     return $ vs Map.! s

eval (BuiltInVar s) = do
     bvs <- gets hcBVars
     return $ bvs Map.! s

eval (ArrayRef _ _) = unsup "Array references"

eval (Incr n f@(FieldRef e))    = incrField n f
eval (Incr n v@(VariableRef s)) = incrVar n v
eval (Decr n f@(FieldRef e))    = decrField n f
eval (Decr n v@(VariableRef s)) = decrVar n v

eval (Relation op le re) = do
     l <- liftM coerceToDouble $ eval le
     r <- liftM coerceToDouble $ eval re
     case op of -- for now, only numeric values
          "==" -> return $ VDouble $ test (l == r)
          "!=" -> return $ VDouble $ test (l /= r)
          ">"  -> return $ VDouble $ test (l >  r)
          ">=" -> return $ VDouble $ test (l >= r)
          "<"  -> return $ VDouble $ test (l <  r)
          "<=" -> return $ VDouble $ test (l <= r)
          otherwise -> fail $ "Unsupported cmp operator " ++ op
  where test b = if b then 1 else 0

eval (Not e) = do
     b <- liftM coerceToBool $ eval e
     return $ VDouble (if b then 0.0 else 1.0)

eval (Neg e) = do
     d <- liftM coerceToDouble $ eval e
     return $ VDouble (- d)

eval (Concat _ _ )  = unsup "Concatenations"
eval (In _ _)       = unsup "Array membership tests"

eval (Logic op le re) = do
     l <- liftM coerceToBool $ eval le
     r <- liftM coerceToBool $ eval re
     case op of
          "&&" -> return $ VDouble $ test (l && r)
          "||" -> return $ VDouble $ test (l || r)
          otherwise -> fail $ "Unsupported logical operator " ++ op
   where test b = if b then 1 else 0

eval (Match _ _)    = unsup "Regexp matchings"
eval (NoMatch _ _)  = unsup "Regexp matchings"

eval (Assignment op p v) = do
     val <- eval v
     case p of
       (FieldRef ref)     -> assignToField op ref  val
       (VariableRef name) -> assignToVar   op name val
       otherwise -> fail "Only to-field and to-variable assignments are supported"

calcNewValue oldVal op arg =
     case op of
        "="  -> arg
        "+=" -> VDouble $ coerceToDouble oldVal + coerceToDouble arg
        "-=" -> VDouble $ coerceToDouble oldVal - coerceToDouble arg
        "*=" -> VDouble $ coerceToDouble oldVal * coerceToDouble arg
        "/=" -> VDouble $ coerceToDouble oldVal / coerceToDouble arg
        otherwise -> undefined

assignToField op ref val = do
     i <- liftM coerceToInt $ eval ref
     oldFields <- gets hcFields
     let newValue  = calcNewValue (oldFields Map.! i) op val
         newFields = Map.insert i newValue oldFields
     modify (\s -> s { hcFields = newFields })
     reconstructThisLine
     return newValue

reconstructThisLine = do
     thisFields <- gets (Map.toList . hcFields)
     ofs        <- liftM toString $ eval (BuiltInVar "OFS")
     let line = intercalate ofs $ map (toString . snd) thisFields
     modify (\s -> s { hcThisLine = line })
     return ()

assignToVar op name val = do
     oldVars <- gets hcVars
     let newValue = calcNewValue (oldVars Map.! name) op val
         newVars  = Map.insert name newValue oldVars
     modify (\s -> s { hcVars = newVars })
     return newValue

-- Currently for internal use only
assignToBVar op name val = do
     oldBVars <- gets hcBVars
     let newValue = calcNewValue (oldBVars Map.! name) op val
         newBVars = Map.insert name newValue oldBVars
     modify (\s -> s { hcBVars = newBVars })
     return newValue

incrField n fld@(FieldRef e) = do
   oldVal <- eval fld
   newVal <- assignToField "+=" e (VDouble 1.0)
   return (if n == Post then oldVal else newVal)

decrField n fld@(FieldRef e) = do
   oldVal <- eval fld
   newVal <- assignToField "-=" e (VDouble 1.0)
   return (if n == Post then oldVal else newVal)

incrVar n var@(VariableRef s) = do
   oldVal <- eval var
   newVal <- assignToVar "+=" s (VDouble 1.0)
   return (if n == Post then oldVal else newVal)

decrVar n var@(VariableRef s) = do
   oldVal <- eval var
   newVal <- assignToVar "-=" s (VDouble 1.0)
   return (if n == Post then oldVal else newVal)


-- Coercions and conversions
coerceToBool :: Value -> Bool
coerceToBool (VString "") = False
coerceToBool (VDouble 0)  = False
coerceToBool _            = True

coerceToDouble :: Value -> Double
coerceToDouble (VDouble d)  = d
coerceToDouble (VString "") = 0.0
coerceToDouble (VString s)  = read s

coerceToInt :: Value -> Int
coerceToInt (VDouble d)  = truncate d
coerceToInt (VString "") = 0
coerceToInt (VString s)  = read s

toString :: Value -> String
toString (VString s) = s
toString (VDouble d) =
    let (rs,p) = floatToDigits 10 d
    in if length rs == p
       then concat $ map show rs
       else show d

-- Execute a statement
data KBlock = KBlock { kNext  :: (() -> Interpreter ())
                     , kExit  :: (() -> Interpreter ())
                     , kRet   :: (() -> Interpreter ())
                     , kCont  :: (() -> Interpreter ())
                     , kBreak :: (() -> Interpreter ())
                     }

emptyKBlock :: KBlock
emptyKBlock = KBlock { kNext  = return
                     , kExit  = return
                     , kRet   = return
                     , kCont  = return
                     , kBreak = return
                     }

exec :: KBlock -> Statement -> Interpreter ()
exec _ (Expression e) = eval e >> return ()
exec k (Block es)     = mapM_ (exec k) es

exec k (IF c t me)    = do
     b <- liftM coerceToBool $ eval c
     if b
     then exec k t
     else case me of
          Nothing -> return ()
          Just es -> exec k es

exec k w@(WHILE c s) = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextWhile k'}
         nextWhile kk = do
            b <- liftM coerceToBool $ eval c
            when b $ (exec kk s >> nextWhile kk)
            br ()
     nextWhile k'

exec k (FOR i c st s) = callCC $ \br -> do
    let k' = k {kBreak = br, kCont = \_ -> nextFor k'}
        -- TODO: optional expressoins
        initFor    = eval (fromJust i)
        nextFor kk = eval (fromJust st) >> execFor kk
        execFor kk = do
           b <- liftM coerceToBool $ eval (fromJust c)
           when b $ exec kk s >> nextFor kk
           br ()
    initFor
    execFor k'

exec k d@(DO s c) = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextDo k'}
         nextDo kk = do
            exec kk s
            b <- liftM coerceToBool $ eval c
            when b $ nextDo kk
            br ()
     nextDo k'

exec _ (PRINT es) = do
   ofs <- liftM toString $ eval (BuiltInVar "OFS")
   ors <- liftM toString $ eval (BuiltInVar "ORS")
   str <- case es of
      []        -> gets hcThisLine
      otherwise -> liftM (intercalate ofs . map toString) $ mapM eval es
   liftIO $ putStr $ str ++ ors

exec k (BREAK)    = (kBreak k) ()
exec k (CONT)     = (kCont  k) ()
exec k (NEXT)     = (kNext  k) ()
exec k (EXIT _)   = (kExit  k) () -- TODO argument
exec k (RETURN _) = (kRet   k) () -- TODO argument
exec _ (NOP)      = return ()
exec _ (DELETE _) = unsup "`delete` statements"
