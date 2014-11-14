{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Regex.TDFA

import Data.List (find, intercalate)
import Data.Maybe (fromJust, fromMaybe)

import GHC.Float (floatToDigits)

import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.Trans
import qualified Data.Map.Lazy as M

import System.Random
import System.IO

import Lang.Hawk.AST

data Value = VString !B.ByteString
           | VDouble !Double
             deriving (Eq, Show)

data HawkContext = HawkContext
                 { hcCode     :: !AwkSource
                 , hcFields   :: !(M.Map Int Value)
                 , hcVars     :: !(M.Map String Value)
                 , hcArrays   :: !(M.Map (String, String) Value)
                 , hcBVars    :: !(M.Map String Value)
                 , hcStack    :: ![M.Map String Value]
                 , hcRetVal   :: !Value
                 , hcThisLine :: !B.ByteString
                 , hcStdGen   :: StdGen
                 }

(*!) :: Ord k => M.Map k Value -> k -> Value
m *! k = M.findWithDefault (VDouble 0) k m

emptyContext :: AwkSource -> HawkContext
emptyContext s = HawkContext
                 { hcCode     = s
                 , hcFields   = M.empty
                 , hcVars     = M.empty
                 , hcArrays   = M.empty
                 , hcBVars    = M.fromList initialBuiltInVars
                 , hcStack    = []
                 , hcRetVal   = VDouble 0
                 , hcThisLine = ""
                 , hcStdGen   = mkStdGen 0
                 }
  where initialBuiltInVars = [ ("FNR", VDouble 0)
                             , ("NR",  VDouble 0)
                             , ("NF",  VDouble 0)
                             , ("OFS", VString " ")
                             , ("FS",  VString " ")
                             , ("ORS", VString "\n")
                             , ("RS",  VString "\n")
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
intMain :: Handle -> String -> Interpreter ()
intMain h inputFile = do
    assignToBVar "=" "FILENAME" (VString $ B.pack inputFile)
    assignToBVar "=" "FNR"      (VDouble 0)
    initialize
    callCC $ \ex -> do
       let k = emptyKBlock {kExit = ex}
       readLoop k B.empty False
       ex ()
    finalize
  where
    readLoop k thisBuf eof = do
      rs <- liftM toString $ eval (BuiltInVar "RS")
      let nrs = B.length rs
      case B.breakSubstring rs thisBuf of
         (l, rest) | B.null l && B.null rest && eof ->
                        return ()
                   | B.null rest && eof -> do
                        let k' = k { kNext = (const $ return ()) }
                        seq k' $ processLine k' l
                   | B.null rest && not eof -> do
                        nextChunk <- liftIO $ B.hGet h 8192
                        readLoop k (B.append thisBuf nextChunk) (B.null nextChunk)  
                   | otherwise -> do
                        let r' = B.drop nrs rest
                            k' = k { kNext = \_ -> readLoop k' r' eof } 
                        seq k' $ processLine k' l
                        readLoop k' r' eof


-- processLine takes a new (next) line from input stream, prepares
-- the execution context, and evaluates the awk code with this context.
processLine :: KBlock -> B.ByteString -> Interpreter ()
processLine k s = do
    oldContext <- get
    thisFields <- liftM (map VString) $ splitIntoFields s
    let thisFldMap = M.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcThisLine = s
                                 , hcFields   = thisFldMap
                                 }
    put $! thisContext
    assignToBVar "="  "NF"  (VDouble $ fromIntegral $ length thisFields)
    assignToBVar "+=" "NR"  (VDouble 1)
    assignToBVar "+=" "FNR" (VDouble 1)
    -- find matching actions for this line and execute them
    actions <- (gets hcCode >>= filterM matches)
    forM_ actions $ \(Section _ ms) -> exec k $
       case ms of
         Nothing  -> (PRINT [])
         (Just s) -> s

splitIntoFields :: B.ByteString -> Interpreter [B.ByteString]
splitIntoFields str = do
   fs <- liftM toString $ eval (BuiltInVar "FS")
   let fields | fs == " " = B.words str                        -- Handles ' ' and '\t'
              | B.null fs = map B.singleton (B.unpack str)     -- Every character is a field
              | otherwise = let ms = getAllMatches (str =~ fs) -- Regular expression
                                rs = invRegions ms (B.length str)
                            in map (\(s,l) -> B.take l (B.drop s str)) rs
   return fields

-- Checks if the given top-level form matches the current line
matches :: TopLevel -> Interpreter Bool
matches (Function _ _ _)     = return False
matches (Section Nothing  _) = return True
matches (Section (Just p) _) = patternMatches p


-- Checks if the given pattern matches the current line
patternMatches :: Pattern -> Interpreter Bool
patternMatches BEGIN    = return False
patternMatches END      = return False
patternMatches (EXPR e) = liftM coerceToBool $! eval e
patternMatches (RE s)   = gets hcThisLine >>= \l -> return $! l =~ s
patternMatches _        = return False -- Not supported yet

unsup s = fail $ s ++ " are not yet supported"


-- Evaluate an expression, return the result
eval :: Expression -> Interpreter Value

eval (Arith op le re) = do
     l <- liftM coerceToDouble $! eval le
     r <- liftM coerceToDouble $! eval re
     case op of
          "*" -> return $! VDouble (l * r)
          "/" -> return $! VDouble (l / r)
          "+" -> return $! VDouble (l + r)
          "-" -> return $! VDouble (l - r)
          otherwise -> fail $ "Unsupported arith operator " ++ op

eval (Const (LitNumeric i)) = return $! VDouble i
eval (Const (LitStr s))     = return $! VString $ B.pack s
eval (Const (LitRE s))      = return $! VString $ B.pack s

eval (FieldRef e) = do
     i <- liftM coerceToInt $ eval e
     if i == 0
     then gets hcThisLine >>= (return . VString)
     else do fs <- gets hcFields
             return $! fs M.! i

eval (VariableRef s) = do
     -- Variable lookup is special, since we may have an hierarchy
     -- of scopes with its personal variables (in function calls).
     -- So it first we do lookup in stack top, and then in the global
     -- dictionary.
     st <- gets hcStack
     vs <- gets hcVars
     let globalVal = vs *! s
     return $! case st of
       (f:_)     -> M.findWithDefault globalVal s f
       otherwise -> globalVal

eval (BuiltInVar s) = do
     bvs <- gets hcBVars
     return $! bvs *! s

eval (ArrayRef s e) = do
     idx <- liftM toString $! eval e
     ars <- gets hcArrays
     return $! ars *! (s, B.unpack idx)

eval (Incr n f@(FieldRef e))    = incrField n f
eval (Incr n v@(VariableRef s)) = incrVar   n v
eval (Incr n a@(ArrayRef s e))  = incrArr   n a
eval (Decr n f@(FieldRef e))    = decrField n f
eval (Decr n v@(VariableRef s)) = decrVar   n v
eval (Decr n a@(ArrayRef s e))  = decrArr   n a

eval (Relation op le re) = do
     l <- liftM coerceToDouble $! eval le
     r <- liftM coerceToDouble $! eval re
     case op of -- for now, only numeric values
          "==" -> return $! VDouble $ test (l == r)
          "!=" -> return $! VDouble $ test (l /= r)
          ">"  -> return $! VDouble $ test (l >  r)
          ">=" -> return $! VDouble $ test (l >= r)
          "<"  -> return $! VDouble $ test (l <  r)
          "<=" -> return $! VDouble $ test (l <= r)
          otherwise -> fail $ "Unsupported cmp operator " ++ op
  where test b = if b then 1 else 0

eval (Not e) = do
     b <- liftM coerceToBool $! eval e
     return $! VDouble (if b then 0.0 else 1.0)

eval (Neg e) = do
     d <- liftM coerceToDouble $! eval e
     return $! VDouble (- d)

eval (Concat _ _ )  = unsup "Concatenations"

-- In a membership test, array name is parsed as an ordinary variable reference.
-- TODO: Check in grammar
eval (In s (VariableRef arr)) = do
     arrs   <- gets hcArrays
     subscr <- liftM toString $ eval s
     return $! VDouble $ test (M.member (arr,B.unpack subscr) arrs)
  where test b = if b then 1 else 0
eval (In _ _) = fail $ "Incorrect membership test syntax"

eval (Logic op le re) = do
     l <- liftM coerceToBool $! eval le
     r <- liftM coerceToBool $! eval re
     case op of
          "&&" -> return $! VDouble $ test (l && r)
          "||" -> return $! VDouble $ test (l || r)
          otherwise -> fail $ "Unsupported logical operator " ++ op
   where test b = if b then 1 else 0


eval (Match s re) = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 1.0 else 0.0
     return $! VDouble rv

eval (NoMatch s re) = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 0.0 else 1.0
     return $! VDouble rv

eval (FunCall "atan2" [vy, vx]) = do
     y <- liftM coerceToDouble $! eval vy
     x <- liftM coerceToDouble $! eval vx
     return $! VDouble $ atan2 y x

eval (FunCall "cos"  [vx]) = proxyFcn cos vx
eval (FunCall "exp"  [vx]) = proxyFcn exp vx
eval (FunCall "int"  [vx]) = proxyFcn (fromIntegral . truncate) vx
eval (FunCall "log"  [vx]) = proxyFcn log vx
eval (FunCall "sin"  [vx]) = proxyFcn sin vx
eval (FunCall "sqrt" [vx]) = proxyFcn sqrt vx

eval (FunCall "srand" vss) = do
     g <- case vss of
       [vs] -> liftM (mkStdGen . coerceToInt) $! eval vs
       []   -> liftIO getStdGen
     modify $ (\s -> s { hcStdGen = g })
     return $! VDouble 0 -- TODO: srand return value?

eval (FunCall "rand" []) = do
     g <- gets hcStdGen
     let (r, g') = randomR (0.0, 1.0) g
     modify $ (\s -> s { hcStdGen = g' })
     return $! VDouble r

eval (FunCall "index" [vs, vt]) = do
     s <- liftM toString $! eval vs
     t <- liftM toString $! eval vt
     let (x, y) = B.breakSubstring t s
     return $! if B.null y
               then VDouble 0
               else VDouble $ fromIntegral $ 1 + B.length x

eval (FunCall "length" [vs]) = do
     s <- liftM toString $! eval vs
     return $! VDouble $ fromIntegral $ B.length s

eval (FunCall "split" [vs, (VariableRef a)]) = do
     fs <- liftM toString $ eval (BuiltInVar "FS")
     evalSplit vs fs a

eval (FunCall "split" [vs, (VariableRef a), vfs]) = do
     fs <- liftM toString $ eval vfs
     evalSplit vs fs a

eval (FunCall "substr" [vs, vp]) = do
     s <- liftM toString    $ eval vs
     p <- liftM coerceToInt $ eval vp
     return $! VString $ B.drop (p-1) s

eval (FunCall "substr" [vs, vp, vn]) = do
     s <- liftM toString    $ eval vs
     p <- liftM coerceToInt $ eval vp
     n <- liftM coerceToInt $ eval vn
     return $! VString $ B.take n $ B.drop (p-1) s

eval (FunCall "gsub" [vr, vs]) = do
     thisLine <- gets hcThisLine
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     let matches = getAllMatches (thisLine =~ r) :: [(MatchOffset, MatchLength)]
         regions = invRegions matches (B.length thisLine)
         strings = map (\(s,l) -> B.take l (B.drop s thisLine)) regions
         result  = B.intercalate s strings
     modify (\s -> s { hcThisLine = result })
     reconstructThisFields result
     return $! VDouble $ fromIntegral $ length matches

eval (FunCall "gsub" [vr, vs, vt]) = do
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     t <- liftM toString $ eval vt
     let matches = getAllMatches (t =~ r) :: [(MatchOffset, MatchLength)]
         regions = invRegions matches (B.length t)
         strings = map (\(s,l) -> B.take l (B.drop s t)) regions
         result  = VString $ B.intercalate s strings
     case vt of
        (VariableRef s)    -> assignToVar   "=" s   result
        (FieldRef ref)     -> assignToField "=" ref result
        (ArrayRef arr ref) -> assignToArr   "=" arr ref result 
     return $! VDouble $ fromIntegral $ length matches

eval (FunCall "sub" [vr, vs]) = do
     thisLine <- gets hcThisLine
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     case (thisLine =~ r) of
          (-1, _)       -> return $! VDouble 0
          (offset, len) -> do
             let result = B.concat [B.take offset thisLine, s, B.drop (offset+len) thisLine]
             modify (\s -> s { hcThisLine = result })
             reconstructThisFields result
             return $! VDouble 1

eval (FunCall "sub" [vr, vs, vt]) = do
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     t <- liftM toString $ eval vt
     case (t =~ r) of
          (-1, _)       -> return $! VDouble 0
          (offset, len) -> do
             let result = VString $ B.concat [B.take offset t, s, B.drop (offset+len) t]
             case vt of
                (VariableRef s)    -> assignToVar   "=" s   result
                (FieldRef ref)     -> assignToField "=" ref result
                (ArrayRef arr ref) -> assignToArr   "=" arr ref result 
             return $! VDouble 1

eval (FunCall "match" [vs, vr]) = do
     s <- liftM toString $ eval vs
     r <- liftM toString $ eval vr
     let (rStart, rLength) = (s =~ r) :: (MatchOffset, MatchLength)
         retS = VDouble $ fromIntegral $ rStart+1
         retL = VDouble $ fromIntegral $ rLength
     assignToBVar "=" "RSTART"  $ retS
     assignToBVar "=" "RLENGTH" $ retL
     return $! retS

eval (FunCall f args) = do
     mfcn <- liftM (find (func f)) $ gets hcCode
     case mfcn of
       (Just (Function _ argNames stmt)) -> do
           -- Build a stack frame for function call first
           argVals <- mapM eval args
           let numArgs = length argNames
               numVals = length argVals
               numLocs = numArgs - numVals

               boundArgs = zip argNames argVals
               localVars = if numLocs > 0
                           then zip (drop numVals argNames) $ repeat (VDouble 0)
                           else []
               newStackFrame = M.fromList $! boundArgs ++ localVars

           oldStack <- gets hcStack
           modify $ (\s -> s { hcStack = newStackFrame:oldStack, hcRetVal = VDouble 0 })
           callCC $ \ret -> do
              let retHook (Just v) = modify (\s -> s { hcRetVal = v }) >> ret ()
                  retHook Nothing  = ret ()
                  k = seq retHook $ emptyKBlock {kRet = retHook}
              exec k stmt
           modify $ (\s -> s { hcStack = oldStack })
           gets hcRetVal
       Nothing    -> fail $ f ++ " - unknown function"
       otherwise  -> fail $ "Fatal error when invoking function " ++ f
  where
     func s (Function ss _ _) = s == ss
     func s _                 = False

eval (Assignment op p v) = do
     val <- eval v
     case p of
       (FieldRef ref)     -> assignToField op ref  val
       (VariableRef name) -> assignToVar   op name val
       (ArrayRef arr ref) -> assignToArr   op arr ref val
       (BuiltInVar name)  -> assignToBVar  op name val
       otherwise -> fail "Only to-field and to-variable assignments are supported"

proxyFcn :: (Double -> Double) -> Expression -> Interpreter Value
proxyFcn f e = do
     d <- liftM coerceToDouble $ eval e
     return $! VDouble $ f d

hawkSplitWith :: B.ByteString -> B.ByteString -> [B.ByteString]
hawkSplitWith s fs = reverse $! splitWith' s []
  where
    splitWith' str res = case B.breakSubstring fs str of
       (x, y) | B.null y  -> x:res
              | otherwise -> splitWith' (B.drop nfs y) (x:res)
    nfs = B.length fs

evalSplit :: Expression -> B.ByteString -> String -> Interpreter Value
evalSplit vs fs arr = do
   s <- liftM toString $ eval vs
   let ss = s `hawkSplitWith` fs
       is = [1, 2..]
   ars <- gets hcArrays
   let -- at first, clear the array from its previous contents
       -- TODO: very slow, when we have all arrays in a single Data.Map
       ars'  = M.filterWithKey (\(a,_) _ -> a /= arr) ars
       -- Form a new array containing extracted values
       keys  = map (arr,)  $ (map show is)
       strs  = map VString $ ss
       res   = M.fromList $ zip keys strs
       -- Put our new values then
       ars'' = M.union ars' res
   modify $ (\s -> s { hcArrays = ars'' })
   return $! VDouble $ fromIntegral $ length ss

-- Helper function - take a range of matches and invert it (to extract the unmatched data)
invRegions :: [(Int,Int)] -> Int -> [(Int,Int)]
invRegions matches len =  invRegions' matches 0 len []
  where invRegions' [] startVal endVal res = res ++ [(startVal, endVal-startVal)]
        invRegions' ((pStart,pLen):ps) startVal endVal res =
          let thisRegn  = (startVal, pStart-startVal) -- point where valuable data ends
              nextStart = pStart + pLen               -- point where next valuable data starts
          in invRegions' ps nextStart endVal (res ++ [thisRegn])

calcNewValue oldVal op arg =
     case op of
        "="  -> arg
        "+=" -> VDouble $! coerceToDouble oldVal + coerceToDouble arg
        "-=" -> VDouble $! coerceToDouble oldVal - coerceToDouble arg
        "*=" -> VDouble $! coerceToDouble oldVal * coerceToDouble arg
        "/=" -> VDouble $! coerceToDouble oldVal / coerceToDouble arg
        otherwise -> undefined

assignToField op ref val = do
     i <- liftM coerceToInt $! eval ref
     oldFields <- gets hcFields
     let newValue  = calcNewValue (oldFields *! i) op val
         newFields = M.insert i newValue oldFields
     modify (\s -> s { hcFields = newFields })
     reconstructThisLine
     return $! newValue

reconstructThisLine = do
     thisFields <- gets (M.toList . hcFields)
     ofs        <- liftM toString $ eval (BuiltInVar "OFS")
     let line = B.intercalate ofs $ map (toString . snd) thisFields
     modify (\s -> s { hcThisLine = line })
     return ()

reconstructThisFields l = do
    oldContext <- get
    let thisFields = map VString $ B.words l
        thisFldMap = M.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcFields   = thisFldMap }
    put $! thisContext

assignToVar op name val = do
     -- As the lookup, variable assignment is also special.
     -- At first, we try to update variable in the current scope (if any),
     -- then we refer to the global scope.
     oldVars  <- gets hcVars
     oldStack <- gets hcStack
     case oldStack of
       (f:_) -> case name `M.lookup` f of
                  Nothing  -> updGlobal oldVars
                  (Just v) -> updStack oldStack
       []    -> updGlobal oldVars
  where
     updGlobal oldVars = do
       let newValue = calcNewValue (oldVars *! name) op val
           newVars  = M.insert name newValue oldVars
       modify (\s -> s { hcVars = newVars })
       return $! newValue

     updStack (f:fs) = do
       let newValue = calcNewValue (f *! name) op val
           newVars  = M.insert name newValue f
       modify (\s -> s { hcStack = newVars:fs })
       return $! newValue

-- Currently for internal use only
assignToBVar op name val = do
   oldBVars <- gets hcBVars
   let newValue = calcNewValue (oldBVars *! name) op val
       newBVars = M.insert name newValue oldBVars
   modify $ (\s -> s { hcBVars = newBVars })
   return $! newValue

assignToArr op arr ref val = do
   oldArrs <- gets hcArrays
   subscr  <- liftM toString $ eval ref
   let index     = (arr, B.unpack subscr)
       newValue  = calcNewValue (oldArrs *! index) op val
       newArrays = M.insert index newValue oldArrs
   modify $ (\s -> s { hcArrays = newArrays })
   return $! newValue

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

incrArr n arr@(ArrayRef name ref) = do
   oldVal <- eval arr
   newVal <- assignToArr "+=" name ref (VDouble 1.0)
   return (if n == Post then oldVal else newVal)

decrArr n arr@(ArrayRef name ref) = do
   oldVal <- eval arr
   newVal <- assignToArr "-=" name ref (VDouble 1.0)
   return (if n == Post then oldVal else newVal)

-- Coercions and conversions
coerceToBool :: Value -> Bool
coerceToBool (VString "") = False
coerceToBool (VDouble 0)  = False
coerceToBool _            = True

coerceToDouble :: Value -> Double
coerceToDouble (VDouble d)  = d
coerceToDouble (VString "") = 0.0
coerceToDouble (VString s)  = read $ B.unpack s

coerceToInt :: Value -> Int
coerceToInt (VDouble d)  = truncate d
coerceToInt (VString "") = 0
coerceToInt (VString s)  = read $ B.unpack s

toString :: Value -> B.ByteString
toString (VString s) = s
toString (VDouble d) =
    let s | rs == [0] && p == 0 = "0"
          | p >= 0 && nrs <= p  = sgn ++ (concat $ map show rs) ++ take (p-nrs) (repeat '0')
          | otherwise           = show d
    in B.pack s
  where (rs,p) = floatToDigits 10 (abs d)
        nrs = length rs
        sgn = if d >= 0 then "" else "-"

data KBlock = KBlock { kNext  :: !(() -> Interpreter ())
                     , kExit  :: !(() -> Interpreter ())
                     , kRet   :: !(Maybe Value -> Interpreter ())
                     , kCont  :: !(() -> Interpreter ())
                     , kBreak :: !(() -> Interpreter ())
                     }

emptyKBlock :: KBlock
emptyKBlock = KBlock { kNext  = return
                     , kExit  = return
                     , kRet   = \_ -> return ()
                     , kCont  = return
                     , kBreak = return
                     }

-- Execute a statement
exec :: KBlock -> Statement -> Interpreter ()
exec _ (Expression e) = eval e >> return ()
exec k (Block es)     = mapM_ (exec k) es

exec k (IF c t me)    = do
     b <- liftM coerceToBool $! eval c
     if b
     then exec k t
     else case me of
          Nothing -> return ()
          Just es -> exec k es

exec k w@(WHILE c s) = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextWhile k'}
         nextWhile kk = do
            b <- liftM coerceToBool $! eval c
            when b $ (exec kk s >> nextWhile kk)
            br ()
     nextWhile k'

exec k (FOR i c st s) = callCC $ \br -> do
    let k' = k {kBreak = br, kCont = \_ -> nextFor k'}
        -- TODO: optional expressoins
        initFor    = eval (fromJust i)
        nextFor kk = eval (fromJust st) >> execFor kk
        execFor kk = do
           b <- liftM coerceToBool $! eval (fromJust c)
           when b $ exec kk s >> nextFor kk
           br ()
    initFor
    execFor k'

exec k d@(DO s c) = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextDo k'}
         nextDo kk = do
            exec kk s
            b <- liftM coerceToBool $! eval c
            when b $ nextDo kk
            br ()
     nextDo k'

-- TODO: The order in which the keys will be traversed may be suprising
exec k f@(FOREACH v@(VariableRef vname) arr st) = do
     arrData <- liftM (filter inArray . map fst . M.toList) $ gets hcArrays
     callCC $ \br -> do
       let k' = k {kBreak = br}
           nextFor kk []         = br ()
           nextFor kk ((_,s):ss) = do
             assignToVar "=" vname (VString $ B.pack s)
             let kk' = kk {kCont = \_ -> nextFor kk' (tail ss)}
             seq kk' $ exec kk' st
             nextFor kk' ss
       nextFor k' arrData
       br ()
   where inArray (a, _) = a == arr

exec _ (PRINT es) = do
   ofs <- liftM toString $! eval (BuiltInVar "OFS")
   ors <- liftM toString $! eval (BuiltInVar "ORS")
   str <- case es of
      []        -> gets hcThisLine
      otherwise -> liftM (B.intercalate ofs . map toString) $ mapM eval es
   liftIO $ B.putStr $ B.append str ors

exec k (BREAK)     = (kBreak k) ()
exec k (CONT)      = (kCont  k) ()
exec k (NEXT)      = (kNext  k) ()
exec k (EXIT _)    = (kExit  k) () -- TODO argument
exec k (RETURN me) = case me of
      Nothing   -> (kRet k) Nothing
      Just expr -> eval expr >>= (kRet k . Just)
exec _ (NOP)       = return ()

exec _ (DELETE e) = case e of
    (ArrayRef arr idx) -> do
       oldArrs <- gets hcArrays
       subscr  <- liftM toString $ eval idx
       let index = (arr, B.unpack subscr)
       modify $ \s -> s {hcArrays = M.delete index oldArrs}
       return ()
    otherwise -> fail $ "Syntax error: delete element"
