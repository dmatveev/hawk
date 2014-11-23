{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings #-}

module Lang.Hawk.Interpreter where

import qualified Data.ByteString.Char8 as B

import Text.Regex.PCRE

import Data.Fixed (mod')

import Data.List (find, intercalate)
import Data.Maybe (fromJust)

import Control.Monad.State.Strict
import Control.Monad.Cont
import Control.Monad.Trans
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import System.Random
import System.IO

import Lang.Hawk.AST
import Lang.Hawk.Value
import Lang.Hawk.Utils

data HawkContext = HawkContext
                 { hcCode     :: !AwkSource
                 , hcFields   :: (IM.IntMap Value)
                 , hcVars     :: !(M.Map String Value)
                 , hcArrays   :: !(M.Map (String, String) Value)
                 , hcBVars    :: !(M.Map BVar Value)
                 , hcStack    :: ![M.Map String Value]
                 , hcRetVal   :: !Value
                 , hcThisLine :: B.ByteString
                 , hcStdGen   :: StdGen
                 }

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

(*!) :: Ord k => M.Map k Value -> k -> Value
m *! k = M.findWithDefault (VDouble 0) k m

(*!!) :: IM.IntMap Value -> Int -> Value
m *!! k = IM.findWithDefault (VDouble 0) k m


emptyContext :: AwkSource -> HawkContext
emptyContext s = HawkContext
                 { hcCode     = s
                 , hcFields   = IM.empty
                 , hcVars     = M.empty
                 , hcArrays   = M.empty
                 , hcBVars    = M.fromList initialBuiltInVars
                 , hcStack    = []
                 , hcRetVal   = VDouble 0
                 , hcThisLine = ""
                 , hcStdGen   = mkStdGen 0
                 }
  where initialBuiltInVars = [ (FNR, VDouble 0)
                             , (NR,  VDouble 0)
                             , (NF,  VDouble 0)
                             , (OFS, defstr " ")
                             , (FS,  defstr " ")
                             , (ORS, defstr "\n")
                             , (RS,  defstr "\n")
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
        (Just s) -> {-# SCC "execFIN" #-} exec emptyKBlock s
  where isEnd (Section (Just END) _) = return True
        isEnd _                      = return False

-- This is actually an entry point to the Interpreter.
-- intMain :: Handle -> String -> Interpreter ()
-- intMain h inputFile = do
--     assignToBVar ModSet FILENAME (valstr $ B.pack inputFile)
--     assignToBVar ModSet FNR      (VDouble 0)
--     initialize
--     callCC $ \ex -> do
--        let k = emptyKBlock {kExit = ex}
--        readLoop k B.empty False
--        ex ()
--     finalize
--   where
--     readLoop k thisBuf eof = do
--       rs <- liftM toString $ eval (BuiltInVar RS)
--       let nrs = B.length rs
--       case B.breakSubstring rs thisBuf of
--          (l, rest) | B.null l && B.null rest && eof ->
--                         return ()
--                    | B.null rest && eof -> do
--                         let k' = k { kNext = (const $ return ()) }
--                         seq k' $ processLine k' l
--                    | B.null rest && not eof -> do
--                         nextChunk <- liftIO $ B.hGet h 8192
--                         readLoop k (B.append thisBuf nextChunk) (B.null nextChunk)  
--                    | otherwise -> do
--                         let r' = B.drop nrs rest
--                             k' = k { kNext = \_ -> readLoop k' r' eof } 
--                         seq k' $ processLine k' l
--                         readLoop k' r' eof


-- processLine takes a new (next) line from input stream, prepares
-- the execution context, and evaluates the awk code with this context.
processLine :: KBlock -> B.ByteString -> Interpreter ()
processLine k s = do
    oldContext <- get
    thisFields <- liftM (map valstr) $ splitIntoFields s
    let thisFldMap = IM.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcThisLine = s, hcFields = thisFldMap }
    put $! thisContext
    assignToBVar ModSet NF  (VDouble $ fromIntegral $ length thisFields)
    assignToBVar ModAdd NR  (VDouble 1)
    assignToBVar ModAdd FNR (VDouble 1)
    -- find matching actions for this line and execute them
    actions <- (gets hcCode >>= filterM matches)
    forM_ actions $ \(Section _ ms) -> exec k $
       case ms of
         Nothing  -> (PRINT [])
         (Just s) -> s

splitIntoFields' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitIntoFields' fs str
    | fs == " " = B.words str                        -- Handles ' ' and '\t'
    | B.null fs = map B.singleton (B.unpack str)     -- Every character is a field
    | otherwise = let ms = getAllMatches (str =~ fs) -- Regular expression
                      rs = invRegions ms (B.length str)
                  in map (\(s,l) -> B.take l (B.drop s str)) rs

splitIntoFields :: B.ByteString -> Interpreter [B.ByteString]
splitIntoFields str = do
   fs <- liftM toString $ eval (BuiltInVar FS)
   return $ splitIntoFields' fs str

-- Checks if the given top-level form matches the current line
matches :: TopLevel -> Interpreter Bool
matches (Function _ _ _)     = return False
matches (Section Nothing  _) = return True
matches (Section (Just p) _) = patternMatches p


-- Checks if the given pattern matches the current line
patternMatches :: Pattern -> Interpreter Bool
patternMatches BEGIN    = return False
patternMatches END      = return False
patternMatches (EXPR e) = liftM toBool $! eval e
patternMatches (RE s)   = gets hcThisLine >>= \l -> return $! l =~ s
patternMatches _        = return False -- Not supported yet

unsup s = fail $ s ++ " are not yet supported"


-- Evaluate an expression, return the result
eval :: Expression -> Interpreter Value
eval (Arith op le re)                = evalArith op le re
eval (Const (LitNumeric i))          = return $! VDouble i
eval (Const (LitStr     s))          = return $! valstr $ B.pack s
eval (Const (LitRE      s))          = return $! valstr $ B.pack s
eval (Id          e)                 = eval e
eval (FieldRef    e)                 = evalFieldRef e
eval (VariableRef s)                 = evalVariableRef s
eval (BuiltInVar  s)                 = evalBVariableRef s 
eval (ArrayRef    s e)               = evalArrRef s e
eval (Incr n f@(FieldRef      e))    = incrField n f
eval (Incr n v@(VariableRef s  ))    = incrVar n v
eval (Incr n a@(ArrayRef    s e))    = incrArr n a
eval (Decr n f@(FieldRef      e))    = decrField n f
eval (Decr n v@(VariableRef s  ))    = decrVar n v
eval (Decr n a@(ArrayRef    s e))    = decrArr n a
eval (Relation op le re)             = evalCmp op le re
eval (Not e)                         = evalNot e 
eval (Neg e)                         = evalNeg e
eval (Concat _ _ )                   = unsup "Concatenations"
eval (In s (VariableRef arr))        = evalArrTest s arr
eval (In _ _)                        = fail $ "Incorrect membership test syntax"
eval (Logic op le re)                = evalLogic op le re
eval (Match s re)                    = evalMatch s re
eval (NoMatch s re)                  = evalNoMatch s re
eval (FunCall "atan2"  [vy, vx])     = evalAtan2 vy vx
eval (FunCall "cos"    [vx])         = proxyFcn cos vx
eval (FunCall "exp"    [vx])         = proxyFcn exp vx
eval (FunCall "int"    [vx])         = proxyFcn (fromIntegral . truncate) vx
eval (FunCall "log"    [vx])         = proxyFcn log vx
eval (FunCall "sin"    [vx])         = proxyFcn sin vx
eval (FunCall "sqrt"   [vx])         = proxyFcn sqrt vx
eval (FunCall "srand"  vss)          = evalSRand vss
eval (FunCall "rand"   [])           = evalRand
eval (FunCall "index"  [vs, vt])     = evalIndex vs vt
eval (FunCall "length" [vs])         = evalLength vs
eval (FunCall "split"  [vs, (VariableRef a)])      = evalSplitFS vs a
eval (FunCall "split"  [vs, (VariableRef a), vfs]) = evalSplitVar vs a vfs
eval (FunCall "substr" [vs, vp])     = evalSubstr vs vp
eval (FunCall "substr" [vs, vp, vn]) = evalSubstr2 vs vp vn
eval (FunCall "gsub"   [vr, vs])     = evalGSub vr vs
eval (FunCall "gsub"   [vr, vs, vt]) = evalGSubVar vr vs vt
eval (FunCall "sub"    [vr, vs])     = evalSub vr vs
eval (FunCall "sub"    [vr, vs, vt]) = evalSubVar vr vs vt
eval (FunCall "match"  [vs, vr])     = evalFMatch vs vr
eval (FunCall f args)                = evalFunCall f args
eval (Assignment op p v)             = evalAssign op p v

proxyFcn :: (Double -> Double) -> Expression -> Interpreter Value
proxyFcn f e = do
     d <- liftM toDouble $ eval e
     return $! VDouble $ f d


assignToField op ref val = do
     i <- liftM toInt $! eval ref
     if i == 0
     then do
          thisLine <- gets hcThisLine
          let newLine    = calcNewValue (valstr thisLine) op val
              newLineStr = toString newLine
          modify (\s -> s {hcThisLine = newLineStr})
          reconstructThisFields newLineStr
          return $! newLine
     else do
          oldFields <- gets hcFields
          let newValue  = calcNewValue (oldFields *!! i) op val
              newFields = IM.insert i newValue oldFields
          modify (\s -> s { hcFields = newFields })
          reconstructThisLine
          return $! newValue

reconstructThisLine = do
     thisFields <- gets (IM.toList . hcFields)
     ofs        <- liftM toString $ eval (BuiltInVar OFS)
     let line = B.intercalate ofs $ map (toString . snd) thisFields
     modify (\s -> s { hcThisLine = line })
     return ()

reconstructThisFields l = do
    oldContext <- get
    thisFields <- liftM (map valstr) $ splitIntoFields l
    let thisFldMap = IM.fromList (zip [1,2..] thisFields)
        thisContext = oldContext { hcFields = thisFldMap }
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


ppval :: Double -> Double -> Notation -> Value
ppval old new Post = VDouble old
ppval old new Pre  = VDouble new

incrField n fld@(FieldRef e) = do
   (VDouble d) <- assignToField ModAdd e (VDouble 1.0)
   return $! ppval (d-1) d n

decrField n fld@(FieldRef e) = do
   (VDouble d) <- assignToField ModSub e (VDouble 1.0)
   return $! ppval (d+1) d n

incrVar n var@(VariableRef s) = do
   (VDouble d) <- assignToVar ModAdd s (VDouble 1.0)
   return $! ppval (d-1) d n

decrVar n var@(VariableRef s) = do
   (VDouble d) <- assignToVar ModSub s (VDouble 1.0)
   return $! ppval (d+1) d n

incrArr n arr@(ArrayRef name ref) = do
   (VDouble d) <- assignToArr ModAdd name ref (VDouble 1.0)
   return $! ppval (d-1) d n

decrArr n arr@(ArrayRef name ref) = do
   (VDouble d) <- assignToArr ModSub name ref (VDouble 1.0)
   return $! ppval (d+1) d n

-- Execute a statement
exec :: KBlock -> Statement -> Interpreter ()
exec _ (Expression e) = {-# SCC "execEXPR"  #-} eval e >> return ()
exec k (Block es)     = {-# SCC "execBLOCK" #-} mapM_ (exec k) es
exec k (IF c t me)    = {-# SCC "execIF"    #-} execIF k c t me
exec k w@(WHILE c s)  = {-# SCC "execWHILE" #-} execWHILE k w c s 
exec k (FOR i c st s) = {-# SCC "execFOR"   #-} execFOR k i c st s
exec k d@(DO s c)     = {-# SCC "execDO"    #-} execDO k d s c 
exec k f@(FOREACH v@(VariableRef vname) arr st) = {-# SCC "execFE" #-} execFOREACH k f v vname arr st
exec _ (PRINT es)     = {-# SCC "execPRINT" #-} execPRINT es
exec k (BREAK)        = {-# SCC "execBREAK" #-} (kBreak k) ()
exec k (CONT)         = {-# SCC "execCONT"  #-} (kCont  k) ()
exec k (NEXT)         = {-# SCC "execNEXT"  #-} (kNext  k) ()
exec k (EXIT _)       = {-# SCC "execEXIT"  #-} (kExit  k) () -- TODO argument
exec k (RETURN me)    = {-# SCC "execRET"   #-} execRET k me
exec _ (NOP)          = {-# SCC "execNOP"   #-} return ()
exec _ (DELETE e)     = {-# SCC "execDEL"   #-} execDEL e


evalArith op le re = do
     l <- liftM toDouble $! eval le
     r <- liftM toDouble $! eval re
     case op of
          Mul -> return $! VDouble (l * r)
          Div -> return $! VDouble (l / r)
          Add -> return $! VDouble (l + r)
          Sub -> return $! VDouble (l - r)
          Mod -> return $! VDouble (mod' l r)
          Pow -> return $! VDouble (l ** r)

evalFieldRef e = do
     i <- liftM toInt $ eval e
     if i == 0
     then gets hcThisLine >>= (return . valstr)
     else do fs <- gets hcFields
             return $! fs IM.! i

evalVariableRef s = do
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

evalBVariableRef s = do
     bvs <- gets hcBVars
     return $! bvs *! s

evalArrRef s e = do
     idx <- liftM toString $! eval e
     ars <- gets hcArrays
     return $! ars *! (s, B.unpack idx)

evalCmp op le re = do
     l <- eval le
     r <- eval re
     return $! VDouble $ test $ case (l, r) of
         (VString lStr lNum sParsed, VString rStr rNum rParsed) ->
            -- If the both strings represent numbers completely
           if sParsed && rParsed then cmp op lNum rNum else cmp op lStr rStr
         (VString _ lNum _, VDouble   rNum  ) -> cmp op lNum rNum
         (VDouble   lNum  , VString _ rNum _) -> cmp op lNum rNum
         (VDouble   lNum  , VDouble   rNum  ) -> cmp op lNum rNum
  where
    cmp op l r = case op of
       CmpEQ -> l == r
       CmpNE -> l /= r
       CmpGT -> l >  r
       CmpGE -> l >= r
       CmpLT -> l <  r
       CmpLE -> l <= r
    test b = if b then 1 else 0

evalNot e = do
     b <- liftM toBool $! eval e
     return $! VDouble (if b then 0.0 else 1.0)

evalNeg e = do
     d <- liftM toDouble $! eval e
     return $! VDouble (- d)


-- In a membership test, array name is parsed as an ordinary variable reference.
-- TODO: Check in grammar
evalArrTest s arr = do
     arrs   <- gets hcArrays
     subscr <- liftM toString $ eval s
     return $! VDouble $ test (M.member (arr,B.unpack subscr) arrs)
  where test b = if b then 1 else 0

evalLogic op le re = do
     l <- liftM toBool $! eval le
     r <- liftM toBool $! eval re
     case op of
          AND -> return $! VDouble $ test (l && r)
          OR  -> return $! VDouble $ test (l || r)
   where test b = if b then 1 else 0

evalMatch s re = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 1.0 else 0.0
     return $! VDouble rv

evalNoMatch s re = do
     l <- liftM toString $! eval s
     r <- liftM toString $! eval re
     let rv = if r /= "" && l =~ r then 0.0 else 1.0
     return $! VDouble rv
     
evalAtan2 vx vy = do
     y <- liftM toDouble $! eval vy
     x <- liftM toDouble $! eval vx
     return $! VDouble $ atan2 y x

evalSRand vss = do
     g <- case vss of
       [vs] -> liftM (mkStdGen . toInt) $! eval vs
       []   -> liftIO getStdGen
     modify $ (\s -> s { hcStdGen = g })
     return $! VDouble 0 -- TODO: srand return value?

evalRand = do
     g <- gets hcStdGen
     let (r, g') = randomR (0.0, 1.0) g
     modify $ (\s -> s { hcStdGen = g' })
     return $! VDouble r

evalIndex vs vt = do
     s <- liftM toString $! eval vs
     t <- liftM toString $! eval vt
     let (x, y) = B.breakSubstring t s
     return $! if B.null y
               then VDouble 0
               else VDouble $ fromIntegral $ 1 + B.length x

evalLength vs = do
     s <- liftM toString $! eval vs
     return $! VDouble $ fromIntegral $ B.length s

evalSplitFS vs a = do
     fs <- liftM toString $ eval (BuiltInVar FS)
     evalSplit vs fs a

evalSplitVar vs a vfs = do
     fs <- liftM toString $ eval vfs
     evalSplit vs fs a

evalSplit :: Expression -> B.ByteString -> String -> Interpreter Value
evalSplit vs fs arr = do
   s <- liftM toString $ eval vs
   let ss = s `splitWithSep` fs
       is = [1, 2..]
   ars <- gets hcArrays
   let -- at first, clear the array from its previous contents
       -- TODO: very slow, when we have all arrays in a single Data.Map
       ars'  = M.filterWithKey (\(a,_) _ -> a /= arr) ars
       -- Form a new array containing extracted values
       keys  = map (arr,)  $ (map show is)
       strs  = map valstr  $ ss
       res   = M.fromList $ zip keys strs
       -- Put our new values then
       ars'' = M.union ars' res
   modify $ (\s -> s { hcArrays = ars'' })
   return $! VDouble $ fromIntegral $ length ss

evalSubstr vs vp = do
     s <- liftM toString    $ eval vs
     p <- liftM toInt $ eval vp
     return $! valstr $ B.drop (p-1) s

evalSubstr2 vs vp vn = do
     s <- liftM toString    $ eval vs
     p <- liftM toInt $ eval vp
     n <- liftM toInt $ eval vn
     return $! valstr $ B.take n $ B.drop (p-1) s

evalGSub vr vs = do
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

evalGSubVar vr vs vt = do
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     t <- liftM toString $ eval vt
     let matches = getAllMatches (t =~ r) :: [(MatchOffset, MatchLength)]
         regions = invRegions matches (B.length t)
         strings = map (\(s,l) -> B.take l (B.drop s t)) regions
         result  = valstr $ B.intercalate s strings
     case vt of
        (VariableRef s)    -> assignToVar   ModSet s   result
        (FieldRef ref)     -> assignToField ModSet ref result
        (ArrayRef arr ref) -> assignToArr   ModSet arr ref result 
     return $! VDouble $ fromIntegral $ length matches

evalSub vr vs = do
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

evalSubVar vr vs vt = do
     r <- liftM toString $ eval vr
     s <- liftM toString $ eval vs
     t <- liftM toString $ eval vt
     case (t =~ r) of
          (-1, _)       -> return $! VDouble 0
          (offset, len) -> do
             let result = valstr $ B.concat [B.take offset t, s, B.drop (offset+len) t]
             case vt of
                (VariableRef s)    -> assignToVar   ModSet s   result
                (FieldRef ref)     -> assignToField ModSet ref result
                (ArrayRef arr ref) -> assignToArr   ModSet arr ref result 
             return $! VDouble 1

evalFMatch vs vr = do
     s <- liftM toString $ eval vs
     r <- liftM toString $ eval vr
     let (rStart, rLength) = (s =~ r) :: (MatchOffset, MatchLength)
         retS = VDouble $ fromIntegral $ rStart+1
         retL = VDouble $ fromIntegral $ rLength
     assignToBVar ModSet RSTART  $ retS
     assignToBVar ModSet RLENGTH $ retL
     return $! retS

evalFunCall f args = do
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

evalAssign op p v = do
     val <- eval v
     case p of
       (FieldRef ref)     -> assignToField op ref  val
       (VariableRef name) -> assignToVar   op name val
       (ArrayRef arr ref) -> assignToArr   op arr ref val
       (BuiltInVar name)  -> assignToBVar  op name val
       otherwise -> fail "Only to-field and to-variable assignments are supported"


execIF k c t me = do
     b <- liftM toBool $! eval c
     if b
     then exec k t
     else case me of
          Nothing -> return ()
          Just es -> exec k es

execWHILE k w c s = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextWhile k'}
         nextWhile kk = do
            b <- liftM toBool $! eval c
            when b $ (exec kk s >> nextWhile kk)
            br ()
     nextWhile k'

execFOR k i c st s = callCC $ \br -> do
    let k' = k {kBreak = br, kCont = \_ -> nextFor k'}
        -- TODO: optional expressoins
        initFor    = eval (fromJust i)
        nextFor kk = eval (fromJust st) >> execFor kk
        execFor kk = do
           b <- liftM toBool $! eval (fromJust c)
           when b $ exec kk s >> nextFor kk
           br ()
    initFor
    execFor k'

execDO k d s c = callCC $ \br -> do
     let k' = k {kBreak = br, kCont = \_ -> nextDo k'}
         nextDo kk = do
            exec kk s
            b <- liftM toBool $! eval c
            when b $ nextDo kk
            br ()
     nextDo k'

-- TODO: The order in which the keys will be traversed may be suprising
execFOREACH k f v vname arr st = do
     arrData <- liftM (filter inArray . map fst . M.toList) $ gets hcArrays
     callCC $ \br -> do
       let k' = k {kBreak = br}
           nextFor kk []         = br ()
           nextFor kk ((_,s):ss) = do
             assignToVar ModSet vname (valstr $ B.pack s)
             let kk' = kk {kCont = \_ -> nextFor kk' (tail ss)}
             seq kk' $ exec kk' st
             nextFor kk' ss
       nextFor k' arrData
       br ()
   where inArray (a, _) = a == arr

execPRINT es = do
   ofs <- liftM toString $! eval (BuiltInVar OFS)
   ors <- liftM toString $! eval (BuiltInVar ORS)
   str <- case es of
      []        -> gets hcThisLine
      otherwise -> liftM (B.intercalate ofs . map toString) $ mapM eval es
   liftIO $ B.putStr $ B.append str ors

execRET k me = case me of
      Nothing   -> (kRet k) Nothing
      Just expr -> eval expr >>= (kRet k . Just)

execDEL e = case e of
    (ArrayRef arr idx) -> do
       oldArrs <- gets hcArrays
       subscr  <- liftM toString $ eval idx
       let index = (arr, B.unpack subscr)
       modify $ \s -> s {hcArrays = M.delete index oldArrs}
       return ()
    otherwise -> fail $ "Syntax error: delete element"
