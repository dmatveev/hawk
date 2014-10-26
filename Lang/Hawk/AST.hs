module Lang.Hawk.AST where

type Op = String

data Literal = LitNumeric Double
             | LitStr String
             | LitRE String
               deriving (Eq, Show)

data Notation = Pre | Post
                deriving (Eq, Show)

data Expression = Arith Op Expression Expression
                | Const Literal
                | FieldRef Expression
                | VariableRef String
                | ArrayRef String Expression
                | BuiltInVar String
                | Assignment Op Expression Expression
                | Incr Notation Expression
                | Decr Notation Expression
                | Relation Op Expression Expression
                | Not Expression
                | Neg Expression
                | Concat Expression Expression
                | In Expression Expression
                | Logic Op Expression Expression
                | Match Expression Expression
                | NoMatch Expression Expression
                | FunCall String [Expression]
                  deriving (Eq, Show)

data Statement = Expression Expression
               | Block [Statement]
               | IF Expression Statement (Maybe Statement)
               | WHILE Expression Statement
               | FOR (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | FOREACH Expression String Statement
               | DO Statement Expression
               | PRINT [Expression]
               | BREAK
               | CONT
               | NEXT
               | EXIT (Maybe Expression)
               | NOP
               | DELETE Expression
               | RETURN (Maybe Expression)
                 deriving (Eq, Show)

data Pattern = BEGIN
             | END
             | EXPR Expression
             | RE String
             | RANGE Pattern Pattern
               deriving (Eq, Show)

data TopLevel = Section (Maybe Pattern) (Maybe Statement)
              | Function String [String] Statement
                deriving (Eq, Show)

type AwkSource = [TopLevel]


vars :: AwkSource -> [String]
vars ts = concat $ map vars' ts
    where vars' (Section mp ms)  = maybe [] varsFromPattern mp ++ maybe [] varsFromStmt ms
          vars' (Function _ _ s) = varsFromStmt s

visitPattern :: (Pattern -> [a]) -> Pattern -> [a]
visitPattern f p = f p ++ case p of
    (RANGE p1 p2) -> f p1 ++ f p2
    otherwise     -> []

visitPatternExpr :: (Expression -> [a]) -> Pattern -> [a]
visitPatternExpr f = visitPattern f'
  where f' (EXPR e) = f e
        f' _        = []

visitStmt :: (Statement -> [a]) -> Statement -> [a]
visitStmt f s = f s ++ case s of
    (Block ss)       -> concat $ map f ss
    (IF c t me)      -> f t ++ maybe [] f me
    (WHILE e s)      -> f s
    (FOR mi mc ms s) -> f s
    (FOREACH e a s)  -> f s
    (DO s e)         -> f s
    otherwise        -> []

visitStmtExpr :: (Expression -> [a]) -> Statement -> [a]
visitStmtExpr f = visitStmt f'
  where f' (Expression e)   = f e
        f' (IF c _ _)       = f c
        f' (WHILE e _)      = f e
        f' (FOR mi mc ms _) = concat $ map (maybe [] f) [mi, mc, ms]
        f' (FOREACH e _ _)  = f e
        f' (DO _ e)         = f e
        f' (PRINT es)       = concat $ map f es
        f' (EXIT me)        = maybe [] f me
        f' (DELETE e)       = f e
        f' (RETURN me)      = maybe [] f me
        f' _                = []

visitExpr :: (Expression -> [a]) -> Expression -> [a]
visitExpr f e = f e ++ case e of
   (Arith _ l r)      -> f l ++ f r
   (Assignment _ l r) -> f l ++ f r
   (Incr _ ex)        -> f ex
   (Decr _ ex)        -> f ex
   (Relation _ l r)   -> f l ++ f r
   (FunCall _ es)     -> concat $ map f es
   (Not ex)           -> f ex
   (Neg ex)           -> f ex
   (Concat l r)       -> f l ++ f r
   (In ex _)          -> f ex
   (Logic _ l r)      -> f l ++ f r
   (Match l r)        -> f l ++ f r
   (NoMatch l r)      -> f l ++ f r
   otherwise          -> []


varsFromPattern :: Pattern -> [String]
varsFromPattern = visitPatternExpr varsFromExpr

varsFromStmt :: Statement -> [String]
varsFromStmt = visitStmtExpr varsFromExpr

varsFromExpr :: Expression -> [String]
varsFromExpr = visitExpr f
   where f (VariableRef s) = [s]
         f _               = []
