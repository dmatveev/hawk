module Lang.Hawk.AST where

type Op = String

data Literal = LitNumeric Integer
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
                | FunCall String [Expression]
                | Not Expression
                | Neg Expression
                | Concat Expression Expression
                | In Expression Expression
                | Logic Op Expression Expression
                | Match Expression Expression
                | NoMatch Expression Expression
                  deriving (Eq, Show)

data Statement = Expression Expression
               | Block [Statement]
               | IF Expression Statement (Maybe Statement)
               | WHILE Expression Statement
               | FOR (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
               | FOREACH Expression Expression Statement
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

varsFromPattern :: Pattern -> [String]
varsFromPattern BEGIN           = []
varsFromPattern END             = []
varsFromPattern (EXPR e)        = varsFromExpr e
varsFromPattern (RE _)          = []
varsFromPattern (RANGE p1 p2)   = varsFromPattern p1 ++ varsFromPattern p2

varsFromStmt :: Statement -> [String]
varsFromStmt (Expression e)     = varsFromExpr e
varsFromStmt (Block ss)         = concat $ map varsFromStmt ss
varsFromStmt (IF c t me)        = varsFromExpr c ++ varsFromStmt t ++ maybe [] varsFromStmt me
varsFromStmt (WHILE e s)        = varsFromExpr e ++ varsFromStmt s
varsFromStmt (FOR mi mc ms s)   = varsFromStmt s
                                  ++ (concat $ map (maybe [] varsFromExpr) [mi, mc, ms])
varsFromStmt (FOREACH e a s)    = varsFromExpr e ++ varsFromExpr a ++ varsFromStmt s
varsFromStmt (DO s e)           = varsFromStmt s ++ varsFromExpr e
varsFromStmt (PRINT es)         = concat $ map varsFromExpr es
varsFromStmt (EXIT me)          = maybe [] varsFromExpr me
varsFromStmt (DELETE e)         = varsFromExpr e
varsFromStmt (RETURN me)        = maybe [] varsFromExpr me
varsFromStmt BREAK              = []
varsFromStmt CONT               = []
varsFromStmt NEXT               = []
varsFromStmt NOP                = []

varsFromExpr :: Expression -> [String]
varsFromExpr (Arith _ l r)      = varsFromExpr l ++ varsFromExpr r
varsFromExpr (Const _)          = []
varsFromExpr (FieldRef _)       = []
varsFromExpr (VariableRef s)    = [s]
varsFromExpr (BuiltInVar _)     = []
varsFromExpr (Assignment _ l r) = varsFromExpr l ++ varsFromExpr r
varsFromExpr (Incr _ e)         = varsFromExpr e
varsFromExpr (Decr _ e)         = varsFromExpr e
varsFromExpr (Relation _ l r)   = varsFromExpr l ++ varsFromExpr r
varsFromExpr (FunCall _ es)     = concat $ map varsFromExpr es
varsFromExpr (Not e)            = varsFromExpr e
varsFromExpr (Neg e)            = varsFromExpr e
varsFromExpr (Concat l r)       = varsFromExpr l ++ varsFromExpr r
varsFromExpr (In l r)           = varsFromExpr l ++ varsFromExpr r
varsFromExpr (Logic _ l r)      = varsFromExpr l ++ varsFromExpr r
varsFromExpr (Match l r)        = varsFromExpr l ++ varsFromExpr r
varsFromExpr (NoMatch l r)      = varsFromExpr l ++ varsFromExpr r
