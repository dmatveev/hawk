module Lang.Hawk.Grammar where

import Data.Maybe
import Control.Monad (when)

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

import Lang.Hawk.AST

hawkBuiltinVars = ["FS"]

-- Lexer
lexer = P.makeTokenParser
        ( emptyDef
        { P.commentLine     = "#" 
        , P.reservedNames   = ["BEGIN","END"] ++ hawkBuiltinVars
        , P.reservedOpNames = ["*","/","+","-"]
        })
parens     = P.parens lexer
natural    = P.natural lexer
strlit     = P.stringLiteral lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
symbol     = P.symbol lexer
whitespace = P.whiteSpace lexer

-- Pattern grammar
pattern = try range
        <|> regexp
        <|> exprp
        <|> begin
        <|> end

begin = do
     reserved "BEGIN"
     return BEGIN

end = do
     reserved "END"
     return END

exprp = do
     e <- expr
     return $ EXPR e
     <?> "expression pattern"

regexp = do
     char '/'
     s <- manyTill anyChar (char '/')
     return $ RE s
     <?> "regexp pattern"

range = do
     pStart <- singlePattern
     char ','
     pEnd <- singlePattern
     return $ RANGE pStart pEnd
     <?> "range pattern"
  where
    singlePattern = exprp <|> regexp

-- Expression grammar
expr = buildExpressionParser table term
     <?> "expression"

term = parens expr
     <|> try funcall
     <|> literal <|> fieldRef <|> variableRef <|> builtInVars

literal = (strlit >>= (return . Const . LitStr))
     <|> (natural >>= (return . Const . LitNumeric))
     <?> "literal"

fieldRef = do
   char '$'
   n <- natural
   return $ FieldRef n
   <?> "data field reference"

variableRef = do
   s <- identifier
   return $ VariableRef s
   <?> "variable reference"

funcall = do
   f <- identifier
   char '('
   s <- expr `sepBy` (symbol ",")
   char ')'
   optional whitespace
   return $ FunCall f s
   <?> "function call"

builtInVar s = do
   reserved s
   return $ BuiltInVar s        

builtInVars = choice $ map builtInVar hawkBuiltinVars

table = [ [rel "<", rel "<=", rel "==", rel "!=", rel ">=", rel ">"]
        , [arith "*", arith "/" ]
        , [arith "+", arith "-" ]
        , [asgn "=", asgn "+=", asgn "-=", asgn "*=", asgn  "/=", asgn "%=", asgn "^="]
        ]

rel   s = binary s (Relation s) AssocLeft
arith s = binary s (Arith s) AssocLeft
asgn  s = binary s (Assignment s) AssocRight

binary  name fun assoc = Infix   (do {reservedOp name; return fun}) assoc
prefix  name fun       = Prefix  (do {reservedOp name; return fun})
postfix name fun       = Postfix (do {reservedOp name; return fun})

-- Statements
statements = many expr
           <?> "statement"

-- Top-level AWK constructs
-- The AWK book does not give any names to pattern-statement pairs, so I did.
section = do
    mp <- optionMaybe pattern
    ma <- optionMaybe action
    when (isNothing mp && isNothing ma) $ fail "empty section"
    skipMany space
    return $ Section mp ma
    <?> "section"

action = do
    char '{'
    whitespace
    s <- statements
    char '}'
    return s
    <?> "action"

awk = many section
