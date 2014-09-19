module Lang.Hawk.Grammar where

import Data.Maybe
import Control.Monad (when)

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr

import Lang.Hawk.AST

hawkBuiltinVars =
    [ "ARGC"     -- number of command-line arguments
    , "ARGV"     -- array of command-line arguments
    , "FILENAME" -- name of current input file
    , "FNR"      -- record number in current file
    , "FS"       -- controls the input field separator (default " ")
    , "NF"       -- number of fields in the current record
    , "NR"       -- number of records read so far
    , "OFMT"     -- output format for numbers (default "%.6g")
    , "OFS"      -- output field separator ("default " ")
    , "ORS"      -- output record separator (default "\n")
    , "RLENGTH"  -- length of string matched by `match` function
    , "RS"       -- controls the input record separator (default "\n")
    , "RSTART"   -- start of string matched by `match` function
    , "SUBSEP"   -- subscript separator (default "\034")
    ]

-- Lexer
lexer = P.makeTokenParser
        ( emptyDef
        { P.commentLine     = "#" 
        , P.reservedNames   = ["BEGIN","END"] ++ hawkBuiltinVars
        , P.reservedOpNames = ["*","/","+","-","%","^"
                              ,"*=","/=","+=","-=","%=","^="
                              ,"++","--"
                              ,"~","!~"
                              ,"&&","||","!",","]
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
        <?> "pattern"

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
     s <- regex
     return $ RE s
     <?> "regexp"

range = do
     pStart <- singlePattern
     reservedOp ","
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
     <|> (regex   >>= (return . Const . LitRE))
     <?> "literal"

regex =  do
     char '/'
     s <- manyTill anyChar (char '/')
     whitespace
     return s
     <?> "regex"

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

-- Decreasing presedence order!
-- The reversed version of The AWK Book's table 2-8.
table = [ [ prefix "++" (Incr Pre), postfix "++" (Incr Post)
          , prefix "--" (Decr Pre), postfix "--" (Decr Post) ]
        , [arith "^"]
        , [prefix "!" Not]
        , [prefix "-" Neg]
        , [arith "*", arith "/", arith "%" ]
        , [arith "+", arith "-" ]
        , [binary ":" Concat AssocRight] -- explicit concatenation operator
        , [rel "<", rel "<=", rel "==", rel "!=", rel ">=", rel ">"]
        , [binary "~" Match AssocRight, binary "!~" NoMatch AssocRight]
        , [binary "in" In AssocRight]
        , [logic "&&"]
        , [logic "||"]
        , [asgn "=", asgn "+=", asgn "-=", asgn "*=", asgn  "/=", asgn "%=", asgn "^="]
        ]

rel   s = binary s (Relation s) AssocLeft
arith s = binary s (Arith s) AssocLeft
asgn  s = binary s (Assignment s) AssocRight
logic s = binary s (Logic s) AssocRight

binary  name fun assoc = Infix   (do {reservedOp name; return fun}) assoc
prefix  name fun       = Prefix  (do {reservedOp name; return fun})
postfix name fun       = Postfix (do {reservedOp name; return fun})

-- Statements
stExpr = do
    e <- expr
    return $ Expression e

stBlock = do
    char '{'
    whitespace
    ss <- many statement
    char '}'
    whitespace
    return $ Block ss
    <?> "block of statements"

stIf = do
    reserved "if"
    cond <- parens expr
    thenBranch <- (stBlock <|> stExpr)
    elseBranch <- optionMaybe $ do
       reserved "else"
       stBlock <|> stExpr
    return $ IF cond thenBranch elseBranch

statement = try stIf
          <|> stBlock
          <|> stExpr
          <?> "statement"

-- Top-level AWK constructs
-- The AWK book does not give any names to pattern-statement pairs, so I did.
section = do
    mp <- optionMaybe pattern
    whitespace
    ma <- optionMaybe action
    when (isNothing mp && isNothing ma) $ fail "empty section"
    whitespace
    return $ Section mp ma
    <?> "section"

action = do
    char '{'
    whitespace
    s <- many statement
    char '}'
    return $ Block s
    <?> "action"

awk = do
    ss <- many section
    eof
    return ss
