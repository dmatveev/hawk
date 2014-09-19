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

hawkConstructs =
    [ "print"    -- prints comma-separated list of arguments, concatenated with OFS
    , "if"       -- if-then[-else] statement
    , "do"       -- do-while statement (body is executed at least once)
    , "while"    -- while statement (body can be skipped on an initially failed test)
    , "for"      -- for([init];[cond];[step])
    , "continue" -- proceeds to the next iteration of the enclosing do/while/for loop
    , "break"    -- breaks the execution of the enclosing do/while/for loop
    , "next"     -- proceeds execution of the entire prograp to the next input record
    , "exit"     -- causes the program to behave as the end of the input had occured;
                 -- when called from any END{} context, aborts the execution
    , "in"       -- array membership
    , "delete"   -- delete item from an array
    , "return"   -- return control (and value, if any) from function
    ]

-- Lexer
lexer = P.makeTokenParser
        ( emptyDef
        { P.commentLine     = "#" 
        , P.reservedNames   = ["BEGIN","END"] ++ hawkBuiltinVars ++ hawkConstructs
        , P.reservedOpNames = ["*","/","+","-","%","^"
                              ,"*=","/=","+=","-=","%=","^="
                              ,"++","--"
                              ,"~","!~"
                              ,"&&","||","!",",",";"]
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
     <|> literal <|> fieldRef <|> try arrayRef <|> variableRef <|> builtInVars

literal = stringLit <|> numericLit <|> regexLit
     <?> "literal"

regexLit   = regex   >>= (return . Const . LitRE)
stringLit  = strlit  >>= (return . Const . LitStr)
numericLit = natural >>= (return . Const . LitNumeric)

regex =  do
     char '/'
     s <- manyTill anyChar (char '/')
     whitespace
     return s
     <?> "regex"

fieldRef = do
   char '$'
   e <- (numericLit <|> variableRef)
   return $ FieldRef e
   <?> "data field reference"

variableRef = do
   s <- identifier
   return $ VariableRef s
   <?> "variable reference"

arrayRef = do
   n <- identifier
   char '['
   i <- expr
   char ']'
   whitespace
   return $ ArrayRef n i

funcall = do
   f <- identifier
   char '('
   s <- expr `sepBy` (symbol ",")
   char ')'
   whitespace
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

stPrint = do
    reserved "print"
    whitespace
    es <- expr `sepBy` (symbol ",")
    whitespace
    return $ PRINT es
    <?> "print"

stBlock = do
    symbol "{"
    ss <- many statement
    symbol "}"
    return $ Block ss
    <?> "block of statements"

stIf = do
    reserved "if"
    cond <- parens expr
    thenBranch <- statement
    elseBranch <- optionMaybe $ do
       reserved "else"
       statement
    return $ IF cond thenBranch elseBranch

stWhile = do
    reserved "while"
    cond <- parens expr
    body <- (stBlock <|> stExpr)
    return $ WHILE cond body

stDoWhile = do
    reserved "do"
    body <- (stBlock <|> stExpr)
    reserved "while"
    cond <- parens expr
    return $ DO body cond

stFor = do
    reserved "for"
    (mInit,mCond,mStep) <- parens $ do
       i <- optionMaybe expr
       symbol ";"
       c <- optionMaybe expr
       symbol ";"
       s <- optionMaybe expr
       return (i,c,s)
    s <- statement
    return $ FOR mInit mCond mStep s

stForEach = do
    reserved "for"
    (var,arr) <- parens $ do
       v <- variableRef
       reserved "in"
       a <- variableRef
       return (v,a)
    s <- statement
    return $ FOREACH var arr s

stDelete = do
    reserved "delete"
    a <- arrayRef
    return $ DELETE a

stBreak = reserved "break"    >> return BREAK
stCont  = reserved "continue" >> return CONT
stNext  = reserved "next"     >> return NEXT
stNop   = symbol   ";"        >> return NOP

stExit = do
    reserved "exit"
    mCode <- optionMaybe expr
    return $ EXIT mCode

stReturn = do
    reserved "return"
    mVal <- optionMaybe expr
    return $ RETURN mVal

statement = try stIf
          <|> try stDoWhile
          <|> try stWhile
          <|> try stFor
          <|> try stForEach
          <|> try stBreak
          <|> try stCont
          <|> try stNext
          <|> try stExit
          <|> try stDelete
          <|> try stReturn
          <|> stNop
          <|> stBlock
          <|> stExpr
          <|> stPrint
          <?> "statement"

-- Top-level AWK constructs
-- The AWK book does not give any names to pattern-statement pairs, so I did.
toplevel = function <|> section

function = do
    reserved "function"
    name <- identifier
    params <- parens $ identifier `sepBy` (symbol ",")
    symbol "{"
    body <- statement
    symbol "}"
    return $ Function name params body

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
    whitespace
    ss <- many toplevel
    eof
    return ss
