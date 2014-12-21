 module Lang.Hawk.Grammar where

import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Maybe
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (when, liftM)

import Text.Parsec
import Lang.Hawk.Parsec.Language
import qualified Lang.Hawk.Parsec.Token as P
import Text.Parsec.Expr

import Lang.Hawk.AST
import Lang.Hawk.Basic

hawkBuiltinVars =
    [ ("ARGC"    ,ARGC    ) -- number of command-line arguments
    , ("ARGV"    ,ARGV    ) -- array of command-line arguments
    , ("FILENAME",FILENAME) -- name of current input file
    , ("FNR"     ,FNR     ) -- record number in current file
    , ("FS"      ,FS      ) -- controls the input field separator (default " ")
    , ("NF"      ,NF      ) -- number of fields in the current record
    , ("NR"      ,NR      ) -- number of records read so far
    , ("OFMT"    ,OFMT    ) -- output format for numbers (default "%.6g")
    , ("OFS"     ,OFS     ) -- output field separator ("default " ")
    , ("ORS"     ,ORS     ) -- output record separator (default "\n")
    , ("RLENGTH" ,RLENGTH ) -- length of string matched by `match` function
    , ("RS"      ,RS      ) -- controls the input record separator (default "\n")
    , ("RSTART"  ,RSTART  ) -- start of string matched by `match` function
    , ("SUBSEP"  ,SUBSEP  ) -- subscript separator (default "\034")
    ]

hawkFunctions = 
    [ ("atan2"  , Atan2  )
    , ("cos"    , Cos    )
    , ("exp"    , Exp    )
    , ("int"    , Int    )
    , ("log"    , Log    )
    , ("sin"    , Sin    )
    , ("sqrt"   , Sqrt   )
    , ("srand"  , Srand  )
    , ("rand"   , Rand   )
    , ("index"  , Index  )
    , ("length" , Length )
    , ("substr" , Substr )
    , ("match"  , FMatch )
    , ("sub"    , FSub   )
    , ("gsub"   , GSub   ) 
    , ("printf" , Printf )
    , ("sprintf", SPrintf)
    ]

hawkConstructs =
    [ "print"    -- prints comma-separated list of arguments, concatenated with OFS
    , "if"       -- if-then[-else] statement
    , "else"
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
    , "print"
    ]

-- Lexer
lexer = P.makeTokenParser
        ( emptyDef
        { P.commentLine     = "#" 
        , P.reservedNames   = ["BEGIN","END"] ++ (map fst hawkBuiltinVars) ++ hawkConstructs
        , P.reservedOpNames = ["*","/","+","-","%","^"
                              ,"=","*=","/=","+=","-=","%=","^="
                              ,"++","--"
                              ,"~","!~"
                              ,"==",">","<",">=","<=","!="
                              ,"&&","||","!",",",";","|"]
        })
parens     = P.parens        lexer
natural    = P.natural       lexer
float      = P.float         lexer
strlit     = P.stringLiteral lexer
rsvd       = P.reserved      lexer
rsvdOp     = P.reservedOp    lexer
identifier = P.identifier    lexer
symbol     = P.symbol        lexer
whitespace = P.whiteSpace    lexer

-- Pattern grammar
pattern = try range <|> regexp <|> exprp <|> begin <|> end <?> "pattern"
begin   = rsvd "BEGIN" >> return BEGIN
end     = rsvd "END"   >> return END
exprp   = EXPR  <$> patExpr  <?> "expression pattern"
regexp  = RE    <$> regex <?> "regexp"
range   = RANGE <$> sp <* rsvdOp "," <*> sp <?> "range pattern" where sp = exprp <|> regexp

-- Expression grammar
expr    = buildExpressionParser defaultTable term <?> "expression"
prnExpr = buildExpressionParser printTable   term <?> "expression (print)"
patExpr = buildExpressionParser patternTable term <?> "expression (pattern)" 

term = parens expr
     <|> try funcalls
     <|> try getline
     <|> literal <|> fieldRef <|> try arrayRef <|> variableRef <|> builtInVars

literal = stringLit <|> numericLit <|> regexLit <?> "literal"

regexLit   = (Const . LitRE)      <$> regex
stringLit  = (Const . LitStr)     <$> strlit
numericLit = (Const . LitNumeric) <$> (try float <|> liftM fromIntegral natural)
regex      = char '/' *> manyTill anyChar (char '/') <* whitespace <?> "regex"

fieldRef = FieldRef <$> (char '$' *> r <?> "data field reference")
   where r = numericLit <|> (try builtInVars) <|> variableRef <|> parens expr

variableRef = VariableRef <$> identifier <?> "variable reference"
arrayRef = ArrayRef <$> identifier <* char '[' <*> expr <* char ']' <* whitespace

funcall (s,f) = FunCall <$> (rsvd s >> return f) <*> args <* whitespace <?> "function call"
   where args = char '(' *> expr `sepBy` (symbol ",") <* char ')'

funcalls = choice $ map funcall hawkFunctions

builtInVar (s,b) = rsvd s >> return (BuiltInVar b)
builtInVars      = choice $ map builtInVar hawkBuiltinVars

getline   = try varFileGetline
        <|> try varGetline
        <|> plainGetline
   where varFileGetline = FGetlineVar <$> (gl *> variableRef) <*> (symbol "<" *> expr)
         varGetline     = GetlineVar  <$> (gl *> variableRef)
         plainGetline   = gl >> return Getline
         gl = rsvd "getline"


-- Decreasing presedence order!
-- The reversed version of The AWK Book's table 2-8.
table gt cc =
   [ [ prefix "++" (Incr Pre), postfix "++" (Incr Post)
     , prefix "--" (Decr Pre), postfix "--" (Decr Post) ]
   , [arith "^" Pow]
   , [prefix "!" Not]
   , [prefix "-" Neg, prefix "+" Id]
   , [arith "*" Mul, arith "/" Div, arith "%" Mod ]
   , [arith "+" Add, arith "-" Sub ]
   , cc
   , [rel "<" CmpLT, rel "<=" CmpLE, rel "==" CmpEQ, rel "!=" CmpNE, rel ">=" CmpGE] ++ gt
   , [binary "~" Match AssocRight, binary "!~" NoMatch AssocRight]
   , [binary "in" In AssocRight]
   , [logic "&&" AND]
   , [logic "||" OR]
   , [iif]
   , [ asgn "=" Set, asgn "+=" Add, asgn "-=" Sub
     , asgn "*=" Mul, asgn  "/=" Div, asgn "%=" Mod, asgn "^=" Pow]
   , [pgetlv, pgetl, fgetl]
   ]

defaultTable = table [rel ">" CmpGT] [conc]
printTable   = table []              [conc]
patternTable = table [rel ">" CmpGT] []

rel   s o = binary s (Relation   o) AssocLeft
arith s o = binary s (Arith      o) AssocLeft
asgn  s o = binary s (Assignment o) AssocRight
logic s o = binary s (Logic      o) AssocRight

binary  name fun assoc = Infix   (do {rsvdOp name; return fun}) assoc
prefix  name fun       = Prefix  (do {rsvdOp name; return fun})
postfix name fun       = Postfix (do {rsvdOp name; return fun})

fgetl   = Prefix  (try $ do {rsvd "getline"; rsvd "<"; return FGetline}) 
pgetl   = Postfix (try $ do {rsvd "|"; rsvd "getline"; return PGetline})
pgetlv  = Postfix (try $ do {rsvd "|"; rsvd "getline"; v <- variableRef;
                             return $ \e -> PGetlineVar e v})

conc = Infix (return Concat) AssocRight

iif = Infix (try $ do {symbol "?"; t <- expr; symbol ":"; return $ \c e -> InlineIf c t e})
      AssocRight

-- Statements
stExpr = Expression <$> expr

stPrnCommon = rsvd "print" *> whitespace *> args <* whitespace
   where args = prnExpr `sepBy` (symbol ",")

stPrint = PRINT <$> stPrnCommon <?> "print"

fileMod = try modAppend <|> modOvwrt
   where modAppend = symbol ">>" >> return ModAppend
         modOvwrt  = symbol ">"  >> return ModRewrite

stFPrint = FPRINT <$> stPrnCommon <*> fileMod <*> expr
   <?> "print with redirect"

stPPrint = PPRINT <$> stPrnCommon <* symbol "|" <*> expr
   <?> "print to process"

stBlock = Block <$> (symbol "{" *> spc *> many statement <* symbol "}" <* spc)
          <?> "block of statements"

stIf = IF <$> (rsvd "if" >> parens expr) <*> statement <*> tryElse
  where tryElse = optionMaybe (rsvd "else" >> statement)

stWhile = WHILE <$> (rsvd "while" >> parens expr) <*> (stBlock <|> stExpr)

stDoWhile = DO <$> (rsvd "do" >> (stBlock <|> stExpr)) <*> (rsvd "while" *> parens expr)

stFor = do
    rsvd "for"
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
    rsvd "for"
    (var,arr) <- parens $ do
       v <- variableRef
       rsvd "in"
       a <- identifier
       return (v,a)
    s <- statement
    return $ FOREACH var arr s

stDelete = DELETE <$> (rsvd "delete" >> (try arrayRef <|> variableRef))
stBreak  = rsvd "break"    >> return BREAK
stCont   = rsvd "continue" >> return CONT
stNext   = rsvd "next"     >> return NEXT
stNop    = symbol   ";"    >> return NOP
stExit   = EXIT   <$> (rsvd "exit"   >> optionMaybe expr)
stReturn = RETURN <$> (rsvd "return" >> optionMaybe expr)

statement = (try stIf
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
            <|> try stFPrint <|> try stPPrint <|> stPrint
            <|> stNop
            <|> stBlock
            <|> stExpr) <* spc
          <?> "statement"

-- Top-level AWK constructs
-- The AWK book does not give any names to pattern-statement pairs, so I did.
toplevel = function <|> section

function = do
    rsvd "function"
    name <- identifier
    params <- parens $ identifier `sepBy` (symbol ",")
    body <- statement
    return $ Function name params body

section = do
    mp <- optionMaybe pattern
    whitespace
    ma <- optionMaybe action
    when (isNothing mp && isNothing ma) $ fail "empty section"
    whitespace
    return $ Section mp ma
    <?> "section"

action = Block <$> (char '{' *> spc *> many statement <* char '}' <* spc) <?> "action"

awk = spc *> many toplevel <* eof

spc = whitespace `sepBy` newline
