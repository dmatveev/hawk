module Parser (parser) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Prim (Parser)

import Lang.Hawk.Basic
import Lang.Hawk.Grammar
import Lang.Hawk.AST

cln :: Double -> Expression
cln = Const . LitNumeric

cls :: String -> Expression
cls = Const . LitStr

p :: Parser a -> String -> a
p pp s = case parse pp "" s of
  (Left e)  -> error $ show e
  (Right a) -> a

data ParseResult a = ParseError | ParseSuccess a deriving (Eq, Show)

pr :: Parser a -> String -> ParseResult a
pr pp s = case parse pp "" s of
  (Left _)  -> ParseError
  (Right a) -> ParseSuccess a

parser :: TestTree
parser =  testGroup "Parser"
    [ testPatterns
    , testExpressions
    , testControlFlow
    , testArrays
    ]

------------------------------------------------------------
--
-- Patterns group
--
testPatterns :: TestTree
testPatterns = testGroup "Patterns"
    [ testCase "BEGIN"    $ p pattern "BEGIN"  @?= BEGIN
    , testCase "END"      $ p pattern "END"    @?= END
    , testCase "Expr"     $ p pattern "NF>3"   @?= EXPR (Relation CmpGT (BuiltInVar NF) (cln 3))
    , testCase "RegExp"   $ p pattern "/awk/"  @?= RE "awk"
    , testCase "Compound" $ p pattern "a && b" @?= EXPR (Logic AND (VariableRef "a") (VariableRef "b"))
    , testCase "Range"    $ p pattern "a, b"   @?= RANGE (EXPR (VariableRef "a")) (EXPR (VariableRef "b"))
    ]

------------------------------------------------------------
--
-- Expressions group
--
testExpressions :: TestTree
testExpressions = testGroup "Expressions"
    [ testNumericLiterals
    , testStringLiterals
    , testVariables
    , testAssignments
    , testArithOp
    , testCmpOp
    , testMatchOp
    , testLogicOp
    , testConditionalOp
    , testIncDec
    , testBuiltinFuncArith
    , testConcat
    ]

testNumericLiterals :: TestTree
testNumericLiterals = testGroup "Numeric literals"
    [ testCase "integer" $ p literal "1"      @?= cln   1.0
    , testCase "double"  $ p literal "1.0"    @?= cln   1.0
    , testCase "sign +"  $ p literal "+1"     @?= cln   1.0
    , testCase "sign -"  $ p literal "-1"     @?= cln (-1.0)
    , testCase "sign +f" $ p literal "+1.0"   @?= cln   1.0
    , testCase "sign -f" $ p literal "-1.0"   @?= cln (-1.0)
    , testCase "sci1"    $ p literal "0.1e+1" @?= cln   1.0
    , testCase "sci2"    $ p literal "10E-1"  @?= cln   1.0
    , testCase "octal"   $ p literal "001"    @?= cln   1.0
    ]

testStringLiterals :: TestTree
testStringLiterals = testGroup "String literals"
    [ testCase "word"    $ p literal "\"Hawk\""         @?= cls "Hawk"
    , testCase "phrase"  $ p literal "\"Hello, world\"" @?= cls "Hello, world"
    , testCase "empty"   $ p literal "\"\""             @?= cls ""
    , testCase "escape"  $ p literal "\"\\\"\""         @?= cls "\""
    ]

testVariables :: TestTree
testVariables = testGroup "Variables"
    [ testUserVariables
    , testBuiltInVariables
    , testFieldVariables
    ]

testUserVariables :: TestTree
testUserVariables = testGroup "User variables"
    [ testCase "var" $ p variableRef "var" @?= VariableRef "var"
    , testCase "vAR" $ p variableRef "vAR" @?= VariableRef "vAR"
    , testCase "x12" $ p variableRef "x12" @?= VariableRef "x12"
    , testCase "y_3" $ p variableRef "y_3" @?= VariableRef "y_3"
    ]

testBuiltInVariables :: TestTree
testBuiltInVariables = testGroup "Built-in variables" $ map mkTest hawkBuiltinVars
  where mkTest (s, b) = testCase s $ p builtInVars s @?= BuiltInVar b

testFieldVariables :: TestTree
testFieldVariables = testGroup "Field variables"
    [ testCase "$0"     $ p fieldRef "$0"     @?= FieldRef (cln 0)
    , testCase "$1"     $ p fieldRef "$1"     @?= FieldRef (cln 1)
    , testCase "$i"     $ p fieldRef "$i"     @?= FieldRef (VariableRef "i")
    , testCase "$NF"    $ p fieldRef "$NF"    @?= FieldRef (BuiltInVar NF)
    , testCase "$(a+1)" $ p fieldRef "$(a+1)" @?= FieldRef (Arith Add (VariableRef "a") (cln 1))
    ]

testAssignments :: TestTree
testAssignments = testGroup "Assignments"
    [ testCase "x = 1"       $ p expr "x = 1"        @?= Assignment Set (VariableRef "x")  (cln 1)
    , testCase "x+= 1"       $ p expr "x+= 1"        @?= Assignment Add (VariableRef "x")  (cln 1)
    , testCase "x-= 1"       $ p expr "x-= 1"        @?= Assignment Sub (VariableRef "x")  (cln 1)
    , testCase "x*= 1"       $ p expr "x*= 1"        @?= Assignment Mul (VariableRef "x")  (cln 1)
    , testCase "x/= 1"       $ p expr "x/= 1"        @?= Assignment Div (VariableRef "x")  (cln 1)
    , testCase "x%= 1"       $ p expr "x%= 1"        @?= Assignment Mod (VariableRef "x")  (cln 1)
    , testCase "x^= 1"       $ p expr "x^= 1"        @?= Assignment Pow (VariableRef "x")  (cln 1)
    , testCase "FS =\"\\t\"" $ p expr "FS=\"\\t\""   @?= Assignment Set (BuiltInVar FS)    (cls "\t")
    , testCase "$3 = \"foo\""$ p expr "$3 = \"foo\"" @?= Assignment Set (FieldRef (cln 3)) (cls "foo")
    ]

testArithOp :: TestTree
testArithOp = testGroup "Arithmetic" $ map mkTest arithOps
  where mkTest (op, tag) = let str = "a " ++ op ++ " b"
                           in  testCase str $ p expr str @?= Arith tag (VariableRef "a") (VariableRef "b")
        arithOps = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("%", Mod), ("^", Pow)]

testCmpOp :: TestTree
testCmpOp = testGroup "Comparison" $ map mkTest cmpOps
  where mkTest (op, tag) = let str = "a " ++ op ++ " b"
                           in  testCase str $ p expr str @?= Relation tag (VariableRef "a") (VariableRef "b")
        cmpOps = [("==", CmpEQ), ("!=", CmpNE), (">", CmpGT), (">=", CmpGE), ("<", CmpLT), ("<=", CmpLE)]

testMatchOp :: TestTree
testMatchOp = testGroup "Matching"
    [ testCase "a  ~ /test/" $ p expr "a  ~ /test/" @?= Match   (VariableRef "a") (Const $ LitRE "test")
    , testCase "a  ~ str"    $ p expr "a  ~  str"   @?= Match   (VariableRef "a") (VariableRef "str")
    , testCase "a !~ /test/" $ p expr "a !~ /test/" @?= NoMatch (VariableRef "a") (Const $ LitRE "test")
    ]

testLogicOp :: TestTree
testLogicOp = testGroup "Logic"
    [ testCase "a && b" $ p expr "a && b" @?= Logic AND (VariableRef "a") (VariableRef "b")
    , testCase "a || b" $ p expr "a || b" @?= Logic OR  (VariableRef "a") (VariableRef "b")
    , testCase "!a"     $ p expr     "!a" @?= Not       (VariableRef "a")
    ]

testConditionalOp :: TestTree
testConditionalOp = testCase "Conditional" $
    p expr "a ? b : c" @?= InlineIf (VariableRef "a") (VariableRef "b") (VariableRef "c")

testIncDec :: TestTree
testIncDec = testGroup "Unary increment/decrement"
    [ testCase "i++" $ p expr "i++" @?= Incr Post (VariableRef "i")
    , testCase "++i" $ p expr "++i" @?= Incr Pre  (VariableRef "i")
    , testCase "j--" $ p expr "j--" @?= Decr Post (VariableRef "j")
    , testCase "--j" $ p expr "--j" @?= Decr Pre  (VariableRef "j")
    ]

testBuiltinFuncArith :: TestTree
testBuiltinFuncArith = testGroup "Built-in functions" $ testAtan2 : testRand : map mkTest otherFuncs
  where testAtan2 = testCase "atan2(y,x)" $ p expr "atan2(y,x)" @?= FunCall Atan2 [ VariableRef "y"
                                                                                  , VariableRef "x" ]
        testRand  = testCase "rand()"     $ p expr "rand()"     @?= FunCall Rand []
        mkTest (f, tag) = testCase f $ p expr f @?= FunCall tag [VariableRef "x"]
        otherFuncs = [ ("cos(x)", Cos), ("exp(x)",  Exp),  ("int(x)",   Int),  ("log(x)", Log)
                     , ("sin(x)", Sin), ("sqrt(x)", Sqrt), ("srand(x)", Srand) ]

testConcat :: TestTree
testConcat = testGroup "Concatenation"
    [ testCase "a b"   $ p expr "a b"   @?= Concat (VariableRef "a") (VariableRef "b")
    , testCase "a b c" $ p expr "a b c" @?= Concat (Concat (VariableRef "a") (VariableRef "b"))
                                                           (VariableRef "c")
    , testCase "NR \":\" $0" $ p expr "NR \":\" $0" @?= Concat (Concat (BuiltInVar NR) (cls ":"))
                                                               (FieldRef (cln 0))
    ]

------------------------------------------------------------
--
-- Control flow group
--
testControlFlow :: TestTree
testControlFlow = testGroup "Control flow"
    [ testIf
    , testWhile
    , testFor
    , testDoWhile
    , testCFKeywords
    ]

testIf :: TestTree
testIf = testGroup "if"
    [ testCase "if (a) b = 1"                $ p  statement "if (a) b = 1"                @?=
          IF (VariableRef "a")
             (Expression $ Assignment Set (VariableRef "b") (cln 1))
             Nothing
    , testCase "if (b) c++   else d++"       $ pr statement "if (b) c++  else   d++"      @?=
          ParseError
    , testCase "if (b) c++\\nelse d++"       $ pr statement "if (b) c++  else\\nd++"      @?=
          ParseError
    , testCase "if (b) c++;  else d++"       $ p  statement "if (b) c++; else   d++"      @?=
          IF (VariableRef "b")
             (Expression $ Incr Post (VariableRef "c"))
             (Just $ Expression $ Incr Post (VariableRef "d"))
    , testCase "if (b) {c++} else d++"       $ p  statement "if (b) {c++} else  d++"      @?=
          IF (VariableRef "b")
             (Block [Expression $ Incr Post (VariableRef "c")])
             (Just $ Expression $ Incr Post (VariableRef "d"))
    , testCase "if (b) {c++} else {d++}"     $ p  statement "if (b) {c++} else {d++}"     @?=
          IF (VariableRef "b")
             (Block [Expression $ Incr Post (VariableRef "c")])
             (Just $ Block [Expression $ Incr Post (VariableRef "d")])
    , testCase "if (b) if (c) d++; else e++" $ p  statement "if (b) if (c) d++; else e++" @?=
          IF (VariableRef "b")
             (IF (VariableRef "c")
                 (Expression $ Incr Post (VariableRef "d"))
                 (Just $ Expression $ Incr Post (VariableRef "e")))
             Nothing
    ]

testWhile :: TestTree
testWhile = testGroup "while"
    [ testCase "while (a) a--"                 $ p statement "while (a) a--"                 @?=
          WHILE (VariableRef "a") (Expression $ Decr Post (VariableRef "a"))
    , testCase "while (--a) ;"                 $ p statement "while (--a) ;"                 @?=
          WHILE (Decr Pre (VariableRef "a")) NOP
    , testCase "while (a > b) { b -= a; a--}" $ p statement "while (a > b) { b -= a; a--}" @?=
          WHILE (Relation CmpGT (VariableRef "a") (VariableRef "b"))
                (Block [ Expression (Assignment Sub (VariableRef "b") (VariableRef "a"))
                       , NOP
                       , Expression (Decr Post (VariableRef "a")) ])
    ]

testFor :: TestTree
testFor = testGroup "for"
    [ testCase "for (;;);"                   $ p statement "for (;;);"                   @?=
          FOR Nothing
              Nothing
              Nothing
              NOP
    , testCase "for (i=0; i<10; i++);"       $ p statement "for (i=0; i<10; i++);"       @?=
          FOR (Just $ Assignment Set (VariableRef "i") (cln 0 ))
              (Just $ Relation CmpLT (VariableRef "i") (cln 10))
              (Just $ Incr Post (VariableRef "i"))
              NOP
    , testCase "for (i=0; i<10; i++) j += i" $ p statement "for (i=0; i<10; i++) j += i" @?=
          FOR (Just $ Assignment Set (VariableRef "i") (cln 0 ))
              (Just $ Relation CmpLT (VariableRef "i") (cln 10))
              (Just $ Incr Post (VariableRef "i"))
              (Expression $ Assignment Add (VariableRef "j") (VariableRef "i"))
    ]

testDoWhile :: TestTree
testDoWhile = testGroup "do..while"
    [ testCase "do i++ while (i<10)"         $ p statement "do i++ while (i<10)"         @?=
          DO (Expression (Incr Post (VariableRef "i")))
             (Relation CmpLT (VariableRef "i") (cln 10))
    , testCase "do {j--; i++} while (j > i)" $ p statement "do {j--; i++} while (j > i)" @?=
          DO (Block [ (Expression $ Decr Post (VariableRef "j"))
                    , NOP
                    , (Expression $ Incr Post (VariableRef "i")) ])
             (Relation CmpGT (VariableRef "j") (VariableRef "i"))
    ]

testCFKeywords :: TestTree
testCFKeywords = testGroup "Keywords"
    [ testCase "break"      $ p statement "break"      @?= BREAK
    , testCase "continue"   $ p statement "continue"   @?= CONT
    , testCase "next"       $ p statement "next"       @?= NEXT
    , testCase "exit"       $ p statement "exit"       @?= EXIT Nothing
    , testCase "exit 1"     $ p statement "exit 1"     @?= EXIT (Just $ cln 1)
    , testCase "exit (a+b)" $ p statement "exit (a+b)" @?= EXIT (Just $ (Arith Add (VariableRef "a") (VariableRef "b")))
    ]


------------------------------------------------------------
--
-- Arrays group
--
testArrays :: TestTree
testArrays = testGroup "Arrays"
    [ testArrAccess
    , testArrMembership
    , testForEach
    , testDelete
    ]

testArrAccess :: TestTree
testArrAccess = testGroup "Array access"
    [ testCase "a[1]"             $ p expr "a[1]"     @?=
          ArrayRef "a" (cln 1)
    , testCase "a[b]"             $ p expr "a[b]"     @?=
          ArrayRef "a" (VariableRef "b")
    , testCase "a[b+1]"           $ p expr "a[b+1]"   @?=
          ArrayRef "a" (Arith Add (VariableRef "b") (cln 1))
    , testCase "a[b[c]]"          $ p expr "a[b[c]]"  @?=
          ArrayRef "a" (ArrayRef "b" (VariableRef "c"))
    -- , testCase "a[i,j]"        $ p expr "a[i,j]"   @?= ???? TODO
    , testCase "a[b]++"           $ p expr "a[b]++"   @?=
          Incr Post (ArrayRef "a" (VariableRef "b"))
    , testCase "++a[b]"           $ p expr "++a[b]"   @?=
          Incr Pre  (ArrayRef "a" (VariableRef "b"))
    , testCase "a[b] = c"         $ p expr "a[b] = c" @?=
          Assignment Set (ArrayRef "a" (VariableRef ("b"))) (VariableRef "c")
    ]

testArrMembership :: TestTree
testArrMembership = testGroup "Array membership"
    [ testCase "a in b"           $ p  expr "a in b"     @?=
          In (VariableRef "a") (VariableRef "b")
    , testCase "1 in b"           $ p  expr "1 in b"     @?=
          In (cln 1) (VariableRef "b")
    , testCase "b in 1"           $ pr expr "b in 1"     @?=
          ParseError
    , testCase "(a+1) in b"       $ p  expr "(a+1) in b" @?=
          In (Arith Add (VariableRef "a") (cln 1)) (VariableRef "b")
    , testCase "b in (a+1)"       $ pr expr "b in (a+1)" @?=
          ParseError
    ]

testForEach :: TestTree
testForEach = testGroup "Array iteration"
    [ testCase "for (a in b) ++b[a]"     $ p statement "for (a in b) ++b[a]"     @?=
          FOREACH (VariableRef "a")
                  "b"
                  (Expression $ Incr Pre (ArrayRef "b" (VariableRef "a")))
    , testCase "for (a in b) { b[a]-- }" $ p statement "for (a in b) { b[a]-- }" @?=
          FOREACH (VariableRef "a")
                  "b"
                  (Block [ Expression $ Decr Post (ArrayRef "b" (VariableRef "a"))])
    , testCase "for (a in b) ;"          $ p statement "for (a in b) ;"          @?=
          FOREACH (VariableRef "a")
                  "b"
                  NOP
    ]

testDelete :: TestTree
testDelete = testGroup "Deleting elements"
    [ testCase "delete a[1]"   $ p  statement "delete a[1]"   @?=
          DELETE (ArrayRef "a" (cln 1))
    , testCase "delete a[1+2]" $ p  statement "delete a[1+2]" @?=
          DELETE (ArrayRef "a" (Arith Add (cln 1) (cln 2)))
    , testCase "delete a"      $ pr statement "delete a"      @?=
          ParseError
    ]

