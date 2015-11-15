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

parser :: TestTree
parser =  testGroup "Parser"
    [ testPatterns
    , testExpressions
    ]

testPatterns :: TestTree
testPatterns = testGroup "Patterns"
    [ testCase "BEGIN"    $ p pattern "BEGIN"  @?= BEGIN
    , testCase "END"      $ p pattern "END"    @?= END
    , testCase "Expr"     $ p pattern "NF>3"   @?= EXPR (Relation CmpGT (BuiltInVar NF) (cln 3))
    , testCase "RegExp"   $ p pattern "/awk/"  @?= RE "awk"
    , testCase "Compound" $ p pattern "a && b" @?= EXPR (Logic AND (VariableRef "a") (VariableRef "b"))
    , testCase "Range"    $ p pattern "a, b"   @?= RANGE (EXPR (VariableRef "a")) (EXPR (VariableRef "b"))
    ]

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
