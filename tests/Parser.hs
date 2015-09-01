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

p :: Parser a -> String -> a
p pp s = case parse pp "" s of
  (Left e)  -> error $ show e
  (Right a) -> a

parser :: TestTree
parser =  testGroup "Parser"
    [ testPattern
    , testNumericLiteral
    ]

testNumericLiteral :: TestTree
testNumericLiteral = testGroup "Numeric literals"
    [ testCase "integer" $ p literal "1"      @?= cln   1.0
    , testCase "double"  $ p literal "1.0"    @?= cln   1.0
    , testCase "sign +"  $ p literal "+1"     @?= cln   1.0
    , testCase "sign -"  $ p literal "-1"     @?= cln (-1.0)
    , testCase "sci1"    $ p literal "0.1e+1" @?= cln   1.0
    , testCase "sci2"    $ p literal "10E-1"  @?= cln   1.0
    , testCase "octal"   $ p literal "001"    @?= cln   1.0
    ]

testPattern :: TestTree
testPattern = testGroup "Patterns"
    [ testCase "BEGIN"    $ p pattern "BEGIN"  @?= BEGIN
    , testCase "END"      $ p pattern "END"    @?= END
    , testCase "Expr"     $ p pattern "NF>3"   @?= EXPR (Relation CmpGT (BuiltInVar NF) (cln 3))
    , testCase "RegExp"   $ p pattern "/awk/"  @?= RE "awk"
    , testCase "Compound" $ p pattern "a && b" @?= EXPR (Logic AND (VariableRef "a") (VariableRef "b"))
    , testCase "Range"    $ p pattern "a, b"   @?= RANGE (EXPR (VariableRef "a")) (EXPR (VariableRef "b"))
    ]
