import Test.Tasty

import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Hawk tests" [ parser ]
