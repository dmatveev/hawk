import Test.Framework (Test, defaultMain)


import Parser

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ parser ]
