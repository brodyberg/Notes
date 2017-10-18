import Test.Tasty
import Test.Tasty.HUnit 

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, string, double, fsep, hcat, punctuate, text, compact, series, pretty) -- , nest)

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Compact of char is the char" $
      (compact $ char 's') @?= "s" ]