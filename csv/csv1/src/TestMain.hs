import Test.Tasty
import Test.Tasty.HUnit 

import PrettyJSON
import SimpleJSON (JValue(..))
-- import Prettify (Doc, (<>), char, string, double, fsep, hcat, punctuate, text, compact, series, pretty) -- , nest)

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [unitTests]

simplest :: JValue
simplest = (JObject [("k", (JString "v"))])

unitTests = testGroup "Unit tests"
  [ --testCase "Compact of char is the char" $
      --(compact $ char 's') @?= "s",
    testCase "Compact of simplest untouched" $
      (compact $ renderJValue simplest) @?= "{\"k\": \"v\"\n}",
    testCase "Nest of simplest tabs once" $
      (pretty 80 $ nest 2 $ renderJValue simplest) @?= "{\n  \"k\": \"v\" }",
    testCase "Passthrough implementation does nothing" $
      (pretty 80 $ renderJValue simplest) @?= "{\"k\": \"v\" }"
  ]