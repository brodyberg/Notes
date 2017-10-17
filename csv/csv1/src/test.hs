-- import Test.Tasty
-- import Test.Tasty.HUnit 

-- import SimpleJSON (JValue(..))
-- import Prettify (Doc, (<>), char, string, double, fsep, hcat, punctuate, text, compact, series, pretty, nest)

-- main = defaultMain tests

-- tests :: TestTree
-- tests = testGroup "tests" [properties, unitTests]

-- -- properties :: TestTree
-- -- properties = testGroup "Properties" [scProps, qcProps]

-- unitTests = testGroup "Unit tests"
--   [ testCase "Char constructor is the Char" $
--       char 's' @?= (Char 's') ]