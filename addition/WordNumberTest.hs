module WordNumberTest where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
--import Test.QuickCheck.Property-- (succeeded, failed)-
import WordNumber 
--  (digitToWord, wordToDigit, digits, numbersToBase10, numberWord, wordNumber)
  (digitToWord, digits, numberWord, wordNumber)

zeroThroughNineDigit :: Gen Int
zeroThroughNineDigit = elements [0..9]

zeroThroughNineWord :: Gen String
zeroThroughNineWord = 
  elements ["zero",  "one",    "two",
            "three", "four",   "five",
            "six"    ,"seven", "eight",
            "nine"]

-- getNonNegative :: NonNegative Int
-- getNonNegative = elements []

-- prop_all_numberWordNumber :: Property
-- prop_all_numberWordNumber = 
prop_numberWordNumber :: Property
prop_numberWordNumber = 
  forAll zeroThroughNineDigit
  (\c -> (numberWord $ wordNumber c) == c)
  
prop_numberWordNumber :: Property
prop_numberWordNumber = 
  forAll zeroThroughNineDigit
  (\c -> (numberWord $ wordNumber c) == c)

prop_wordNumberWord :: Property
prop_wordNumberWord = 
  forAll zeroThroughNineWord
  (\w -> (wordNumber $ numberWord w) == w)

main :: IO ()
--main = quickCheck prop_wordNumberWord
-- main = verboseCheck prop_wordNumberWord
-- main = verboseCheck prop_numberWordNumber

-- hspec $ do 
--   describe "digitToWord" $ do
--     it "returns Nothing for -1" $ do
--       digitToWord (-1) `shouldBe` Nothing
--     it "returns Nothing for 10" $ do
--       digitToWord 10 `shouldBe` Nothing
--     it "returns Just \"zero\" for 0" $ do
--       digitToWord 0 `shouldBe` (Just "zero")
--     it "returns Just \"one\" for 1" $ do
--       digitToWord 1 `shouldBe` (Just "one")
--     it "returns Just \"nine\" for 9" $ do
--       digitToWord 9 `shouldBe` (Just "nine")
      
--   describe "digits" $ do
--     it "returns [] for -1" $ do
--       digits (-1) `shouldBe` [] 
--     it "returns [0] for 0" $ do
--       digits 0 `shouldBe` [0]
--     it "returns [1] for 1" $ do
--       digits 1 `shouldBe` [1]
--     it "returns [0,1] for 10" $ do
--       digits 10 `shouldBe` [0,1]
--     it "returns [4,3,2,1] for 1234" $ do
--       digits 1234 `shouldBe` [4,3,2,1]
    
--   describe "wordNumber" $ do
--     it "returns \"\" for -1" $ do
--       wordNumber (-1) `shouldBe` ""
--     it "returns zero for 0" $ do
--       wordNumber 0 `shouldBe` "zero"
--     it "returns \"one zero\" for 10" $ do
--       wordNumber 10 `shouldBe` "one zero"
--     it "returns \"one two three four\" for 1234" $ do
--       wordNumber 1234 `shouldBe` "one two three four"
--     -- it "x: try to do quickcheck" $ do
--     --   property prop_x
--     it "prop: prop_numberWordNumber" $ do
--       property prop_numberWordNumber
--     it "prop: prop_wordNumberWord" $ do
--       property prop_wordNumberWord
    -- it "y: try to do quickcheck" $ do
    --   property prop_y
    -- it "z: try to do quickcheck" $ do
    --   property prop_z


      -- it "can round-trip using number -> word -> number" $ do
    --   property prop_toNumberIsReversible
    -- it "can round-trip using word -> number -> word" $ do
    --   property prop_toNumberIsReversible2


-- prop_x :: Int -> Bool
-- prop_x n = (numberWord (wordNumber n)) == n

-- prop_y :: Gen Result -- Property
-- prop_y = do
--   word <- sample zeroThroughNineWord
--   return $ if (wordNumber (numberWord word)) == word
--     then succeeded :: Result
--     else failed { reason = "bork" } :: Result

-- prop_z :: Gen Result
-- prop_z = do
--   n <- arbitrary :: Gen Int
--   return $ 
--     if ((numberWord (wordNumber n)) == n)
--     then Success { output = "brody" }
--     else Failure { reason = "z bork" }
    -- then succeeded :: Result
    -- else failed { reason = "bork" } :: Result

-- prop_toNumberIsReversible2 :: Property
-- prop_toNumberIsReversible2 = 
--   forAll zeroThroughNineWord
--   (\w -> (wordNumber $ numberWord w) == w)

-- prop_conversionReversible :: Property
-- prop_conversionReversible = 
--   forAll zeroThroughNineDigit
--   (\c -> digits c)

  --(\d -> ((wordNumber d) >>= numberWord) == d)

-- prop_toNumberIsReversible :: Int -> Bool
-- prop_toNumberIsReversible x = 
--   (numberWord $ wordNumber x) == x
