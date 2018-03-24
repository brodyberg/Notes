module WordNumberTest where

import Test.Hspec
import WordNumber 
  (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do 
  describe "digitToWord" $ do
    it "returns Nothing for -1" $ do
      digitToWord (-1) `shouldBe` Nothing
    it "returns Nothing for 10" $ do
      digitToWord 10 `shouldBe` Nothing
    it "returns Just \"zero\" for 0" $ do
      digitToWord 0 `shouldBe` (Just "zero")
    it "returns Just \"one\" for 1" $ do
      digitToWord 1 `shouldBe` (Just "one")
    it "returns Just \"nine\" for 9" $ do
      digitToWord 9 `shouldBe` (Just "nine")
      
  describe "digits" $ do
    it "returns [] for -1" $ do
      digits (-1) `shouldBe` [] 
    it "returns [0] for 0" $ do
      digits 0 `shouldBe` [0]
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [0,1] for 10" $ do
      digits 10 `shouldBe` [0,1]
    it "returns [4,3,2,1] for 1234" $ do
      digits 1234 `shouldBe` [4,3,2,1]
    
  describe "wordNumber" $ do
    it "returns \"\" for -1" $ do
      wordNumber (-1) `shouldBe` ""
    it "returns zero for 0" $ do
      wordNumber 0 `shouldBe` "zero"
    it "returns one-zero for 10" $ do
      wordNumber 10 `shouldBe` "one-zero"
    it "returns one-two-three-four for 1234" $ do
      wordNumber 1234 `shouldBe` "one-two-three-four"