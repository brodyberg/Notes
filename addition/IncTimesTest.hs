module IncTimesTest where

import Test.Hspec

incTimes :: (Ord a, Eq a, Num a) => a -> a -> a
incTimes times n = 
  if times == 0 then n
  else if times > 0 then 1 + (incTimes (times - 1) n)
  else n 

main :: IO ()
main = hspec $ do
  describe "incTimes" $ do
    it "Incrementing 0 times results in number" $ do
      incTimes (0 :: Int) 1 `shouldBe` 1
    it "Incrementing 1 time results in number + 1" $ do
      incTimes (1 :: Int) 0 `shouldBe` 1
    it "Incrementing by (-1) results in number" $ do
      incTimes ((-1) :: Int) 0 `shouldBe` 0
  