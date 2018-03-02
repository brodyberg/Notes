module IncTimesTest where

import Test.Hspec

-- incTimes :: (Ord a, Eq a, Num a) => a -> a -> a
-- incTimes times n = 
--   case times of 
--     times <  0 -> n
--     times == 0 -> n
--     times >  0 -> 1 + (incTimes (times - 1) n)

-- incTimes :: (Ord a, Eq a, Num a) => a -> a -> a
-- incTimes times n | times <  0 = n
--                  | times == 0 = n
--                  | times >  0 = 1 + (incTimes (times - 1) n)

incTimes :: (Ord a, Eq a, Num a) => a -> a -> a
incTimes times n = 
  if times == 0 then n
  else if times > 0 then 1 + (incTimes (times - 1) n)
  else n 

-- | (times <  0) _ = n 
  -- | (times == 0) _ = n
  -- | (times >  0) _ = 1 + (incTimes (times - 1) n)

-- incTimes :: (Ord a, Eq a, Num a) => a -> a -> a
-- incTimes times n
--   | (times <  0) _ = n 
--   | (times == 0) _ = n
--   | (times >  0) _ = 1 + (incTimes (times - 1) n)

main :: IO ()
main = hspec $ do
  describe "incTimes" $ do
    it "Incrementing 0 times results in number" $ do
      incTimes (0 :: Int) 1 `shouldBe` 1
    it "Incrementing 1 time results in number + 1" $ do
      incTimes (1 :: Int) 0 `shouldBe` 1
    it "Incrementing by (-1) results in number" $ do
      incTimes ((-1) :: Int) 0 `shouldBe` 0

-- main :: IO ()
-- main = hspec $ do 
--   describe "incTimes" $ do
--     it "15 divided by 3 is 5" $ do
--       dividedBy (15 :: Int) 3 `shouldBe` (5, 2)
  