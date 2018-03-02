module Addition where

--import Test.Hspec() as H
import Test.Hspec as H

sayHello :: IO ()
sayHello = putStrLn "Hello!"

--default (Integer)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = 
             go (n - d) d (count + 1)

-- warning: [-Wtype-defaults]
-- this is here to get around
-- Wtype-defaults where we were defaulting to 
-- something (Int or Integer) and the defaulting
-- action itself appears to be throwing the warning
main :: IO ()
main = hspec $ do 
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Int) 3 `shouldBe` (5, 2)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
      dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
    it "1 + 1 is greater than 1" $ do
      ((1 :: Int) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do 
      ((2 :: Int) + 2) `shouldBe` 4
