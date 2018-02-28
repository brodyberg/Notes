module Addition where

--import Test.Hspec() as H
import Test.Hspec as H

sayHello :: IO ()
sayHello = putStrLn "Hello!"

--default (Integer)

main :: IO ()
main = hspec $ do 
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      -- --let x = 1 :: Int
      -- let x = 1
      -- (x + x) > x `shouldBe` True
       
          -- warning: [-Wtype-defaults]
          -- this is here to get around
          -- Wtype-defaults where we were defaulting to 
          -- something (Int or Integer) and the defaulting
          -- action itself appears to be throwing the warning
      ((1 :: Int) + 1) > 1 `shouldBe` True
