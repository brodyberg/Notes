module Chapter14Exercises where

import Test.Hspec
import Test.QuickCheck

half :: Fractional a => a -> a 
half x = x / 2

halfIdentity :: (Eq a, Fractional a) => a -> Bool
halfIdentity x = (((*2) . half) x) == x

main :: IO ()
main = 
  hspec $ do 
    describe "half" $ do
      it "Number *2 after half is Number" $ do
        property (halfIdentity :: Double -> Bool)