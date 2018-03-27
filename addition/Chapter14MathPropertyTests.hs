module Chapter14MathPropertyTests where

import Test.QuickCheck

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

testAssoc :: IO ()
testAssoc = verboseCheck (plusAssociative :: Int -> Int -> Int -> Bool)

-- do same for multiplication