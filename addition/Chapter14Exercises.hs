module Chapter14Exercises where

import Test.QuickCheck

half :: Fractional a => a -> a 
half x = x / 2

halfIdentity :: (Eq a, Fractional a) => a -> Bool
halfIdentity x = (((*2) . half) x) == x

main :: IO ()
main = verboseCheck (halfIdentity :: Double -> Bool)