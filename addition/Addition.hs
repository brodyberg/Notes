module Addition where

import Test.Hspec as H
import Test.QuickCheck

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool 
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genAZ :: Gen Char
genAZ = elements ['a'..'z']

-- sample (arbitrary :: Gen (Maybe Char))
-- sample (genTuple :: Gen (Int, NonNegative Int))
-- sample (arbitrary :: Gen (Either String Int))

genTuple :: (Arbitrary a, Arbitrary b) 
         => Gen (a, b)
genTuple = do 
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- sample (genEither :: Gen (Either Int String))
genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- sample (genMaybe :: Gen (Maybe String))
genMaybe :: (Arbitrary a) 
         => Gen (Maybe a)
genMaybe = do 
  a <- arbitrary
  elements [Just a, Nothing]    

-- sample (genMaybe' :: Gen (Maybe String))
genMaybe' :: (Arbitrary a) 
          => Gen (Maybe a)
genMaybe' = do 
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]        

sayHello :: IO ()
sayHello = putStrLn "Hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = 
             go (n - d) d (count + 1)


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreater' :: Int -> Bool
prop_additionGreater' x = if x == 51 then False else True

runQc :: IO ()
runQc = quickCheck prop_additionGreater

runQc' :: IO ()
runQc' = quickCheck prop_additionGreater'

-- warning: [-Wtype-defaults]
-- this is here to get around
-- Wtype-defaults where we were defaulting to 
-- something (Int or Integer) and the defaulting
-- action itself appears to be throwing the warning
main :: IO ()
main = hspec $ do 
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Int) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
       \ 4 remainder 2" $ do
      dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
    it "1 + 1 is greater than 1" $ do
      ((1 :: Int) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do 
      ((2 :: Int) + 2) `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
