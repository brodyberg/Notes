module Chapter14ListTest where

--import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where 
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, _)       = (Just y, x >= y)

prop_sortedList :: (Ord a) => [a] -> Bool
prop_sortedList list = listOrdered $ sort list

main :: IO ()
main = verboseCheck (prop_sortedList :: [Int] -> Bool)
