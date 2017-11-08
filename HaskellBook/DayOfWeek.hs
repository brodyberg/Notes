module DayOfWeek where

data DayOfWeek = 
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Ord, Show, Eq)

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'