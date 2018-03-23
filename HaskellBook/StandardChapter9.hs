module Standard where

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) 
  | b == True = and' bs
  | otherwise = False

-- and' [True, False]
-- False
-- and' [True, True]
-- True

-- their second example, short, but
-- I like mine above better because I'm 
-- trying to cut the recursion short 
-- if we fail but maybe && does that..?
and'' :: [Bool] -> Bool
and'' (b:bs) = b && and'' bs

-- problem 1
-- returns True if any are True
or' :: [Bool] -> Bool
or' [] = False
or' (b:bs) 
  | b == True = True
  | otherwise = or' bs

-- or' [False, True]
-- True

-- this whole | stuff is called "pattern matching"

-- problem 2
-- any' returns True if a -> Bool returns True
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (a:as) 
  | f a = True
  | otherwise = any' f as

-- any' (\c -> c == 'a') "ba"
-- True

-- problem 3 a) recursive elem

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False
elem' x (n:ns) 
  | x == n = True
  | otherwise = elem' x ns

-- elem' 'f' ['a'..'z']
-- True
-- elem' 'f' ['A'..'Z']
-- False

-- problem 3 b) elem via any

elem'' :: Eq a => a -> [a] -> Bool
elem'' x ns = any' (\c -> c == x) ns

-- elem'' 'f' ['A'..'Z']
-- False

-- problem 4 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- problem 5
-- squish flattens a list into a list

-- squish :: [[a]] -> [a]
-- squish [] = []
-- squish [(firstList:moreLists)] = 
--   firstList ++ squish moreLists

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- squish [[1..10], [11..20]]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

-- problem 6

-- squishMap maps a function over a list and
-- concatenates the results

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f list = squish lists
  where 
    lists = map f list

-- squishMap (\x -> take 3 $ repeat x) [1..5]
-- [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]

-- problem 7 

-- squishAgain flattens a list of lists into a list
-- and uses squishMap to do it

squishAgain :: [[a]] -> [a]
squishAgain lists = 
  squishMap id lists

-- *Standard> squishAgain [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]

-- problem 8 

-- take a comparison function, apply it to a list
-- and return the greatest element based on the last
-- value that the comparison returned GT

maximumBy' :: (a -> a -> Ordering)
           -> [a] -> a
maximumBy' f (x:xs) = go f x xs
  where 
    go _ left [] = left
    go f left (m:ms)
      | f left m == GT = go f left ms
      | otherwise = go f m ms

values = [1, 0, 9001, 89]
-- *Standard> maximumBy' compare values
-- 9001

-- problem 9 

-- minimumBy' takes a comparison function and a 
-- list and returns the least element in the list 
-- based on the last value for which the comparison
-- returned LT

minimumBy' :: (a -> a -> Ordering) 
           -> [a] -> a
minimumBy' f (x:xs) = go f x xs
  where 
    go _ left [] = left
    go f left (m:ms)
      | f left m == LT = go f left ms
      | otherwise = go f m ms
        
-- problem 10

maximum' :: (Ord a) => [a] -> a
maximum' list = maximumBy' compare list

minimum' :: (Ord a) => [a] -> a
minimum' list = minimumBy' compare list