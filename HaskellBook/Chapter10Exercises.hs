module Chapter10Exercises where

-- 1

stops = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops
-- and vowels and produces 3-tuples of all possible
-- stop-vowel-stop combinations. 

f :: [Char] -> [Char] -> [(Char, Char, Char)]
f s v = [(s', v', s'') | s' <- s, v' <- v, s'' <- v]

-- b) Modify that function so that it only returns 
-- the combinations that begin with a p

f' :: [Char] -> [Char] -> [(Char, Char, Char)]
f' s v = filter (\(a, _, _) -> a == 'p') $ f s v

-- c) Now set up lists of nouns and verbs (instead of
-- stops and vowels) and modify the function to make
-- tuples representing possible noun-verb-noun 
-- sentences

nouns = ["jack", "beanstalk", "seattle", "raindrop"]
verbs = ["climb", "listen", "be", "think"]

f'' :: [[Char]] -> [[Char]] -> [([Char], [Char], [Char])]
f'' ns vs = [(n, v, n') | n <- ns, v <- vs, n' <- ns]

-- 2. What does the following mystery function do?
-- What is its type? Try to get a good sense of what 
-- it does before you test it in the REPL to verify it. 

-- before repl answer: given a [Char], pass that to 
-- words, a function that I know will return a list
-- of the words in a given string. 
-- then we get the length of that word list. 
-- that's the denominator
-- the numerator is: the sum of all the words
-- in the string. 

-- so: 
-- total character count of all the words 
-- / 
-- number of words

seekritFunc :: [Char] -> Int
seekritFunc x = 
  div (sum (map length (words x)))
      (length (words x))

-- 3. We'd really like the answer to be more precise. 
-- Can you rewrite that using fractional division?

test = "a lazy brown fox jumped over the black dog"

seekritFunc' :: (Fractional b, Num b) => [Char] -> b
seekritFunc' x = 
  (sum (map (\w -> fromIntegral $ length w) (words x))) / fromIntegral (length (words x))

----------------
-- All of the below should
--  a) use foldr
--  b) be point-free

-- 1. or

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. any

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f items = myOr (map f items)
myAny f = foldr (\item acc -> f item || acc) False

-- myAny even [1,3,5]
-- False
-- myAny odd [1,3,5]
-- True

-- 3. elem

myElem :: Eq a => a -> [a] -> Bool
myElem item items = foldr (\current _ -> current == item) False items

-- myElem 1 [1..10]
-- True
-- myElem 1 [2..10]
-- False

-- 4. reverse, don't worry about making it lazy

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- myReverse "blah"
-- "halb"
-- myReverse [1..5]
-- [5,4,3,2,1]

-- 5. map, should have same behavior as built-in map

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- myMap (+1) [1..5]
-- [2,3,4,5,6]

-- 6. filter, should have same behavior as built-in filter

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- myFilter even [1..5]
-- [2,4]

-- 7. squish, flattens list of lists into a single list

squish :: [[a]] -> [a]
squish = foldr (\x acc -> x ++ acc) []

-- squish [[1..5],[6..10]]
-- [1,2,3,4,5,6,7,8,9,10]

-- 8. squishMap, maps a function over a list and
--    concatenates the results

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) [] 

-- squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- let f x = "WO " ++ [x] ++ " OT "
-- squishMap f "blah"
-- "WO b OT WO l OT WO a OT WO h OT"

-- 9. squishAgain, flattens a list of lists into 
--    a list, this time re-use squishMap 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap 

-- 10. myMaximumBy takes a comparison function
--     and a list, and returns the greatest element
--     of the list based on the last value that the
--     comparison returned GT for

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy = undefined

-- myMaximumBy (\_ _ -> GT) [1..10]
-- 1
-- myMaximumBy (\_ _ -> LT) [1..10]
-- 10

-- 11. myMinimumBy takes a comparison function
--     and a list, and returns the smallest element
--     of the list based on the last value that the
--     comparison returned LT for

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy = undefined

-- myMinimumBy (\_ _ -> GT) [1..10]
-- 10
-- myMinimumBy (\_ _ -> LT) [1..10]
-- 1
-- myMinimumBy compare [1..10]
-- 1