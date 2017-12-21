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
