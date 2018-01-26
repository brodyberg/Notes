module Phone where

import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)
import Data.Char (isUpper, toLower, isLetter, toUpper, ord, chr)

convo :: [String]
convo = [
  "Hope is the beginning of despair",
  "Ok Marneas calm down",
  "We were betrayed at Calth",
  "True enough",
  "Only the Emperor can save us now",
  "Since when do Ultramarines say that",
  "Put on your fancy gauntlets and fight"]

type Digit = Char
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

-- 1. Create a datastructure that captures the
--    phone layout on page 457.

data Phone = Phone String [String]
  deriving (Eq, Show)

standardPhone :: Phone
standardPhone
       = Phone "123456789*0#"
               ["",     "abc", "def",
                "ghi",  "jkl", "mno",
                "pqrs", "tuv", "wxyz",
                "^",    "+ ",  ".,"]

standardCharToTaps :: Char
                   -> [(Digit, Presses)]
standardCharToTaps = charToTaps standardPhone

charToTaps :: Phone
           -> Char
           -> [(Digit, Presses)]
charToTaps (Phone keys values) c =
  if isUpper c
  then ('*', 1) : (charToTaps (Phone keys values) $ toLower c)
  else thePress
  where
    f (k, v) acc =
      if elem c (v ++ [k])
      then (k, pressCount c v): acc
      else acc
    pressCount :: Char -> String -> Presses
    pressCount k v = (fromJust $ elemIndex c (v ++ [k])) + 1
    thePress = foldr f [] $ zip keys values

tapToChar :: Phone
          -> (Digit, Presses)
          -> Char
tapToChar (Phone keys values) (digit, presses) =
  let
    kv = zip keys values
    filtered = filter (\(c, str) -> c == digit) kv
    x = head filtered
    makeTheStr (key, values) = 
      values ++ [key]
  in
    (makeTheStr x) !! (presses - 1)

standardTapsToString :: [(Digit, Presses)]
                     -> String
standardTapsToString = tapsToString standardPhone

tapsToString :: Phone
             -> [(Digit, Presses)]
             -> String
tapsToString phone taps = reverse result
  where
    (_, _, result) = foldr processor (phone, False, "") $ reverse taps

processor :: (Digit, Presses)
          -> (Phone, Bool, String)
          -> (Phone, Bool, String)
processor tap (p, upCase, acc) = 
  let
    tapChar = tapToChar p tap
    capitalize = tap == ('*', 1)
    outputChar = 
      if upCase && isLetter tapChar 
      then toUpper tapChar
      else tapChar
  in 
    if capitalize
    then (p, capitalize, acc)
    else (p, capitalize, outputChar : acc)

tapsa = [('2' :: Digit, 1 :: Presses)]
tapsA = [('*' :: Digit, 1 :: Presses), ('2', 1 :: Presses)]

(Phone keys values) = standardPhone
kv = zip keys values

standardStringToTaps :: String
                     -> [(Digit, Presses)]
standardStringToTaps = stringToTaps standardPhone

stringToTaps :: Phone
             -> String
             -> [(Digit, Presses)]
stringToTaps p s = foldr (++) [] $ map (\c -> charToTaps p c) s
-- standardStringToTaps $ convo !! 1
-- [('*',1),('6',3),('5',2),('0',2),('*',1),('6',1),('2',1),('7',3),('6',2),('3',2),('2',1),('7',4),('0',2),('2',3),('2',1),('5',3),('6',1),('0',2),('3',1),('6',3),('9',1),('6',2)]

hibd = "Hope is the beginning of despair."
hibd' = standardStringToTaps hibd
rhibd = standardTapsToString hibd' 

-- 3. How many times do digits need to be pressed
--    for each message?

fingerTapCount :: [(Digit, Presses)] -> Presses
fingerTapCount = foldr (\(d, p) acc -> p + acc) 0

-- 4. What was the most popular letter for each message?
--    What was its cost? You'll want to combine reverseTaps
--    and fingerTaps figure out what it cost in taps. 
--    ReverseTaps is a list because you need to press a 
--    different button in order to get capitals

beforeSlice :: Int -> [a] -> [a]
beforeSlice index acc = 
  if index == 0
  then []
  else take index acc

afterSlice :: Int -> [a] -> [a]
afterSlice index acc = 
  if (index - 1) == (length acc)
  then []
  else drop (index + 1) acc

largestLetter :: Int
              -> (Int, (Int, Int))
              -> (Int, (Int, Int))
largestLetter count (index, t@(largestIndex, largestCount)) =
  if count > largestCount
  then (newIndex, (index, count))
  else (newIndex, t)
  where
    newIndex = index + 1

mostFrequentLetter :: [Int] ->  Char
mostFrequentLetter counts = winnerChar
  where
    (_, winnerChar) = mostFrequentLetterAndMagnitude counts

mostPopularLetter :: String -> Char
mostPopularLetter s = winnerChar 
  where
    (_, winnerChar) = mostPopularLetterAndMagnitude s

mostFrequentLetterAndMagnitude :: [Int] ->  (Int, Char)
mostFrequentLetterAndMagnitude counts = (magnitude, chr (winnerOrd + 97))
  where
    (_, (winnerOrd, magnitude)) = foldr largestLetter (0, (0, 0)) $ reverse counts

mostPopularLetterAndMagnitude :: String -> (Int, Char)
mostPopularLetterAndMagnitude s = 
  mostFrequentLetterAndMagnitude $ letterCounts s

letterCounts :: String
             -> [Int]
letterCounts s = counts sentenceAllLowerLetters
  where
    sentenceAllLowerLetters = 
      foldr (\c acc -> if isLetter c then (toLower c) : acc else acc) "" s
    counts sent = 
      foldr 
        (\c acc -> 
          let 
            index = (ord c) - 97
            indexValue = acc !! index
            newValue = indexValue + 1
          in 
            (beforeSlice index acc) ++ [newValue] ++ (afterSlice index acc))
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        sent

-- 5. What was the most popular letter overall? What was the 
--    most popular word? 

acc :: [Int]
acc = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

folder :: String
       -> [Int]
       -> [Int]
folder s acc = combinedLetterCounts
  where 
    thisLetterCount = letterCounts s
    combinedLetterCounts = map (\(x, y) -> x + y) $ zip acc thisLetterCount

maxFolder :: Int
          -> (Int, Int, Int)
          -> (Int, Int, Int)
maxFolder count (index, curMax, curIx) = 
  if count > curMax 
  then (index + 1, count, index) 
  else (index + 1, curMax, curIx)

maxOfList :: [Int] -> (Int, Int, Int)
maxOfList = foldr maxFolder (0, 0, 0)

x = overallLetterCounts convo

overallLetterCounts :: [String] -> [Int]
overallLetterCounts sentences = 
  foldr 
    folder
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    sentences

mostPopularLetterOverall :: [String] -> (Int, Char)
mostPopularLetterOverall sentences = (count, actualChar)
  where 
    (_, count, ordinal) = maxOfList $ reverse $ overallLetterCounts sentences
    actualChar = chr (ordinal + 97)

convoMostPopularLetter = mostPopularLetterOverall convo

sentenceWithTheMostOfAParticularLetter :: [String] -> (Int, Char, String)
sentenceWithTheMostOfAParticularLetter sentences = 
  foldr 
    (\s t@(magnitude, largestLetter, sLeader) -> 
      let 
        (sMag, sChar) = mostPopularLetterAndMagnitude s
      in 
        if sMag > magnitude
        then (sMag, sChar, s)
        else t) 
    (0, 'a', "") 
    sentences

data Trie a = 
  Node a Int [Trie a]
  deriving (Eq, Show)

insert' :: Eq a 
        => [a]
        -> Trie a
        -> Trie a
insert' [] (Node item count children) = 
  (Node item (count + 1) children)
insert' (x:xs) (Node item count children) = 
  case branchIndex of 
    Just ix -> (Node item count ((before ix) ++ [(insert' xs (toUpdate ix))] ++ (after ix)))
    _       -> (Node item count ((insert' xs (Node x 0 [])) : children))
  where 
    before ix = beforeSlice ix children
    after ix = afterSlice ix children
    toUpdate ix = children !! ix
    branchIndex = findIndex (\(Node c _ _) -> c == x) children

convoInTrie :: [String]
            -> Trie Char
convoInTrie sentences = 
  foldr (\w acc -> insert' w acc) (Node '_' 0 []) allWordsLower
  where 
    allTheWords = foldr (\s acc -> words s ++ acc) [] sentences
    allWordsLower = foldr (\w acc -> (foldr (\c cacc -> toLower c : cacc) "" w) : acc) [] allTheWords

mostPopularWord :: [String] -> Char
mostPopularWord = undefined 
  -- read trie for path (word) with max count

-- we could take a word and return a count

maybeIndex :: Eq a 
           => a
           -> Trie a
           -> Maybe Int
maybeIndex x (Node _ _ children) = findIndex (\(Node c _ _) -> c == x) children

frequencyOfWord :: String
                -> Trie Char
                -> Maybe Int
frequencyOfWord [] t@(Node _ count _) = Just count
frequencyOfWord (x:xs) (Node item count children) = 
  case maybeIndex x t of
    Just ix -> frequencyOfWord xs (children !! ix)
    _       -> Nothing
