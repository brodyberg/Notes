module StringProcessing where

import Data.List (concat, intersperse, elemIndex)

-- 1. Write a recursive function named replaceThe 
-- which takes a text/string, breaks it into 
-- words and replaces each instance of 'the' with 'a'. 

-- Only replace exactly "the"

longString :: String
longString = "Far far away, behind the word mountains, far from the countries Vokalia and Consonantia, there live the blind texts. Separated they live in Bookmarksgrove right at the coast of the Semantics, a large language ocean. A small river named Duden flows by their place and supplies it with the necessary regelialia. It is a paradisematic country, in which roasted parts of sentences fly into your mouth. Even the all-powerful Pointing has no control about the blind texts it is an almost unorthographic life One day however a small line of blind text by the name of Lorem Ipsum decided to leave for the far World of Grammar. The Big Oxmox advised her not to do so, because there were thousands of bad Commas, wild Question Marks and devious Semikoli, but the Little Blind Text didn’t listen. She packed her seven versalia, put her initial into the belt and made herself on the way. When she reached the first hills of the Italic Mountains, she had a last view back on the skyline of her hometown Bookmarksgrove, the headline of Alphabet Village and the subline of her own road, the Line Lane. Pityful a rethoric question ran over her cheek, then"

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

isThe :: String -> Maybe String
isThe s = 
  case notThe s of
    Nothing -> Just "the"
    _       -> Nothing

replaceThe' :: String -> String
replaceThe' s = foldr (\w acc -> w ++ " " ++ acc) "" noThe
  where 
    noThe = foldr folder [] $ words s
    folder w acc = 
      case notThe w of
        Just word -> word : acc
        _         -> "a" : acc

replaceThe :: String -> String
replaceThe s = concat $ intersperse " " $ go (words s)
  where
    go [] = []
    go (w:ws) = 
      case notThe w of 
        Just word -> word : go ws
        _         -> "a" : go ws 

-- 2. Write a recursive function that takes a string, 
--    breaks it into words, and counts the number of 
--    instances of "the" followed by a vowel-initial
--    word

hasLeadingVowel :: String -> Bool
hasLeadingVowel [] = False
hasLeadingVowel (x:xs) = 
  case elemIndex x vowels of
    Just n -> True
    _      -> False
  where vowels = "aeoiu"

test1 = "i was careful the alarm was blue"
test2 = "i was careful the alarm was blue but not the rest"
test3 = "the alarm was blue but not the rest of the outcomes"

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = sum $ go $ words s
  where
    go :: [String] -> [Int] 
    go [] = [0]
    go (x:[]) = [0]
    go (x:y:xs) = 
      case isThe x of
        Just w -> if hasLeadingVowel y then 1 : go xs else 0 : go(y:xs)
        _      -> 0 : go(y:xs)

-- 3. Return the number of letters that are vowels in 
--    a word. Possible steps: test for vowelhood, 
--    return the vowels in a word, count the number
--    of elements returned

countVowels :: String -> Integer
countVowels s = 
  foldr (\c acc -> if isVowel c then 1 + acc else acc) 0 s 
  where
    isVowel c =    
      case elemIndex c "aeoiu" of
        Just _ -> True
        _      -> False

-- Next: ValidateTheWordChapter12.hs