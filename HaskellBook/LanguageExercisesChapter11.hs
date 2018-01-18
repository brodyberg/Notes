module LanguageExercises where

import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

x = "the red fox jumped over the lazy cat. the green turtle slept."

-- isVowel :: Char -> Bool
-- isVowel 'a' = True
-- isVowel 'e' = True
-- isVowel 'i' = True
-- isVowel 'o' = True
-- isVowel 'u' = True
-- isVowel _   = False

-- resultV = foldr (\c (vowels, acc) -> (if isVowel c then c : vowels else vowels, c : acc)) ("", "") x

-- resultL = 
--   foldr 
--     (\s (shorts, acc) -> 
--       (if length s <= 3 then s : shorts else shorts, s : acc)) 
--       ([], []) $ words x

resultSeenOrNot = 
  foldr 
    (\c (untilSeen, seenList) -> 
      let 
        thisIsPeriod = c == '.' 
        glom = c : untilSeen
      in 
        if thisIsPeriod
        then ([], glom : seenList)
        else (glom, seenList))
    ([], [])
    x
    

-- resultSeenOrNot = 
--   foldr 
--     (\c (seen, untilSeen, seenList) -> 
--       let thisIsPeriod = c == '.' 
--       in 
--         if (thisIsPeriod == false)
--         then (seen, c : untilSeen, seenList)
--         else ()
    
    
--     )


-- fold
-- acc is (curr, [[String]])
-- if char is . then add it and curr to [[string]]
-- then
-- map each to capitalize first (watching for leading space)
-- then concat all
-- how to reuse capitalizeWord?
--   foldr (++) $ capitalizeWord $ head $ words sentence?

sentences :: [Char] -> [[Char]]
sentences s = result
  where (_, result) = foldr adder ("", []) s

sentences' :: [Char] -> ([Char], [[Char]])
sentences' s = (curr, result)
  where (curr, result) = foldr adder ("", []) s

sentences'' :: [Char] -> ([Char], [[Char]])
sentences'' s = (curr, result)
  where (curr, result) = foldr adder_ ("", []) s
  
adder_ :: Char
  -> ([Char], [[Char]])
  -> ([Char], [[Char]])
adder_ '.' (curr, sents) = ("", ('.' : curr) : sents)
adder_ c   (curr, sents) = (c : curr, sents)

adder :: Char
      -> ([Char], [[Char]])
      -> ([Char], [[Char]])
adder c (curr, sents) = 
  let added = c : curr
  in 
    if c == '.' 
    then (added,    added : sents) 
    else (added, sents)

capitalizedSentence :: [Char] -> [Char]
capitalizedSentence [] = []
capitalizedSentence s = firstCapitalized ++ " " ++ (foldr (++) "" listRest)
  where
    listWords :: [[Char]]
    listWords = words s
    firstWord ::[Char]
    firstWord = head listWords
    listRest :: [[Char]]
    listRest = tail listWords
    firstCapitalized = capitalizeWord firstWord

capitalizedSentences :: [Char] -> [[Char]]
capitalizedSentences s = map capitalizedSentence $ sentences s

capitalizeParagraph :: [Char] -> [Char] 
capitalizeParagraph [] = []
capitalizeParagraph s = 
  foldr (++) "" $ capitalizedSentences s

