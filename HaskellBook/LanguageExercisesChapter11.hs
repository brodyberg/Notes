module LanguageExercises where

import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

x = "the red fox jumped over the lazy cat. the green turtle slept."

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

adder :: Char
      -> ([Char], [[Char]])
      -> ([Char], [[Char]])
adder c (curr, sents) = 
  let added = c : curr 
  in 
    if c == '.' 
    then ("",    added : sents) 
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

