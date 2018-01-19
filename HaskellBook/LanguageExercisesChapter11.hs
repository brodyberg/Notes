module LanguageExercises where

import Data.Char (toUpper)

-- 1. Capitalize a word

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

x = "the red fox jumped over the lazy cat. the green turtle slept."

-- 2. Capitalize each sentence in a paragraph. 
--    Re-use capitalizeWord

withSpaces :: [[Char]] -> [Char]
withSpaces = foldr (\w wacc -> w ++ " " ++ wacc) ""

capitalizeParagraph :: String -> String
capitalizeParagraph = 
  capitalizedSentences . 
  nonEmptyNonReversedSentences . 
  makeReversedSentences
  where 
    makeReversedSentences paragraph = reversedSentences 
      where
        (_, reversedSentences) = 
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
            $ reverse paragraph
    nonEmptyNonReversedSentences = 
      foldr 
        (\s acc -> 
          if length s > 0 
          then (reverse s) : acc 
          else acc) 
        []
    capitalizedSentences = 
      foldr 
        (\s acc -> 
          let 
            (firstWord : otherwords) = words s
          in 
            acc ++ (withSpaces $ (capitalizeWord firstWord) : otherwords))
        []
