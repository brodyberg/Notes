module LanguageExercises where

import Data.Char (toUpper)

-- 1. Capitalize a word

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

x = "the red fox jumped over the lazy cat. the green turtle slept."

-- 2. Capitalize each sentence in a paragraph. 
--    Re-use capitalizeWord

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p = capitalizedParagraph
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
        $ reverse p
    nonEmptyNonReversedSentences = 
      foldr 
        (\s acc -> 
          if (length s) > 0 
          then (reverse s) : acc 
          else acc) 
        []
        reversedSentences
    capitalizedSentences = 
      foldr 
        (\s acc -> 
          let 
            allTheWords = words s
            firstWord   = head allTheWords
            otherWords  = tail allTheWords
            firstCapd   = capitalizeWord firstWord
            rawOutput   = firstCapd : otherWords
            cappedAndJoined = foldr (\w wacc -> w ++ " " ++ wacc) "" rawOutput
          in 
            cappedAndJoined : acc)
        []
        nonEmptyNonReversedSentences
    capitalizedParagraph = 
      foldr (++) "" $ reverse capitalizedSentences
