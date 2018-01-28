module ValidateTheWord where

import Data.List (elemIndex)


-- Use the Maybe type to write a function that 
-- counts the number of vowels in a string and the
-- number of consonants. If the number of vowels
-- exceeds the number of consonants return Nothing. 
-- In many human languages, vowels rarely exceed 
-- the number of consonants so when they do, it
-- *may* indicate the input isn't a word. 

newtype Word' = 
  Word' String 
  deriving (Eq, Show)

countVowels :: String -> Int
countVowels s = 
  foldr (\c acc -> if isVowel c then 1 + acc else acc) 0 s 
  where
    isVowel c =    
      case elemIndex c "aeoiu" of
        Just _ -> True
        _      -> False

mkWord :: String -> Maybe Word' 
mkWord w = 
  if vowelCount > consonantCount
  then Nothing
  else Just (Word' w)
  where
    vowelCount = countVowels w
    consonantCount = length w - vowelCount





-- Next: ItsOnlyNaturalChapter12.hs