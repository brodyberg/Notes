import Data.Char (isAlpha)
import Data.List (sort)

isWord :: String -> Maybe String
isWord word = 
  case null word of 
    True -> Nothing
    False -> 
      case (all isAlpha word) of
        False -> Nothing
        True -> Just word

isPalindrome :: String -> Bool
isPalindrome word = word == (reverse word)

checkPalindrome :: String -> String
checkPalindrome word = 
  case (isWord word) of 
    Just item -> 
      case (isPalindrome item) of
        True -> item ++ " is a palindrome"
        False -> item ++ " is NOT a palindrome"
    Nothing -> "Word invalid"

main :: IO ()
main = 
  do 
    putStr "Please enter a word.\n"
    word1 <- getLine
    print (checkPalindrome word1)