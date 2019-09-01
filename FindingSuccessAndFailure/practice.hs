import Data.Char (isAlpha)
import Data.List (sort)

function x y = if (x > y) then (x + 10) else y

function2 :: Integer -> Integer -> Integer
function2 x y = 
  case (x > y) of
    False -> y
    True -> x + 10

absVal :: (Num a, Ord a) => a -> a
absVal x =
  case (x > 0) of 
    True -> x
    _ -> negate x

validateUsernameAndPassword :: String -> String -> String
validateUsernameAndPassword username password = 
  case null username of 
    True -> case null password of 
      True -> "username and password empty"
      False -> "username empty"
    False -> case null password of 
      True -> "password empty"
      False -> "username and password good"

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word = 
  case null word of 
    True -> Nothing
    False -> 
      case (all isAlpha word) of
        False -> Nothing
        True -> Just word

checkAnagram :: String -> String -> String
checkAnagram word1 word2 = 
  case (isWord word1) of 
    Just left -> 
      case (isWord word2) of
        Just right -> 
          case (isAnagram left right) of
            True -> left ++ " and " ++ right ++ " are anagrams"
            False -> left ++ " and " ++ right ++ " are NOT anagrams"
        Nothing -> "Second word invalid"
    Nothing -> "First word invalid"

main :: IO ()
main = 
  do 
    putStr "Please enter a word.\n"
    word1 <- getLine
    putStr "Please enter a second word.\n"
    word2 <- getLine
    print (checkAnagram word1 word2)