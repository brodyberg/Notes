function x y = if (x > y) then (x + 10) else y

-- function2 :: Integer -> Integer -> Integer
-- function2 x y = if (x > y) then (x + 10) else y

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

-- null username
-- null password

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





