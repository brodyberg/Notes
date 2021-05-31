module Chapter3 where

addExclamation s = s ++ "!"

headOfFourth s = head $ drop 4 s

fromNinth = drop 9  

third :: String -> Char
third s = s !! 2

letterIndex :: Int -> Char
letterIndex i = "Curry is Awesome" !! i

rvrs :: String
rvrs = third ++ " " ++ second ++ " " ++ first
  where 
    curry = "Curry is awesome"
    first = take 5 curry
    second = take 2 $ drop 6 curry
    third = drop 9 curry