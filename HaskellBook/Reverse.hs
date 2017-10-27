module Reverse where

rvrs :: String -> String
rvrs curry = third ++ " " ++ second ++ " " ++ first
where 
  first = take 5 curry
  second = take 2 $ drop 6 curry
  third = drop 9 curry

main :: IO ()
main = print $ rvrs "Curry is awesome"