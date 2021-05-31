module Acros where

acros :: String -> String
acros str =
  [x | x <- str, x `elem` ['A'..'Z']]