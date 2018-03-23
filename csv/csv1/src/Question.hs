module Question where

-- https://mail.haskell.org/pipermail/beginners/2017-November/017921.html

notThe :: String -> Maybe String
notThe word
  | word == "the" = Nothing
  | otherwise     = Just word

replaceThe :: String -> String
replaceThe word = go $ words word
  where 
    go []     = ""
    go (x:xs) = 
          case (notThe x) of
            Just x  -> x ++ " " ++ go xs
            Nothing -> " a " ++ go xs 



-- case (words word) of 
--     | 

-- go (words word)
--   where go (x:xs)
--           | Just x  = word ++ go xs
--           | Nothing = " a " ++ go xs


  -- | notThe x == Just [] = []
  --         | notThe x == Just word = word ++ go xs
  --         | notThe word == Nothing = " a " ++ go xs
