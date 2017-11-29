module MyLines where

firstSen = "Tyger, Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = go s []
  where go s acc = 
          case s of
            [] -> acc
            _  -> takeWhile (/= '\n') s : 
                  go (dropWhile (== '\n') $ dropWhile (/= '\n') s) acc

myLines' :: Char -> String -> [String]
myLines' c s = go c s []
  where go c s acc = 
          case s of
            [] -> acc
            _  -> takeWhile (/= c) s : 
                  go c (dropWhile (== c) $ dropWhile (/= c) s) acc
                  
shouldEqual = 
  [ "Tyger, Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = 
  print $ 
  "Is myLines working? "
  ++ show (myLines sentences == shouldEqual)
  ++ " Is myLines' working? "
  ++ show (myLines' '\n' sentences == shouldEqual)