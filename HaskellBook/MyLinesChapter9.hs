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

shouldEqual = 
  [ "Tyger, Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame they fearful symmetry?"
  ]

main :: IO ()
main = 
  print $ 
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)