module Main where

import PrettyJSON

--main = do putStrLn $ compact $ renderJValue (JBool False)
main = do 
  let jval = (JArray [(JString "foo"), (JNull), (JNumber 5)])
  putStrLn $ compact $ renderJValue jval
  putStrLn $ pretty 80 $ renderJValue jval
    
    
  
