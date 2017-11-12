module Matching where

-- isItTwo :: Integer -> Bool 
-- isItTwo 2 = True
-- isItTwo _ = False

-- warning: [-Woverlapping-patterns]
-- isItTwo :: Integer -> Bool
-- isItTwo _ = False
-- isItTwo 2 = True

-- warning: [-Wincomplete-patterns]
-- isItTwo 3 
-- *** Exception: Non-exhaustive patterns in function isItTwo
isItTwo :: Integer -> Bool
isItTwo 2 = True