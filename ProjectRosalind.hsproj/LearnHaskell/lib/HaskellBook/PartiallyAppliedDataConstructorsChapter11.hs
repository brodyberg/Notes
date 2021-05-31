module PartiallyAppliedDataConstructors where

data ThereYet = 
  There Float Int Bool
  deriving (Eq, Show)

--nope :: Double -> Int -> Bool -> ThereYet


notYet :: Int -> Bool -> ThereYet
notYet i b = There 25.5 i b

notQuite :: Bool -> ThereYet
notQuite b = notYet 10 b

yussss :: ThereYet
yussss = notQuite False

x = There 5.5
-- x :: Int -> Bool -> ThereYet

y = x 10
-- y :: Bool -> ThereYet

z = y True
-- z :: ThereYet