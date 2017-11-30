module SquareCube where

mySquare = [x^2 | x <- [1..5]]
myCube   = [y^3 | y <- [1..5]]

tuples = [(x,y) | x <- mySquare, y <- myCube]

-- *SquareCube> take 5 mySquare
-- [1,4,9,16,25]
-- *SquareCube> take 10 tuples
-- [(1,1),(1,8),(1,27),(1,64),(1,125),(4,1),(4,8),(4,27),(4,64),(4,125)]

tuplesLessThan50 = 
  [(x,y) | x <- mySquare, y <- myCube, x < 50, y < 50]

-- length $ take 100 tuplesLessThan50