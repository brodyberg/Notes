module FoldingExample where

xs :: [[Char]]
xs = map show [1..5]

y :: [Char]
y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

z :: [[Char]] -> [Char]

                                -- single value
                                        -- xs
                                           -- so how do we recurse...
z = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0"

z' :: [[Char]] -> [Char]
-- concat: [[a]] -> [a]
-- concat :: Foldable t => t [a] -> [a]
z' = foldr (\x xs -> concat ["(", x, "+", xs, ")"]) "0"

-- I think the idea is just that the implementation of foldr is
-- such that when it's rewriting, it determines if there's more
-- to the list and if so then we recurse at that point 

foldr concat 0 ["1"] =
  case ["1"] of
    [] -> 0 -- doesn't match
    ("1": []) -> concat "1" (foldr concat 0 [])

-- the point is that: 
--   the folding function passed to foldr joins
--   the implementation of foldr is such that we recurse directly
--       into the rest of the list after re-writing the folding function
--       and single item 


-- i just don't see from this example how we 
-- run foldr concat xs *on* y recursively
-- 

-- foldr 

-- concat "(1+"
--   concat "(2+"
--     concat "(3+"
--       concat "(4+"
--         concat "(5+"
--           "0"
--                ")"
--              ")"
--            ")"
--          ")"
--        ")"
             
            