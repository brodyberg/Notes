module Unfolds where

-- While the idea of catamorphisms are still
-- relatively fresh in our minds, let's turn our 
-- attention to their dual: anamorphisms. If folds, 
-- or catamorphims, let us break data structures
-- down then unfolds let us build them up. There 
-- are, as with folds, a few different ways to 
-- unfold a datastructure. We can use them to create
-- finite and infinite data structures alike. 

-- iterate is like a limited unfold that never ends
-- :t iterate
-- iterate :: (a -> a) -> a -> [a]

-- because it never ends, we must use take to get
-- a finite list

-- take 10 $ iterate (+1) 0
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-- unfoldr is more general
-- :t unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- using unfoldr to do the same thing as iterate: 
-- take 10 $ unfoldr (\b -> Just (b, b + 1)) 0
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-- there's lots of content to read here

-- Next: NotAListChapter12.hs