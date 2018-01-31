module Unfolds where

import Data.List(unfoldr)

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

-- 1. Write the function myIterate using direct
--    recursion. Compare the behavior with the built-in
--    iterate to gauge correctness. 

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)
  -- holy shit there's going to be no base case!!!!

normal = take 5 $ iterate (+1) 0

-- 2. Write the function unfoldr using direct recursion
--    Compare with the built-in unfoldr to check your
--    implementation of unfoldr so that you can figure
--    it out yourself. 

-- *Unfolds> :m + Data.List
-- *Unfolds Data.List> :i unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
--         -- Defined in `base-4.9.1.0:Data.OldList'

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f x = 
  case f x of 
    Just (m, n) -> m : myUnfoldr f n
    _           -> [] 

test = take 10 $ unfoldr (\b -> if b == 3 then Nothing else Just (b, b+1)) 0
-- [0,1,2]
test' = take 10 $ myUnfoldr (\b -> if b == 3 then Nothing else Just (b, b+1)) 0
-- [0,1,2]

-- using unfoldr to do the same thing as iterate: 
-- take 10 $ unfoldr (\b -> Just (b, b + 1)) 0
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-- 3. Rewrite myIterate into betterIterate using
--    myUnfoldr. Hint: we used unfoldr to produce the
--    same results as iterate earlier. Do this with 
--    different functions and see if you can abstract
--    the structure out. 

-- It helps to have the types in front of you
-- myUnfoldr :: (b -> Maybe (a, b))
--           -> b
--           -> [a]

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = undefined -- myUnfoldr ...?

-- Remember, your betterIterate should have the
-- same results as iterate: 

-- take 10 $ iterate (+1) 0 
-- take 10 $ betterIterate (+1) 0

-- Next: NotAListChapter12.hs