module SmallLibraryForEither where

-- Write each of the following functions. 
-- If more than one possible unique function exists
-- for the type, use common sense to determine what
-- it should do. 

-- 1. Try to eventually arrive at a solution that 
--    uses foldr, even if earlier versons don't use
--    foldr. 

lefts' :: [Either a b] -> [a]
lefts' = undefined

-- 2. Same as the last one. Use foldr eventually. 

rights' :: [Either a b] -> [b]
rights' = undefined

-- 3. 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = undefined

-- 4. 

eitherMaybe' :: (a -> b)
             -> Either a b
             -> Maybe c
eitherMaybe' = undefined

-- 5. This is a general catamorphism for Either values

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' = undefined

-- 6. Same as before, but use the either' function you 
--    just wrote. 

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' = undefined

-- Next: UnfoldsChapter12.hs
          