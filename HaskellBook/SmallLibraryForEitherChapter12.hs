module SmallLibraryForEither where

-- Write each of the following functions. 
-- If more than one possible unique function exists
-- for the type, use common sense to determine what
-- it should do. 

allLeft = [Left 'a', Left 'b', Left 'c']
allRight = [Right 1, Right 2, Right 3]
mixed = [Left 'a', Right 2, Left 'c']

-- 1. Try to eventually arrive at a solution that 
--    uses foldr, even if earlier versons don't use
--    foldr. 

lefts' :: [Either a b] -> [a]
lefts' list = 
  foldr folder [] list
  where 
    folder e acc = 
      case e of 
        Right _ ->     acc
        Left n  -> n : acc

-- 2. Same as the last one. Use foldr eventually. 

rights' :: [Either a b] -> [b]
rights' list = 
  foldr folder [] list
  where
    folder e acc = 
      case e of 
        Right n -> n : acc
        Left _  ->     acc

-- 3. 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = 
  foldr folder ([],[]) list
  where
    folder e (lacc, racc) = 
      case e of 
        Right n -> (lacc, n : racc)
        Left m  -> (m : lacc, racc)

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
          