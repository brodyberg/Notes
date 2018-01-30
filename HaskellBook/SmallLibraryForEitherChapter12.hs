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

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f e = 
  case e of
    Right n -> Just $ f n
    _       -> Nothing

-- 5. This is a general catamorphism for Either values

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f g e = 
  case e of
    Right x -> g x
    Left y  -> f y

-- 6. Same as before, but use the either' function you 
--    just wrote. 

-- couldn't do this one, my solution compiles
-- and seems like not the right answer even though
-- I do check at runtime the function g will 
-- never be invoked... 

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f e = 
  case e of 
--    Right x -> Just $ f x
  --    Right x -> Just $ either' id f e
--    Right x -> Just $ either' id f e
    Right x -> Just $ either' undefined f e
    _       -> Nothing
  
-- Next: UnfoldsChapter12.hs
          