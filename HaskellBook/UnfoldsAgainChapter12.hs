module UnfoldsAgain where

-- 1. Write using direct recursion. Must produce
--    results equal to iterate. 
myIterate :: (a -> a)
          -> a 
          -> [a]
myIterate f x = x : myIterate f (f x)

test = take 5 $ myIterate (+1) 0

-- 2. Write myUnfoldr using direct recursion. 

myUnfoldr :: (b -> Maybe(a, b))
          -> b
          -> [a]
myUnfoldr f x = 
  case f x of
    Just (m, n) -> m : myUnfoldr f n
    _           -> []

-- am I half remembering or actually doing this?
-- interesting that I'm writing the puller then the 
-- producer...

test2 = take 5 $ myUnfoldr (\x -> Just(x, x + 1)) 0

-- 3. Rewrite myIterate to instead use myUnfoldr. 


-- wait, what the fuck is going on -  
-- we have (a -> a) getting applied inside the
-- function we pass to myUnfoldr in order to generate
-- the Maybe(a,b). That could exactly be the thing
-- that we use to count in BinaryTree ...
betterIterate :: (a -> a)
              -> a
              -> [a]
betterIterate f x = myUnfoldr (\n -> Just(n, f n)) x 

-- DO THIS: Write the puller then write the producer
test3 = take 5 $ betterIterate (+1) 0

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe(a, b, a))
       -> a
       -> BinaryTree b
unfold f x = 
  case f x of
    Just(l, c, r) -> Node (unfold f l) c (unfold f r)
    _             -> Leaf

treeBuild :: Int
          -> BinaryTree Int
treeBuild x = 
  unfold (\n -> if n < x then Just(n + 1, n + 1, n + 1) else Nothing) 0

  -- START AT FUCKING ZERO
  -- GO UP
  -- IF YOU GET TO THE LIMIT FUCKING STOP