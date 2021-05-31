-- File: 
-------------
import Data.Monoid
import Control.Applicative(liftA2)
import Control.Monad.Identity
import Data.Char
import qualified Data.Map as Map(lookup, fromList)

newtype Identity2 a = Identity2 a
  deriving (Eq, Ord, Show)
  
instance Functor Identity2 where
--  fmap f x = Identity2 f x 
  fmap f (Identity2 x) = Identity2 (f x)
   
instance Applicative Identity2 where
  pure = Identity2
  (<*>) (Identity2 f) (Identity2 x) = fmap f (Identity2 x)
--  (<*>) (Identity2 f) (Identity2 x) = Identity2 $ f x
--  (<*>) (Identity2 f) (Identity2 x) = fmap

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

--let c (x:xs) = toUpper x:xs
--let c (x:xs) = toUpper x:xs-------------
-- Interactive: 

fmap (+2) $ Identity2 3
(++) <$> Identity2 "foo" <*> Identity2 "bar"

Identity 3

const <$> Identity [9, 9, 9] <*> Identity [1, 2, 3]
const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
const <$> [1,2,3] <*> [9,9,9]

x = liftA2 (++)

(g 9) `x` (f 4)
-- (g 9) `liftA2 (++)` (f 4)

liftA2 (++) (g 9) (f 4)
liftA2 (^) (h 5) (m 4)
liftA2 (*) (h 5) (m 4)
liftA2 (*) (h 1) (m 1)

f 3
g 8
(++) <$> f 3 <*> g 7
(+) <$> h 5 <*> m 1
(+) <$> h 5 <*> m 6

c (x:xs) = toUpper x:xs

fmap c $ Map.lookup 3 (Map.fromList [(3, "hello")])

fmap (\(x:xs) -> toUpper x:xs) $ lookup 3 [(3, "hello")]
fmap c $ Just "hello"
fmap c $ lookup 3 [(3, "hello")]

length <$> Just "Hello"
fmap length $ Just "Hello"

:t Just "Hello"

length $ lookup  3 [(3, "hello")]
length $ "Hello"
length "Hello"

fmap length $ lookup 3 [(3, "hello")]

lookup 3 [(3, "Hello")]

max <$> [1,2] <*> [1,4]
liftA2 max [1,2] [1,4]

(+) <$> [1,2] <*> [3,5]
liftA2 (+) [1,2] [3,5]

liftA2 (,) [1,2] [3,4]
(,) <$> [1,2] <*> [3,4]

(,) <$> [1,2]
liftA2 (,) [1,2]

-- (\x y z -> x + y + z) <$> [1,2,3] -- <$> [4,5,6] -- <*> [7,8,9]

(,) <$> [1,2] <*> [3,4]
(,,) <$> [1,2, 3] <*> [3,4, 5]
(+1) <$> [1,2,3]

-- [] <$> [1,2] <*> [3,4]

[(+1), (*2)] <*> [2,4]

Just (+1) <*> Just 3

((All True), (+1)) <*> ((All False), 0)

(All True) <> (All True)
(All False) <> (All True)

(Product 3) <> (Product 4)
(Sum 3) <> (Sum 4)

getSum (Sum 2)
fst $ ((Product 3), (+9)) <*> ((Product 2), 8)

getProduct $ fst $ ((Product 3), (+9)) <*> ((Product 2), 8)

((Product 3), (+9)) <*> ((Product 2), 8)
((Sum 2), (+1)) <*> ((Sum 0), 0)

-- a combines via Monoid instance
-- b combines via function application
("Woo", (+1)) <*> (" Hoo!", 0)

Right (+3) <*> Right 4
Left "a" <*> Right 4

Just (+2) <*> Nothing
Just (+2) <*> Just 4
Nothing <*> Just 4

(+1) $ 1
fmap (+1) [1,2,3]
(+1) <$> [1,2,3]

pure (+1) <*> [1..3]

[(+1),(+2)] <*> [1..3]

[1,2,3] >>= return . (+1)

pure 1 :: [Int]
pure 1 :: Maybe Int
pure 1 :: Either a Int
pure 1 :: ([a], Int)
