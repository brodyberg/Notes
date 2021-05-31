{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

-- Exericses Chapter 11 Logic Goats

-- 1. Write an instance of TooMany for (Int, String)
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- tooMany (45, "bar") fails because 45 is still :: Num a
-- tooMany (45 :: Int, "bar") works

-- 2. Write an instance of TooMany for (Int, Int)
instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 43

-- 3. Write an instance of TooMany for
-- (Num a, TooMany a) => (a, a)

instance (Num a, TooMany a) => TooMany (a, a) where
---  tooMany (n, m) = (n + m) > 42 
  tooMany (n, m) = tooMany n && tooMany m 

-- Got amazing type errors at this point
-- when passing tooMany (4, 9) to the above
-- I think in part because there is no Int 
-- instance for TooMany

instance TooMany Int where
  tooMany n = n > 42

-- Ok this also didn't resolve the issue
-- because raw numbers in GHCI aren't clamping
-- to Int but instead remain Num a. 

-- How about making an instance for Num a?

-- instance (Num a) => TooMany a where
--   tooMany n = n > 42

-- This brings an interesting error: 
-- LogicGoatsChapter11.hs:42:10: error:
--     * The constraint `Num a' is no smaller than the instance head
--       (Use UndecidableInstances to permit this)
--     * In the instance declaration for `TooMany a'

newtype MyNum a = MyNum a

instance (Num a) => TooMany (MyNum a) where
--  tooMany (MyNum n) = n > 42
  tooMany _ = True

-- *LogicGoats> tooMany (4 :: Int, 9 :: Int)

-- <interactive>:64:1: error:
--     * Overlapping instances for TooMany (Int, Int)
--         arising from a use of `tooMany'

-- above error is because we can't tell the difference
-- between the Int instance for tooMany and the
-- (Int, Int) instance for tooMany

instance TooMany Integer where
  tooMany n = n > 42