module UsingState where

import Control.Monad.State

-- type Stack = [Int]

-- pop :: State Stack Int
-- pop = state $ \(x:xs) -> (x,xs)

-- push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     a <- pop
--     pop

type Messages = [String]

