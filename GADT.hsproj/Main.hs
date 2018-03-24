{-# LANGUAGE GADTs #-}

module Main where


import Control.Monad.State

data M a = Foo a deriving Show


type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackManip2 = 
    push 3 >>= (\s -> push 4)

stackManip3 = 
    push 3 
    >>= (\s -> pop) 
    >>= (\s -> pop)

--stackManip2 = 
--    push 3 >>= pop >>= pop

--stackManip2 = 
--    (push 3) >>= (push 4)


--
--data T a where
--  D1 :: Int -> T String
--  D2 :: T Bool
--  D3 :: (a,a) -> T [a]
--
--
--  
----
----data Either2 a b = Left2 a | Right2 b
----
----type X2 a = Either2 a a 
--
---- type F [a] = Set a 
----
--class IsSimple a
--instance IsSimple Bool 
--instance IsSimple Int
--instance IsSimple Double
--

--data Maybe2 a = Just2 a | Nothing2
--
--data Maybe3 a = Just3 a
--     Maybe3 a = Nothing
--     


main = do putStrLn "foo"