module Chapter7Exercises where

q3 :: (Ord a) => a -> a -> Bool
q3 = undefined

q5 :: a -> a
q5 x = x

tensDigit :: Integral a => a -> a 
tensDigit x = d 
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- tensDigit' :: Integral a => a -> a
-- tensDigit' x = snd $ divMod x 10
tensDigit' x = (fst $ divMod x 10) `mod` 10

hunsD x = (fst $ divMod x 100) `mod` 10

foldBool :: (Ord a) => a -> a -> Bool -> a
foldBool x y b = 
  case b of 
    True  -> x
    False -> y

foldBool' :: (Ord a) => a -> a -> Bool -> a 
foldBool' x y b
  | b = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
