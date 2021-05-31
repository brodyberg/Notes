module EftChapter9 where

eftBool :: Bool -> Bool -> [Bool]
eftBool True  True  = [True]
eftBool True  False = []
eftBool False True  = [False, True]
eftBool False False = [False]

eftOrdering :: Ordering
            -> Ordering
            -> [Ordering]
eftOrdering start end = 
  case compare start end of
    LT -> start : eftOrd (succ start) end
    EQ -> [end]
    GT -> []

eftInt :: Int -> Int -> [Int]
eftInt start end =
  case compare start end of 
    LT -> start : eftInt (succ start) end
    EQ -> [end]
    GT -> []

eftChar :: Char -> Char -> [Char]
eftChar start end = 
  case compare start end of
    LT -> start : eftChar (succ start) end
    EQ -> [end]
    GT -> []

eftOrd :: (Ord a, Enum a) => a -> a -> [a]
eftOrd start end = 
  case compare start end of
    LT -> start : eftOrd (succ start) end
    EQ -> [end]
    GT -> []
