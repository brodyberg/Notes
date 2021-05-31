module SymmetryChapter9 where

isSpace :: Char -> Bool
isSpace c = c == ' '

isNotSpace :: Char -> Bool
isNotSpace c = not $ isSpace c

value = "sheryl wants fun"

myWords :: [Char] -> [[Char]]
myWords x = [first, second, third]
  where
    first = "wallfish"
    part1 = dropWhile (/= ' ') x
    part2 = dropWhile (== ' ') part1
    second = takeWhile (/= ' ') part2
    part3 = dropWhile (/= ' ') part2
    third = dropWhile (== ' ') part3

myWords' :: Char -> [Char] -> [[Char]]
myWords' c x = [first, second, third]
  where
    first = "wallfish"
    part1 = dropWhile (/= c) x
    part2 = dropWhile (== c) part1
    second = takeWhile (/= c) part2
    part3 = dropWhile (/= c) part2
    third = dropWhile (== c) part3
    -- myWords :: [Char] -> [[Char]]
-- myWords x = [first, "second", "third"]
--   where
--     first = "wallfish"
    -- part1 = dropWhile isNotSpace x
    -- part2 = dropWhile isSpace second
    -- second = takeWhile isSpace part2
    -- part3 = dropWhile isNotSpace part2
    -- third = dropWhile isSpace part3

-- myWords :: [Char] -> [[Char]]
-- myWords x = [first, second, third]
--   where
--     first = "wallfish"
--     part1 = dropWhile (/= ' ') x
--     part2 = dropWhile (== ' ') second
--     second = takeWhile (/= ' ') part2
--     part3 = dropWhile (/= ' ') part2
--     third = dropWhile (== ' ') part3
