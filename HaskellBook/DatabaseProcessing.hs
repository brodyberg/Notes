module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime 
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 7 4)
            (secondsToDiffTime 34123))
  , DbNumber 891]

-- 1. filters for DbDate values and returns a 
-- list of UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDate []
  where 
    filterDate (DbDate x) acc = x : acc
    filterDate _          acc = acc 

-- 2. filters for DbNumber values and returns a
-- list of Integer values inside them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumber []
  where
    filterNumber (DbNumber n) acc = n : acc
    filterNumber _            acc = acc

-- 3. Gets the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr max minDate (filterDbDate db)
  where minDate = (UTCTime 
                    (fromGregorian 0 0 0) 
                    (secondsToDiffTime 34123))

-- 4. Sums all the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)

-- 5. Gets average of the DbNumber values
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumDb db)) / fromIntegral (length $ filterDbNumber db)