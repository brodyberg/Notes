module ProjectRosalind.FindingASharedMotif where

import Test.QuickCheck

--Problem
--A common substring of a collection of strings is a substring of every member of the collection. We say that a common substring is a longest common substring if there does not exist a longer common substring. For example, "CG" is a common substring of "ACGTACGT" and "AACCGTATA", but it is not as long as possible; in this case, "CGTA" is a longest common substring of "ACGTACGT" and "AACCGTATA".

--Note that the longest common substring is not necessarily unique; for a simple example, "AA" and "CC" are both longest common substrings of "AACC" and "CCAA".

--Given: A collection of k (k≤100) DNA strings of length at most 1 kbp each in FASTA format.

--Return: A longest common substring of the collection. (If multiple solutions exist, you may return any single solution.)

--Sample Dataset

-- >Rosalind_1
-- GATTACA
-- >Rosalind_2
-- TAGACCA
-- >Rosalind_3
-- ATACA

-- Sample Output
-- AC

strings = ["GATTACA", "TAGACCA", "ATACA"]

-- make it so we don't have to pass in [String] 
allSubstrings :: String -> [String] -> [String]
allSubstrings [] acc = acc
allSubstrings str acc = (substrings str) ++ (allSubstrings (tail str) [])

substrings :: String -> [String]
substrings []   = []
substrings item = ss "" item []
  where 
    ss [] [] acc = acc
    ss [] therest acc = ss [(head therest)] (tail therest) acc
    ss save [] acc = save : acc 
    ss save therest acc = ss (save ++ [(head therest)]) (tail therest) (save : acc)

results = foldr allSubstrings [] strings


-- Formula from
-- https://stackoverflow.com/questions/12418590/finding-substrings-of-a-string
prop_allPossibleSubstringCount :: String -> Bool
prop_allPossibleSubstringCount str = 
  length (allSubstrings str []) == (n * (n + 1)) `div` 2
  where n = length str

-- implement all the fasta stuff

-- 4 * (4 + 1) / 2 == 10
-- 3 * (3 + 1) / 2 == 6
-- 2 * (2 + 1) / 2 == 3
-- 1 * (1 + 1) / 2 == 1


-- getAllPossibleSubstrings g attaca
-- []

-- save what you got: g
-- pop head: a
-- cons to parameter: [g a]
-- pass g a, and rest

-- save what you got: g a
-- pass g a t, and rest

-- save what you got: g a t
-- pass g a t t, and rest

-- save what you got: g a t t
-- pass g a t t a, and rest

-- save what you got: g a t t a
-- pass g a t t a c, and rest

-- save what you got: g a t t a c
-- pass g a t t a c a, and rest

-- save what you got: g a t t a c a
-- return as rest is []

-- 


-- the string: GATTACA
-- G
-- GA
-- GAT
-- GATT
-- GATTA
-- GATTAC
-- GATTACA
--  A
--  AT
--  ATT
--  ATTA
--  ATTAC
--  ATTACA
--   T
--   TT
--   TTA
--   TTAC
--   TTACA
--    T
--    TA
--    TAC
--    TACA
--     A
--     AC
--     ACA
--      C
--      CA
--       A
