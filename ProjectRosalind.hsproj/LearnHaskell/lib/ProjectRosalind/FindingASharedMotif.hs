module ProjectRosalind.FindingASharedMotif where

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

-- how is this problem not: 
-- create lookup
-- read all possible substrings of each row
-- each possible substring becomes key in lookup 
-- every time a substring key is read into the lookup the value is incremented
-- after all rows are read any value === row count is our longest common substring

-- so even substrings in the string itself that rhyme will trigger
-- the above algorithm - disallow these
-- how would you know any particular substring was added by this row?
-- if you say: row, give me all possible substrings which fold into the unique substrings
-- then when we add all these into lookup the count would be ok since there are no duplicates
-- also, we could use a trie since the number of heads is the size of the alphabet

ssWrapper :: String -> [String] -> [String]
ssWrapper [] acc = acc
ssWrapper s  acc = (ss "" s []) ++ (ssWrapper (tail s) [])

ss :: String 
   -> String 
   -> [String] 
   -> [String]
ss [] [] found = found
ss [] therest found = ss [(head therest)] (tail therest) []
ss save [] found = save : found 
ss save therest found = ss (save ++ [(head therest)]) (tail therest) (save : found)

-- implement all the fasta stuff


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
