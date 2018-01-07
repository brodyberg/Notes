module TypeDistributiveProperty where

-- data Fiction = FictionXX deriving Show
-- data Nonfiction = NonfictionXX deriving Show

-- data BookType = 
--     FictionBook Fiction
--   | NonfictionBook Nonfiction
--   deriving Show

-- type AuthorName = String

-- -- this is a type, but it isn't in normal-form
-- data Author = Author (AuthorName, BookType)

type AuthorName = String

data Author = 
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

-- This is: 
-- a is Author
-- b is FictionBook
-- c is NonfictionBook
--
-- so we can rewrite:           
-- data Author = (AuthorName, BookType)  -- original line 14
-- data Author = (a         * (b + c))   -- replace with symbols
-- data Author = (a * (b + c))           -- format
-- data Author = (ab + ac)               -- distribute a
-- data Author = (ab | ac)               -- rewrite + 
-- data Author =                         -- rewrite parens for binary product type
--    ab
--  | ac
-- data Author =                         -- replace with names
--    AuthorName Fiction
--  | AuthorName NonFiction
-- data Author =                         -- ?
--    Fiction AuthorName                 -- Flip so data constructor leads 
--  | Nonfiction AuthorName              -- rather than type alias which is 
                                         -- not right here
-- or maybe we can explain the ab flip to ba 
-- in that we're moving to normal form which is 
-- "sum of products." What this means is that we
-- need a data constructor in the leftmost spot
-- and ... multiplication is commutative anyway
-- so maybe we can flip a and b around since we 
-- know b is going to be the name of a data 
-- constructor while a is what that data constructor
-- will contain *and* is also not a valid data 
-- constructor within the context of AuthorName. 