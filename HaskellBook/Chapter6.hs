module Chapter6Exercises where

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- printPerson (Person False)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                 then Blah
                 else x
    
-- settleDown Blah
-- settleDown Woot

type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object 
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"