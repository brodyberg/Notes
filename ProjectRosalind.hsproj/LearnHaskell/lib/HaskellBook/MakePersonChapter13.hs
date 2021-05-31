module MakePerson where

type Name = String
type Age = Int

data Person = Person Name Age deriving Show 

data PersonInvalid = 
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age 
  | name /= "" && age > 0 = 
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = 
      Left $ PersonInvalidUnknown $ 
        "Name was: " ++ show name ++
        " Age was: " ++ show age
    
-- prompt the user
-- need a read function for Age
-- attempt to construct a Person
-- if it works: "Yay! Successfully got a person: " {person}
-- if it failed: "Report an error occurred and print the error"

gimmePerson :: IO ()
gimmePerson = do --forever $ do
  putStr "Enter a name: "
  name <- getLine
  putStr "Enter an age: "
  ageRaw <- getLine
  let age = read ageRaw :: Int

  case mkPerson name age of
    Right p -> putStrLn $ "Yay! Created: " ++ show p
    Left e -> putStrLn $ "Failed: " ++ show e 