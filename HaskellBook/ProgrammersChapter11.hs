module Programmers where

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang = 
    Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Eq, Show)

data Programmer = 
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = 
  Programmer { os = Mac 
             , lang = Haskell }
      
feelingWizardly :: Programmer
feelingWizardly = 
  Programmer { lang = Agda
             , os = GnuPlusLinux }

-- Excercise: Programmers

-- 1. Write a function that generates all possible
-- values of Programmers. Use the provided lists of
-- inhabitants of OperatingSystem and ProgLang. 

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = 
  [
    Haskell
  , Agda
  , Idris
  , Purescript
  ]

allProgrammers :: [Programmer]
allProgrammers = 
  [Programmer os lang | 
    os   <- allOperatingSystems, 
    lang <- allLanguages]

-- *Programmers> :m + Data.List
-- *Programmers Data.List> :i nub
-- nub :: Eq a => [a] -> [a]
--         -- Defined in `base-4.9.1.0:Data.OldList'
-- *Programmers Data.List> :t nub
-- nub :: Eq a => [a] -> [a]
-- *Programmers Data.List> length allProgrammers
-- 16
-- it :: Int
-- *Programmers Data.List> length $ nub allProgrammers
-- 16
-- it :: Int