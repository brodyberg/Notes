module RegisteredUser where

newtype UserName =
  UserName String

newtype AccountNumber = 
  AccountNumber Integer

data User = 
    Unregistereduser 
  | RegisteredUser UserName AccountNumber