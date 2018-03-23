module RegisteredUser where 

newtype UserName =
    UserName String

newtype AccountNumber = 
    AccountNumber Integer

data User = 
    UnregisteredUser 
    | RegisteredUser UserName AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = 
  putStrLn "UnregisteredUser"
printUser (RegisteredUser 
           (UserName name)
           (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum


    
