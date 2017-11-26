module WorkDataKinds where

data JobDescription = JobOne { n :: Int }
                    | JobTwo
                    | JobThree { n :: Int }
  deriving (Show, Eq)

taskOneWorker :: JobDescription -> IO ()
taskOneWorker t = do
  putStrLn $ "n: " ++ (show $ n t)

main :: IO ()
main = do 
  taskOneWorker (JobOne 10)

--  taskOneWorker JobTwo

  taskOneWorker (JobThree 10)