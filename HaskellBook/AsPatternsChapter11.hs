module AsPatterns where

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do 
  print a
  return t

-- f ("brody", 5)
-- "brody"
-- ("brody", 5)
-- and with :set +t
-- it :: ([Char], Integer)

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- Exercises: Use as-patterns in implementing the following functions

-- 1. This should return true iff all the values in the
-- first list appear in the second list, though they need
-- not be contiguous they *do* need to be in order

isSubseqOf :: (Eq a) 
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf ax@(x:xs) (y:ys) 
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf ax ys

testMatchStart :: IO ()
testMatchStart = 
  if isSubseqOf "blah" "blahwoot"
  then putStrLn "match start: pass"
  else putStrLn "match start: fail"

testMatchEnd :: IO ()
testMatchEnd = 
  if isSubseqOf "blah" "wootblah"
  then putStrLn "match end: pass"
  else putStrLn "match end: fail"

testIntermingled :: IO ()
testIntermingled = 
  if isSubseqOf "blah" "wboloath"
  then putStrLn "match intermingled: pass"
  else putStrLn "match intermingled: fail"

testNoMatchIncomplete :: IO ()
testNoMatchIncomplete = 
  if not $ isSubseqOf "blah" "wootbla"
  then putStrLn "no match incomplete: pass"
  else putStrLn "no match incomplete: fail"

testNoMatchOutOfOrder :: IO () 
testNoMatchOutOfOrder = 
  if not $ isSubseqOf "blah" "halbwoot"
  then putStrLn "no match out of order: pass"
  else putStrLn "no match out of order: fail"

testIntermingled2 :: IO ()
testIntermingled2 = 
  if isSubseqOf "blah" "blawhoot"
  then putStrLn "match intermingled 2: pass"
  else putStrLn "match intermingled 2: fail"
   
main :: IO ()
main = do 
  testMatchStart
  testMatchEnd
  testIntermingled
  testNoMatchIncomplete
  testNoMatchOutOfOrder
  testIntermingled2