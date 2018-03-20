module WordNumberTest where

import Test.Hspec
import WordNumber 
  (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do 
  describe "digitToWord" 