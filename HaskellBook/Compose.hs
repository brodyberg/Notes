module Compose where

thing1 = take 5 . enumFrom $ 3

thing2 = take 5 . filter odd . enumFrom $ 3

