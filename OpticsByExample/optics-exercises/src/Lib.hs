{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
     ( someFunc
     )
where

import           Control.Lens
import           Control.Applicative
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T



data Ship =
    Ship { _name    :: String
         , _numCrew :: Int
         }
    deriving(Show)

makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship { _numCrew = newNumCrew }

--numCrew :: Lens' Ship Int
--numCrew = lens getNumCrew setNumCrew

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName ship newName = ship { _name = newName }

--name :: Lens' Ship String
--name = lens getName setName

purplePearl :: Ship
purplePearl = Ship { _name = "Purple Pearl", _numCrew = 74 }

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- makeLenses ''Ship
