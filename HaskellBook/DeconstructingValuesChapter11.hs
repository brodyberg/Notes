module DeconstructingValues where

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType =
    DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving Show

data Farmer = 
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

-- *DeconstructingValues>f = (Farmer (Name "Brody") (Acres 10 )WheatFarmer)    )
-- f :: Farmer
-- *DeconstructingValues> f
-- Farmer (Name "Brody") (Acres 10) WheatFarmer
-- it :: Farmer

data FarmerRec = 
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = 
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False
                