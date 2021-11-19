module DeconstructingValue where

newtype Name = Name String deriving (Show)

newtype Acres = Acres Int deriving (Show)

-- FarmerType is Sum
data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Eq, Show)

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer
  = Farmer Name Acres FarmerType
  deriving (Show)

-- Now we're going to write a very basic function that breaks down
-- and unpacks the data inside our constructors

-- write by `pattern match`
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- write by `guards`
isDairyFarmer' :: Farmer -> Bool
isDairyFarmer' (Farmer _ _ x)
  | x == DairyFarmer = True
  | otherwise = False

-- FarmerRec is Record
data FarmerRec = FarmerRec
  { name :: Name,
    acres :: Acres,
    farmerType :: FarmerType
  }
  deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False
