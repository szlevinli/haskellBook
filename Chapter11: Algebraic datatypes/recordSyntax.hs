module RecordSyntax where

data Car = Car
  { make :: String,
    model :: String,
    year :: Integer
  }
  deriving (Eq, Show)

data Automobile
  = Null
  | Automobile Car
  deriving (Eq, Show)

-- Don't usage
data Automobile'
  = Null'
  | Car'
      { make' :: String,
        model' :: String,
        year' :: Integer
      }
  deriving (Eq, Show)

car1 :: Car
car1 = Car {make = "bmw", model = "x3", year = 2021}

automobile' :: Automobile'
automobile' = Car' {make' = "bmw", model' = "x3", year' = 2021}
