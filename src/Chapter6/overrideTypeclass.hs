class Ord a => MyOrd a where
  compare :: a -> a -> Int

newtype Age = Age Int deriving (Show)

instance Eq Age where
  (==) (Age n) (Age n') = n == n'

instance Ord Age where
  compare (Age n) (Age n')
    | n == n' = EQ
    | n > n' = GT
    | otherwise = LT

instance MyOrd Age where
  compare (Age n) (Age n')
    | n == n' = 0
    | n > n' = 1
    | otherwise = -1