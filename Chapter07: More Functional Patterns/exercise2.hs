module Exercise2 where

functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

trip :: Integer -> Integer
trip = \x -> x * 3

func :: (Eq a, Num a) => a -> [Char]
func x =
  case x + 1 == 1 of
    True -> "T"
    False -> "F"

isItTwo :: (Eq a, Num a) => a -> Bool
isItTwo 2 = True
isItTwo _ = False