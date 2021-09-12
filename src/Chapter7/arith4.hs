module Arith4 where

roundTrip :: (Read a, Show a) => a -> a
roundTrip a = read (show a)

roundTripTp :: (Read a, Show a) => a -> a
roundTripTp = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)
  print . roundTripTp $ 4
  print (roundTrip2 4 :: Int)