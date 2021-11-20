module FunctionWithWhere where

printInc :: (Show a, Num a) => a -> IO ()
printInc n = print plusTwo
  where
    plusTwo = n + 2

{-
let x = 5; y = 6 in x * y
-}
mult1 :: Integer
mult1 = x * y
  where
    x = 5
    y = 6

{-
let x = 3; y = 1000 in x * 3 + y
-}
mult2 :: Integer
mult2 = x * 3 + y
  where
    x = 3
    y = 1000

{-
let y = 10; x = 10 * 5 + y in x * 5
-}
mult3 :: Integer
mult3 = x * 5
  where
    x = 10 * 5 + y
    y = 10

mult4 :: Double
mult4 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10