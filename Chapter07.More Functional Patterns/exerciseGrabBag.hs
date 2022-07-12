module ExerciseGrabBag where

mth1 :: Num a => a -> a -> a -> a
mth1 x y z = x * y * z

mth2 :: Num a => a -> a -> a -> a
mth2 x y = \z -> x * y * z

mth3 :: Num a => a -> a -> a -> a
mth3 x = \y -> \z -> x * y * z

mth4 :: Num a => a -> a -> a -> a
mth4 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mFlip f x y = f y x