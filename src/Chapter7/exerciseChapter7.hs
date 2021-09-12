module ExerciseChapter7 where

tensDigit :: Integral b => b -> b
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

foldBool :: p -> p -> Bool -> p
foldBool x y t = case t of
  True -> y
  False -> x

foldBool2 :: p -> p -> Bool -> p
foldBool2 x y t
  | t = x
  | otherwise = y

g :: (t -> a) -> (t, b) -> (a, b)
g f (a, c) = (f a, c)