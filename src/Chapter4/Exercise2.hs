x :: Int -> Int -> Int
x = (+)

f :: Foldable t => t a -> Int
f xs = w `x` 1
  where
    w = length xs
