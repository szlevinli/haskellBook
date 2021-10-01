module Exercise1 where

k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k (4 - 1, 10)

ke :: [Char]
ke = k ("three", 1 + 2)

k3 :: Integer
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))