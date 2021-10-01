module TypeKwonDo where

-- data Woot

-- data Blah

-- f :: Woot -> Blah
-- f = undefined

-- g :: (Blah, Woot) -> (Blah, Blah)
-- g (b, w) = (b, f w)

-- Delimiter ------------------------------------------------------------------

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- Delimiter ------------------------------------------------------------------

data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- Delimiter ------------------------------------------------------------------

data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xForm :: (X, Y) -> (Z, Z)
xForm (x, y) = (xz x, yz y)

-- Delimiter ------------------------------------------------------------------

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge x2y y2xz x = fst (y2xz (x2y x))
