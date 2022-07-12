module GiveTypeWriteFunction where

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc x2y y2z _ (a, x) = (a, y2z (x2y x))

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r x = x

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c (a2b a)

fa :: (a -> c) -> a -> a
fa _ a = a

a' :: (a -> b) -> a -> b
a' a2b = a2b
