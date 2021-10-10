module ExerciseChapter10 where

import Data.Bool (bool)

-- 1. Given the following sets of consonants (辅音字母) and vowels (元音字母):
stop :: [Char]
stop = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

--    a) Write a function that takes inputs from `stops` and `vowels` and makes
--       3-tuples of all possible stop-vowel-stop combinations.

threeT :: String -> String -> [(Char, Char, Char)]
threeT ss vs = [(s, v, s1) | s <- ss, v <- vs, s1 <- ss]

--    b) Modify that function so that it only returns the combinations that
--       begin with a `p`.

justP :: String -> String -> [(Char, Char, Char)]
justP ss = filter (\(x, _, _) -> x == 'p') . threeT ss

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (==) a) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny (a ==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x y -> bool y (x : y) (f x)) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f =
  head
    . foldr
      ( \a b -> case b of
          [] -> [a]
          (b : bs) -> if f a b == GT then [a] else [b]
      )
      []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f as =
  case as of
    [] -> undefined
    a : as -> foldl (\a b -> if f a b == GT then a else b) a as

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f as =
  case as of
    [] -> undefined
    a : as -> foldl (\a b -> if f a b == LT then a else b) a as

-- Refactor `myMaximumBy` and `myMinimumBy`

maxOrMinBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
maxOrMinBy o f as =
  case as of
    [] -> undefined
    a : as -> foldl (\a b -> if f a b == o then a else b) a as

myMaximumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy2 = maxOrMinBy GT

myMinimumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy2 = maxOrMinBy LT
