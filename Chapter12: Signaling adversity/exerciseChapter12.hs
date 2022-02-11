module ExerciseChapter12 where

import Data.Maybe (fromMaybe)

-- Determine the kinds
-- 1. Give
--    id :: a -> a
-- What is the kind of a?
-- a :: *

-- 2. r :: a -> f a
-- What are the kinds of a and f?
-- a, f a :: *
-- f :: * -> *

-------------------------------------------------------------------------------
-- String processing
-------------------------------------------------------------------------------
-- 1. Write a recursive function named `replaceThe` which takes a text/string,
--    breaks it into words and replaces each instance of "the" with "a".
--    It's intended only to replace exactly the word "the".
--    `notThe` is a suggested helper function for accomplishing this.

-- > notThe "the"
-- Nothing
-- > notThe "ThisTheMoon"
-- Just "ThisTheMoon"
-- > notThe "wow"
-- Just "wow"

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

-- > replaceThe "the cow loves us"
-- "a cow loves us"

replaceThe :: String -> String
replaceThe = unwords . go . words
  where
    go :: [String] -> [String]
    go [] = []
    go (w : ws) =
      case notThe w of
        Just w -> w : go ws
        Nothing -> "a" : go ws

replaceThe' :: String -> String
replaceThe' = unwords . map (fromMaybe "a" . notThe) . words

-- 2. Write a recursive function that takes a text/string, breaks it into
--    words, and counts the number of instance of "the" follow by
--    a vowel-initial word.

-- > countTheBeforeVowel "the cow"
-- 0
-- > countTheBeforeVowel "the evil cow"
-- 1

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go (wa : wb : ws) =
      case notThe wa of
        Just _ -> go (wb : ws)
        Nothing ->
          if head wb `elem` "aeiou"
            then 1 + go (wb : ws)
            else go (wb : ws)
    go _ = 0

countTheBeforeVowel' :: String -> Integer
countTheBeforeVowel' = go . words
  where
    go :: [String] -> Integer
    go (x : x1 : xs)
      | x == "the" && isVowel x1 = 1 + go (x1 : xs)
      | otherwise = go (x1 : xs)
    go _ = 0
    isVowel :: String -> Bool
    isVowel = flip elem "aeiou" . head

-- 3. Return the number of letters that are vowels in a word.
--    Hint: it's helpful to break this into steps. Add any helper functions
--    necessary to achieve your objectives.
--    a) Test for vowelhood
--    b) Return the vowels of a string
--    c) Count the number of element returned
-- > countVowels "the cow"
-- 2
-- > countVowels "Mikolajczak"
-- 4

countVowels :: String -> Integer
countVowels [] = 0
countVowels (x : xs)
  | x `elem` "aeiou" = 1 + countVowels xs
  | otherwise = countVowels xs

-------------------------------------------------------------------------------
-- Validate the word
-- Use the `Maybe` type to write a function that counts the number of
-- vowels in a string and the number of consonants. If the number
-- of vowels exceeds the number of consonants, the function returns
-- `Nothing`. In many human languages, vowels rarely exceed the number
-- of consonants so when they do, it may indicate the input isn't a word
-- (that is, a valid input to your dataset):
-------------------------------------------------------------------------------

newtype Word'
  = Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

-- I wrote this myself
mkWord :: String -> Maybe Word'
mkWord s = go . count $ s
  where
    count :: String -> Int
    count [] = 0
    count (x : xs)
      | x `elem` vowels = (-1) + count xs
      | otherwise = 1 + count xs
    go :: Int -> Maybe Word'
    go n = if n <= 0 then Nothing else Just $ Word' s

-- From Haskell book
-- help functions
isVowels :: Char -> Bool
isVowels c = c `elem` vowels

countVowels' :: String -> Integer
countVowels' = fromIntegral . length . filter isVowels

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (not . isVowels)

mkWord' :: String -> Maybe Word'
mkWord' s
  | countVowels' s > countConsonants s = Nothing
  | otherwise = Just $ Word' s

-------------------------------------------------------------------------------
-- It's only Natural
-- You'll be presented with a datatype to represent the natural numbers.
-- The only values representable with the naturals are whole numbers
-- from zero to infinity. Your task will be to implement functions to
-- convert `Naturals` to `Integers` and `Integers` to `Naturals`.
-- The conversion from `Naturals` to `Integers` won't return `Maybe`
-- because `Integer` is a strict superset of `Natural`. Any `Natural` can be
-- represented by an `Integer`, but the same is not true of any `Integer`.
-- Negative numbers are not valid natural numbers.
-------------------------------------------------------------------------------

-- As natural as any
-- competitive bodybuilder
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

-- > natToInteger Zero
-- 0
-- > natToInteger (Succ Zero)
-- 1
-- > natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

-- > integerToNat 0
-- Just Zero
-- > integerToNat 1
-- Just (Succ Zero)
-- > integerToNat 2
-- Just (Succ (Succ Zero))
-- > integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n >= 0 = Just $ go n
  | otherwise = Nothing
  where
    go 0 = Zero
    go n = Succ (go $ n - 1)