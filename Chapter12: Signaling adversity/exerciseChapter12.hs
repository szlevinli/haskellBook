module ExerciseChapter12 where

import Data.List (unfoldr)
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

-------------------------------------------------------------------------------
-- Small library for Maybe
-- Write the following functions. This may take some time.
-------------------------------------------------------------------------------

-- 1. Simple boolean checks for `Maybe` values.
-- > isJust (Just 1)
-- True
-- > isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- > isNothing (Just 1)
-- False
-- > isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2. The following is the `Maybe` catamorphism. You can turn a `Maybe`
--    value into anything else with this.
-- > mayybee 0 (+1) Nothing
-- 0
-- > mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

-- 3. In case you just want to provide a fallback value.
-- > formMaybe 0 Nothing
-- 0
-- > fromMaybe 0 (Just 1)
-- 1
fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just a) = a
fromMaybe' a Nothing = a

-- 4. Converting between `List` and `Maybe`.
-- > listToMaybe [1, 2, 3]
-- Just 1
-- > listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

-- > maybeToList (Just 1)
-- [1]
-- > MaybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 5. For when we want to drop the `Nothing` values from our list.
-- > catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- > let xs = take 3 $ repeat Nothing
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just a : xs) = a : catMaybes xs

-- 6. You'll see this called "sequence" later.
-- > flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- > flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : _) = Nothing
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- flipMaybe (Just a : xs) = (a :) <$> flipMaybe xs
flipMaybe (Just a : xs) = fmap (a :) (flipMaybe xs)

-------------------------------------------------------------------------------
-- Small library for Either
-- Write each of the following functions. If more than one possible
-- unique function exists for the type, use common sense to determine
-- what it should do.
-------------------------------------------------------------------------------

-- 1. Try to eventually arrive at a solution that use `foldr`, even if
--    earlier version don't use `foldr`.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) as = a : as
    f (Right _) as = as

-- 2. Same as the last one. Use `foldr` eventually.
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left _) bs = bs
    f (Right b) bs = b : bs

-- 3.
partitionEithers' ::
  [Either a b] ->
  ([a], [b])
-- partitionEithers' es = (lefts' es, rights' es)
partitionEithers' = foldr f ([], [])
  where
    f (Left a) (as, bs) = (a : as, bs)
    f (Right b) (as, bs) = (as, b : bs)

-- 4.
eitherMaybe' ::
  (b -> c) ->
  Either a b ->
  Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' f (Left _) = Nothing

-- 5. This is a general catamorphism for `Either` values.
either' ::
  (a -> c) ->
  (b -> c) ->
  Either a b ->
  c
either' fa _ (Left a) = fa a
either' _ fb (Right b) = fb b

-- 6. Same as before, but use the `either'` function you just wrote.
eitherMaybe'' ::
  (b -> c) ->
  Either a b ->
  Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-------------------------------------------------------------------------------
-- Unfolds
-- While the idea of catamorphisms is still relatively fresh in our minds,
-- let's turn our attention to their dual: anamorphisms. If folds, or
-- catamorphisms, let us break data structures down then unfolds let us
-- build them up. There are, as with folds, a few different way to unfold
-- a data structure. We can use them to create finite and infinite data
-- structure alike.
-------------------------------------------------------------------------------

-- iterate is like a limited
-- unfold that never ends
-- > :t iterate
-- iterate :: (a -> a) -> a -> [a]

-- because it never ends, we must use
-- take to get a finite list
-- > take 10 $ iterate (+1) 0
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-- unfold is more general
-- > :t unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- Using unfoldr to do
-- the same thing as iterate
-- > take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

-------------------------------------------------------------------------------
-- Write your own iterate and unfoldr
-------------------------------------------------------------------------------

-- 1. Write the function `myIterate` using direct recursion. Compare
--    the behavior with the built-in `iterate` to gauge correctness.
--    Do not look at the source or any examples of `iterate` so that you
--    are forced to do this yourself.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2. Write the function `myUnfoldr` using direct recursion. Compare
--    with the built-in `unfoldr` to check your implementation. Again,
--    don't look at implementations of `unfoldr` so that you figure it
--    out yourself.
myUnfoldr ::
  (b -> Maybe (a, b)) ->
  b ->
  [a]
myUnfoldr f b =
  case f b of
    Just (b, b') -> b : myUnfoldr f b'
    Nothing -> []

-- 3. Rewrite `myIterate` into `betterIterate` using `myUnfoldr`.
--    A hint -- we used `unfoldr` to produce the same results as `iterate`
--    earlier. Do this with different functions and see if you can abstract
--    the structure out.
betterIterate ::
  (a -> a) ->
  a ->
  [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-------------------------------------------------------------------------------
-- Finally something other than a list!
-- Given the `BinaryTree` from last chapter, complete the following
-- exercises.
-------------------------------------------------------------------------------
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write `unfold` for `BinaryTree`.
unfold ::
  (a -> Maybe (a, b, a)) ->
  a ->
  BinaryTree b
unfold f a =
  case f a of
    Just (l, n, r) -> Node (unfold f l) n (unfold f r)
    Nothing -> Leaf

-- 2. Make a tree builder
--    Using the `unfold` function you're made for `BinaryTree`, write the
--    following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (go n) 0
  where
    go n i
      | n > i = Just (i + 1, i, i + 1)
      | otherwise = Nothing