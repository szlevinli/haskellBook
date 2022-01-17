module ExerciseChapter11 where

import Data.Char (chr, ord, toUpper)

-- ----------------------------------------------------------------------------
-- Vigenere Cipher
--
-- 满足 ASCII 可视字符(32-126)的 Vigenere Cipher 实现
-- ----------------------------------------------------------------------------

--
-- Common function
--
type From = Int

type To = Int

type Offset = Int

type PlainText = String

type CipherText = String

type Key = String

-- 在给定的范围内进行偏移(向右)操作
-- 比如: 32-126 的范围内对给定的此范围内的数字进行偏移
--       125 偏移 8 位得到 38

rotFromTo :: From -> To -> Offset -> Int -> Int
rotFromTo from to offset input =
  (input - from + offset) `mod` (to - from + 1) + from

rotInt :: Offset -> Int -> Int
rotInt = rotFromTo 32 126

rotChar :: Int -> Char -> Char
rotChar offset = chr . rotInt offset . ord

--
-- Caesar Cipher
--

caesar :: Offset -> PlainText -> CipherText
caesar offset = map $ rotChar offset

unCaesar :: Offset -> CipherText -> PlainText
unCaesar = caesar . negate

--
-- Vigenere Cipher
--

-- zipWithSpace "ab cd e" "KEY"
-- RETURN: [('a','K'),('b','E'),(' ','Y'),('c','K'),('d','E'),(' ','Y'),('e','K')]

zipWithKey :: PlainText -> Key -> [(Char, Char)]
zipWithKey [] _ = []
zipWithKey _ [] = []
zipWithKey (a : as) (b : bs) = (a, b) : zipWithKey as (bs ++ [b])

vigenere :: Key -> PlainText -> CipherText
vigenere key plain = map encode $ zipWithKey plain key
  where
    encode (c, k) = rotChar (ord k - ord ' ') c

unVigenere :: Key -> CipherText -> PlainText
unVigenere key cipher = map decode $ zipWithKey cipher key
  where
    decode (c, k) = rotChar (negate $ ord k - ord ' ') c

-- ----------------------------------------------------------------------------
-- As-patterns
-- ----------------------------------------------------------------------------

-- 1. This should return `True` if (and only if) all the values in the first
--    list appear in the second list, though they need not be contiguous.
-- isSubseqOf "blah" "blahwoot" -> True
-- isSubseqOf "blah" "wboloath" -> True
-- isSubseqOf "blah" "halbwoot" -> False

isSubseqOf ::
  (Eq a) =>
  [a] ->
  [a] ->
  Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf aas@(a : as) (b : bs) =
  if a == b
    then isSubseqOf as bs
    else isSubseqOf aas bs

-- 2. Split a sentence into words, then tuple each word with the capitalized
--    form of each.
-- capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = map capitalize $ words s
  where
    capitalize [] = ("", "")
    capitalize aas@(a : as) = (aas, toUpper a : as)

-- Language exercises

-- 1. Write a function that capitalizes a word.

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (a : as) = toUpper a : as

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize
--    when a new sentence has begun by checking for periods.

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . capitalize . words
  where
    capitalize [] = []
    capitalize entire@(first : _) =
      capitalizeWord first : capitalizeAfterPeriod entire
    endIsPeriod [] = False
    endIsPeriod w = last w == '.'
    capitalizeAfterPeriod (first : second : tail) =
      if endIsPeriod first
        then capitalizeWord second : capitalizeAfterPeriod (second : tail)
        else second : capitalizeAfterPeriod (second : tail)
    capitalizeAfterPeriod _ = []
