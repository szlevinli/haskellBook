module Cipher where

import Data.Char (chr, ord)

lowerCaseBaseZero :: Char -> Int
lowerCaseBaseZero = subtract 97 . ord

lowerCaseMod :: Int -> Int
lowerCaseMod = flip mod 26

lowerCase :: Int -> Char
lowerCase = chr . (+ 97)

toRight' :: Int -> Char -> Char
toRight' i = lowerCase . lowerCaseMod . (+ i) . lowerCaseBaseZero

toRight :: Int -> Char -> Char
toRight i c = chr $ mod (ord c - 97 + i) 26 + 97

toLeft :: Int -> Char -> Char
toLeft i c = chr $ mod (ord c - 97 - i) 26 + 97

rotFromTo :: Int -> Int -> Int -> Int -> Int
rotFromTo from to offset input =
  (input - from + offset) `mod` (to - from) + from

rotInt :: Int -> Int -> Int
rotInt = rotFromTo 32 126

rotChar :: Int -> Char -> Char
rotChar offset = chr . rotInt offset . ord

caesar :: Int -> String -> String
caesar offset = map $ rotChar offset

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
