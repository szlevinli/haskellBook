module ExerciseChapter8 where

import Data.List (intercalate)

sum' :: (Eq t, Num t) => t -> t
sum' n
  | n == 0 = n
  | otherwise = n + sum' (n - 1)

mult :: Integral a => a -> a -> a
mult x y = go x y 0
  where
    go x' y' r
      | y' == 0 = r
      | otherwise = go x' (y' - 1) (r + x')

type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

type Remainder = Integer

data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

-- 实现除法 (完整版)
-- 10 / 2 = 5
-- 10 / -2 = -5
-- -10 / 2 = -5
-- -10 / -2 = 5
dividedBy' :: Numerator -> Denominator -> DividedResult
dividedBy' num denom = go num denom 0
  where
    go n d c
      | d == 0 = DividedByZero
      | d < 0 = case dividedBy' n (- d) of
        DividedByZero -> DividedByZero
        Result r -> Result (- r)
      | n < 0 = case dividedBy' (- n) d of
        DividedByZero -> DividedByZero
        Result r -> Result (- r)
      | n < d = Result c
      | otherwise = go (n - d) d (c + 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

{-
00. mc91 95
01. mc91 (mc91 106)
02. mc91 96
03. mc91 (mc91 107)
04. mc91 97
05. mc91 (mc91 108)
06. mc91 98
07. mc91 (mc91 109)
08. mc91 99
09. mc91 (mc91 110)
10. mc91 100
11. mc91 (mc91 111)
12. mc91 101
13. 91
 -}

{-
01. mc91 87
02. mc91 (mc91 98)
03. mc91 (mc91 (mc91 109))
04. mc91 (mc91 99)
...
 -}

digitToWord :: Int -> String
digitToWord i
  | i == 0 = "zero"
  | i == 1 = "one"
  | i == 2 = "two"
  | i == 3 = "three"
  | i == 4 = "four"
  | i == 5 = "five"
  | i == 6 = "six"
  | i == 7 = "seven"
  | i == 8 = "eight"
  | i == 9 = "nine"
  | otherwise = ""

digits :: Int -> [Int]
digits i = go i []
  where
    go :: Int -> [Int] -> [Int]
    go d r
      | d < 10 = d : r
      | otherwise = go (d `div` 10) (d `mod` 10 : r)

wordNumber :: Int -> String
wordNumber =
  intercalate "-" . map digitToWord . digits