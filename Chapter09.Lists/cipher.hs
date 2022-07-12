module Cipher where

import Data.Char (chr, ord)

-- ----------------------------------------------------------------------------
-- 凯撒密码 Caesar Cipher
-- 将给定的字符偏移指定的位数, 得到新的字符
-- ----------------------------------------------------------------------------

type From = Int

type To = Int

type Offset = Int

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

caesar :: Int -> String -> String
caesar offset = map $ rotChar offset

unCaesar :: Int -> String -> String
unCaesar = caesar . negate
