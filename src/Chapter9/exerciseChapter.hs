module ExerciseChapter9 where

import Data.Char (isUpper, toUpper)

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

upperAll :: String -> String
upperAll = map toUpper

capitalizeHead :: String -> Char
capitalizeHead = toUpper . head