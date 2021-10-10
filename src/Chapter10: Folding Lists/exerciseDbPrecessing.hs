module ExerciseDbProcessing where

import Data.List (maximum)
import Data.Time
  ( UTCTime (UTCTime),
    fromGregorian,
    secondsToDiffTime,
  )

{-
UTCTime :: Day -> DiffTime -> UTCTime
fromGregorian :: Integer -> Int -> Int -> Day
secondsToDiffTime :: Integer -> DiffTime
-}

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for `DbDate` values and return a list
--    of the `UTCTime` values inside them.

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr concatDates []
  where
    concatDates :: DatabaseItem -> [UTCTime] -> [UTCTime]
    concatDates (DbDate t) ts = t : ts
    concatDates _ ts = ts

-- 2. Write a function that filters for `DbNumber` values and returns a list
--    of the `Integer` values inside them.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr concatNumbers []
  where
    concatNumbers (DbNumber i) is = i : is
    concatNumbers _ is = is

-- 3. Write a function that gets the most recent date.

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the `DbNumber` values.

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNumbers 0
  where
    sumNumbers (DbNumber i) sn = i + sn
    sumNumbers _ sn = sn

-- 5. Write a function that gets the average of the `DbNumber` values.

avgDb :: [DatabaseItem] -> Double
avgDb dbItem =
  fromInteger (sumDb dbItem)
    / fromIntegral (length $ filterDbNumber dbItem)
