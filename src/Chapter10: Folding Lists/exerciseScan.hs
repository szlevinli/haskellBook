module ExerciseScan where

fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

-- 1. Modify your `fibs` function to only return the first 20 Fibonacci numbers.

take20 :: Num a => [a]
take20 = take 20 fibs

-- 2. Modify `fibs` to return the Fibonacci numbers that are less than 100.

lessThan100 :: (Num a, Ord a) => [a]
lessThan100 = takeWhile (< 100) fibs

-- 3. Try to write the `factorial` function from Recursion as a scan.
--    You'll want `scanl` again, and your start value will be 1.

factorial :: [Integer]
factorial = scanl (*) 1 [1 ..]