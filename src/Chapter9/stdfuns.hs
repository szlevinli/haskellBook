module StdFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (x ==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish [[]] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

-- rewrite `squish` use `squishMap`
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy ::
  (a -> a -> Ordering) ->
  [a] ->
  a
myMaximumBy _ [] = error "empty list."
myMaximumBy _ [x] = x
myMaximumBy f (x : xs) = case f x mxs of
  LT -> mxs
  _ -> x
  where
    mxs = myMaximumBy f xs

{-
myMaximumBy f [1, 53, 9001, 10]
case f 1 (myMaximumBy f [53, 9001, 10])
case f 1 (case f 53 (myMaximumBy f [9001, 10]))
case f 1 (case f 53 (case f 9001 (myMaximumBy f [10])))
case f 1 (case f 53 (case f 9001 10))
case f 1 (case f 53 9001)
case f 1 9001
9001
 -}

myMinimumBy ::
  (a -> a -> Ordering) ->
  [a] ->
  a
myMinimumBy _ [] = error "empty list."
myMinimumBy _ [x] = x
myMinimumBy f (x : xs) = case f x mns of
  LT -> x
  _ -> mns
  where
    mns = myMinimumBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare