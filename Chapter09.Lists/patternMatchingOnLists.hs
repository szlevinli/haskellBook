module PatternMatchingOnLists where

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x : _) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail [x] = Nothing
myTail (_ : xs) = Just xs
