module PatternMatchingTuples where

f :: (a1, a2) -> (b1, b2) -> ((a2, b2), (a1, b1))
f (a, b) (c, d) = ((b, d), (a, c))