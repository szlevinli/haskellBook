module ExpandShowFold where

showFold :: Foldable t => p1 -> p2 -> t [a] -> [a]
showFold x y = concat

showFoldAdd :: String -> String -> String
showFoldAdd x y = showFold x y ["(", x, "+", y, ")"]

showFoldConst :: [Char] -> [Char] -> [Char]
showFoldConst x y = showFold x y ["(", x, "const", y, ")"]