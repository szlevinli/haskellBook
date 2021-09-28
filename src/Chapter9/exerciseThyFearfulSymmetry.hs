module ExerciseThyFearfulSymmetry where

myWords :: String -> [String]
myWords [] = []
myWords str =
  takeWhile (/= ' ') str :
  myWords (dropWhile (== ' ') (dropWhile (/= ' ') str))

-- 网上的答案
-- myWords :: String -> [String]
-- myWords [] = []
-- myWords str =
--   if null str
--     then []
--     else word : myWords rest
--   where
--     isSpace = flip elem " "
--     trim = dropWhile isSpace str
--     word = takeWhile (not . isSpace) trim
--     rest = dropWhile (not . isSpace) trim

firstSen :: [Char]
firstSen = "T T, burning bright\n"

secondSen :: [Char]
secondSen = "In the forest of the night\n"

thirdSen :: [Char]
thirdSen = "What immortal hand or eye\n"

fourthSen :: [Char]
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines str
  | null str = []
  | otherwise =
    takeWhile (/= '\n') str :
    myLines (dropWhile (== '\n') (dropWhile (/= '\n') str))

shouldEqual :: [[Char]]
shouldEqual =
  [ "T T, burning bright",
    "In the forest of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy c str =
  takeWhile (/= c) str :
  myLines (dropWhile (== c) (dropWhile (/= c) str))

main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
            == shouldEqual
        )