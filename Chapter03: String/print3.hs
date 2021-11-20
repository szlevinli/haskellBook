module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " Haskell!"

hello :: String
hello = "hello"

word :: String
word = "Haskell"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting =
      concat [hello, " ", word]
