# Building projects

## 13.1 Modules

## 13.2 Making packages with Stack

## 13.3 Working with a basic project

## 13.4 Making our project a library

## 13.5 Module exports

## 13.6 More on importing modules

## 13.7 Making our program interactive

## 13.8 do syntax and IO

## 13.9 Hangman game

```haskell
module Main where

-------------------------------------------------------------------------------
-- Step One: Importing modules
-------------------------------------------------------------------------------
import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-------------------------------------------------------------------------------
-- Step two: Generating a word list
-------------------------------------------------------------------------------
-- type WordList = [String]
newtype WordList
  = WordList [String]
  deriving (Eq, Show)

-- Reads data from a text file, separated by lines, and returns an array of
-- strings, where each element in the array is a line of data in the file.
--
-- readFile :: FilePath -> IO String
-- FilePath is `type FilePath = String`
-- dict :: String
-- lines :: String -> [String]
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- Filter out words of specified length according to the
-- upper and lower bounds.
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength :: [Char] -> Bool
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength
            && l < maxWordLength

-- Returns a random string by index from an array of strings.
--
-- randomRIO ::
--   (System.Random.Random a, Control.Monad.IO.Class.MonadIO m) =>
--   (a, a) -> m a
-- (!!) :: [a] -> Int -> a
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- @String: the word we're trying to guess
-- @[Maybe Char]: the characters we've filled in so far
-- @[Char]: the letters we're guessed so far
data Puzzle
  = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $
      fmap renderPuzzleChar discovered
        ++ " Guessed so far: "
        ++ guessed

-- First we're going to write function that will take our puzzle word
-- and turn it into a list of `Nothing`
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

-- Now we need a function that looks at the `Puzzle String` and determines
-- whether the character you guessed is an element of that string.
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) = flip elem s

-- The next function is very similar to the one you just wrote, but
-- this time we don't care if the `Char` is part of the `String` argument
-- this time we want to check and see if it is an element of the `guessed`
-- list.
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) = flip elem s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  --                    [1]                  [2]
  Puzzle word newFilledInSoFar (c : s)
  where
    --         [    3    ]

    zipper :: Char -> Char -> Maybe Char -> Maybe Char
    zipper guessed wordChar guessChar =
      --[4]  [5]     [6]       [7]
      if wordChar == guessed
        then Just wordChar
        else guessChar
    --      [   8   ]
    newFilledInSoFar :: [Maybe Char]
    newFilledInSoFar =
      -- [  9  ]
      zipWith
        (zipper c)
        word
        filledInSoFar

--        [  10  ]

-- 1. The first argument is out `Puzzle` with its three arguments, with
--    `s` representing the list of characters already guessed.
-- 2. The `c` is our `Char` argument and is the character the player
--    guessed on this turn.
-- 3. Our result is the `Puzzle` with the `filledInSoFar` replaced by
--    `newFilledInSoFar` the `c` consed onto the front of the `s` list.
-- 4. `zipper` is a combining function for deciding how to handle the
--    character in the word, what's been guessed already, and the
--    character that was just guessed. If the current character in the
--    word is equal to what the player guessed, the we go ahead and
--    return `Just wordChar` to fill in that spot in the puzzle. Otherwise,
--    we kick the `guessChar` back out. We kick `guessChar` back out
--    because it might either be a previously correctly guessed character
--    or a `Nothing` that has not been guessed correctly this time nor in
--    the past.
-- 5. `guessed` is the character they guessed.
-- 6. `wordChar` is the character in the puzzle word -- not the ones
--    they're guessed or not guessed, but the characters in the word
--    that they're supposed to be guessing.
-- 7. `guessChar` is the list that keeps track of the characters the player
--    has guessed so far.
-- 8. This `if-then-else` expression checks to see if the guessed character
--    is one of the word characters. If it is, it wrap it in a `Just`
--    because our puzzle word is a list of `Maybe` values.
-- 9. `newFilledInSoFar` is the new state of the puzzle which uses `zipWith`
--    and the `zipper` combining function to fill in characters in the
--    puzzle. The `zipper` function is first applied to the character the
--    player just guessed because that doesn't change. Then it's zipped
--    across two lists. One list is `word` which is the word the user is
--    trying to guess. The second list , `filledInSoFar` is the puzzle state
--    we're starting with of type `[Maybe Char]`. That's telling us which
--    characters in `word` have been guessed.
-- 10. Now we're going to make our `newFilledInSoFar` by using `zipWith`.
--     You may remember this from the Lists chapter. It's going to zip
--     the `word` with the `filledInSoFar` values while applying the `zipper`
--     function from just above it to the values as it does.

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess,
         alreadyGuessed puzzle guess
       ) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

-- all :: Foldable t => (a -> Bool) -> t a -> Bool
-- isJust :: Maybe a -> Bool
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter:"
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ ->
      putStrLn
        "Your guess must\
        \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
```
