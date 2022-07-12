module FunctionWithLet where

printInc :: (Show a, Num a) => a -> IO ()
printInc n =
  let plusTwo = n + 2
   in print plusTwo