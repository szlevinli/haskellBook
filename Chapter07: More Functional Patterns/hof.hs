module Hof where

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $
    show e
      ++ " is the boss of "
      ++ show e'

employeeRand ::
  (Employee -> Employee -> Ordering) ->
  Employee ->
  Employee ->
  IO ()
employeeRand f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ ->
      putStrLn
        "Neither employee\
        \ is the boss"
    LT -> reportBoss e' e