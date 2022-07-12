module ExerciseChapter15 where

-- Write the `Monoid` instance for our `Maybe` type renamed to `Optional`.

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance
  Semigroup a =>
  Semigroup (Optional a)
  where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only b) = Only $ a <> b
  Nada <> Nada = Nada

instance
  Monoid a =>
  Monoid (Optional a)
  where
  mempty = Nada
  mappend = (<>)
