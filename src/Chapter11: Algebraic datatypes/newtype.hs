{-# LANGUAGE FlexibleInstances #-}

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n, m) = tooMany (n + m)