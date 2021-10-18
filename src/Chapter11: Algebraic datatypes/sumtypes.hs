module SumTypes where

import Data.Int (Int8)

data NumberOrBool
  = NumNum Int8
  | BoolBool Bool
  deriving (Eq, Show)
