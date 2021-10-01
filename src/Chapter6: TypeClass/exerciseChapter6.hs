module ExerciseChapter6 where

import Data.List (sort)

newtype Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood = Foo | Bar deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Foo then Bar else Foo

settleDown2 :: Mood -> Mood
settleDown2 Foo = Bar
settleDown2 _ = Foo

settleDown3 :: Mood -> Mood
settleDown3 x
  | x == Foo = Bar
  | otherwise = Foo

type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "A" "B"

s2 :: Sentence
s2 = Sentence "1" "2" "3"

newtype Rocks = Rocks String deriving (Eq, Show)

newtype Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Error:
-- phew = Papu "chases" True

truth :: Papu
truth = Papu (Rocks "A") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Error:
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

{-
f :: Num a => a
f = 1.0
<interactive>:128:26: error:
    • Could not deduce (Fractional a) arising from the literal ‘1.0’
      from the context: Num a
        bound by the type signature for:
                   f :: forall a. Num a => a
        at <interactive>:128:5-19
      Possible fix:
        add (Fractional a) to the context of
          the type signature for:
            f :: forall a. Num a => a
    • In the expression: 1.0
      In an equation for ‘f’: f = 1.0
-}

{-
:i RealFrac
type RealFrac :: * -> Constraint
class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
  {-# MINIMAL properFraction #-}
        -- Defined in ‘GHC.Real’
instance RealFrac Float -- Defined in ‘GHC.Float’
instance RealFrac Double -- Defined in ‘GHC.Float’
-}
f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

{-
myX :: Int
myX = 1 :: Int

sigmund :: a -> a
sigmund x = myX
<interactive>:3:36: error:
    • Couldn't match expected type ‘a’ with actual type ‘Int’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          sigmund :: forall a. a -> a
        at <interactive>:3:5-21
    • In the expression: myX
      In an equation for ‘sigmund’: sigmund x = myX
    • Relevant bindings include
        x :: a (bound at <interactive>:3:32)
        sigmund :: a -> a (bound at <interactive>:3:24)
-}

{-
myX :: Int
myX = 1 :: Int

sigmund' :: Num a => a -> a
sigmund' x = myX
<interactive>:5:47: error:
    • Couldn't match expected type ‘a’ with actual type ‘Int’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          sigmund' :: forall a. Num a => a -> a
        at <interactive>:5:5-31
    • In the expression: myX
      In an equation for ‘sigmund'’: sigmund' x = myX
    • Relevant bindings include
        x :: a (bound at <interactive>:5:43)
        sigmund' :: a -> a (bound at <interactive>:5:34)
-}

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

{-
mySort :: [Char] -> [Char]
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
• Couldn't match type ‘a’ with ‘Char’
  ‘a’ is a rigid type variable bound by
    the type signature for:
      signifier :: forall a. Ord a => [a] -> a
    at /Users/levin/workspace/git-repositories/haskell/haskellBook/src/Chapter6/exerciseChapter6.hs:144:1-30
  Expected type: [Char]
    Actual type: [a]
• In the first argument of ‘mySort’, namely ‘xs’
  In the first argument of ‘head’, namely ‘(mySort xs)’
  In the expression: head (mySort xs)
• Relevant bindings include
    xs :: [a]
      (bound at /Users/levin/workspace/git-repositories/haskell/haskellBook/src/Chapter6/exerciseChapter6.hs:145:11)
    signifier :: [a] -> a
      (bound at /Users/levin/workspace/git-repositories/haskell/haskellBook/src/Chapter6/exerciseChapter6.hs:145:1)
-}

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a2b a b = a2b a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith a2b i a = a2b a + fromInteger i

{-
newtype Nada = Nada Double deriving (Eq, Show)

instance Fractional Nada where
  (/) (Nada x) (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)

• No instance for (Num Nada)
    arising from the superclasses of an instance declaration
• In the instance declaration for ‘Fractional Nada’
 -}