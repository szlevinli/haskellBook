newtype AnInteger = AnInteger Integer

instance Eq AnInteger where
  (==) (AnInteger i) (AnInteger i') = i == i'

data TwoIntegers = TwoIntegers Integer Integer

instance Eq TwoIntegers where
  (==) (TwoIntegers i1 i2) (TwoIntegers i1' i2') =
    i1 == i1' && i2 == i2'

data StringOrInt = AnInt Int | AString String

instance Eq StringOrInt where
  (==) (AnInt i) (AnInt i') = i == i'
  (==) (AString s) (AString s') = s == s'
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    x == x' && y == y'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False