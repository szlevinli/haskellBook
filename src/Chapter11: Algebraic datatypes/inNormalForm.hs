module InNormalForm where

-------------------------------------------------------------------------------
-- Author's definition: no normal form
-------------------------------------------------------------------------------

-- data Fiction = Fiction deriving (Show)

-- data NonFiction = NonFiction deriving (Show)

-- data BookType
--   = FictionBook Fiction
--   | NonFictionBook NonFiction
--   deriving (Show)

-- type AuthorName = String

-- newtype Author = Author (AuthorName, BookType)

-------------------------------------------------------------------------------
-- Author's definition: in normal form
-------------------------------------------------------------------------------

type AuthorName = String

data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-------------------------------------------------------------------------------
-- Exercise: How does Your Garden Grow?
-------------------------------------------------------------------------------

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden
  = Garden Gardener FlowerType
  deriving (Show)

data Garden'
  = Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Garden
  | Lilac' Garden
  deriving (Show)
