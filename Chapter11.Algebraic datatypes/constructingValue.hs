module ConstructValue where

data GuessWhat
  = ChickenButt
  deriving (Eq, Show)

newtype Id a
  = MkId a
  deriving (Eq, Show)

data Product a b
  = Product a b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pFirst :: a,
    pSecond :: b
  }
  deriving (Eq, Show)

--
-- Sum and Product
--

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)

newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep
  = NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo
  = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

--
-- Constructing values
--

trivialValue :: GuessWhat
trivialValue = ChickenButt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetWork :: Sum Twitter AskFm
socialNetWork = First Twitter

data OperatingSystem
  = GunPlusLinux
  | OpenBSDPlusNeverMindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

--
-- Exercise: Programmers
--

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GunPlusLinux,
    OpenBSDPlusNeverMindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell,
    Agda,
    Idris,
    PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]
