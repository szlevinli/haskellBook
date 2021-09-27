# Chapter 6: TypeClass

## What are typeclasses?

Why typeclasses are a means of ad hoc polymorphism - *ad hoc* because typeclass are a dispatched by type.

> *ad hoc* 这里应该翻译成"临时"? *dispatched by type* 是什么意思?

## Writing TypeClass instances

- Datatype is sum type

```haskell
-- defined datatype
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun

-- implement Eq
instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Weds == Weds = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _ == _ = False
```

- Datatype for data constructor with arguments

```haskell
-- defined datatype
data Date = Date DayOfWeek Int

-- implement Eq
instance Eq Date where
  (Date weekday dayOfMonth) == (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'
```

- Datatype with polymorphic parameters

```haskell
-- defined datatype
newtype Identity a = Identity a

-- implement Eq
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```

> Difference between `data` and `newtype` see [stack overflow](https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell)

## Partial functions

A partial function is one that doesn't handle all the possible cases, so there are possible scenarios in which we haven't defined any way for the code to evaluate.

## Type-defaulting typeclasses

- default Num Integer
- default Real Integer
- default Enum Integer
- default Integral Integer
- default Fractional Double
- default RealFrac Double
- default Floating Double
- default RealFloat Double

$Num, Real$, etc., are typeclasses, and $Integer$ and $Double$ are the types they default to. This type defaulting for $Fractional$ means that:

```haskell
(/) :: Fractional a => a -> a -> a
```

changes to

```haskell
(/) :: Double -> Double -> Double
```

## Enum

This typeclass covers types that are enumerable, therefore have known ***predecessors*** and ***successors***.

```haskell
succ 4 -- output: 5
pred 'd' -- output: 'c'
```

## Instances are dispatched by type

Typeclass are dispatched by type.

Typeclass are defined by the set of operations and values all instances will provide.

Typeclass *instances* are unique pairing of the typeClass and a type.

- a typeclass defines a set of functions and/or values;
- types have instances of that typeclass;
- the instances specify the ways that type use the functions of the typeclass.

## 关于 override

在 Haskell 表述, 不存在子类改写父类的情况, 那么 Haskell 是如何处理 override 这种情况呢?

下面的测试代码说明了这一点. 源代码在 *Chapter6/overrideTypeclass.hs* 文件中

```haskell
class Ord a => MyOrd a where
  compare :: a -> a -> Int

newtype Age = Age Int deriving (Show)

instance Eq Age where
  (==) (Age n) (Age n') = n == n'

instance Ord Age where
  compare (Age n) (Age n')
    | n == n' = EQ
    | n > n' = GT
    | otherwise = LT

instance MyOrd Age where
  compare (Age n) (Age n')
    | n == n' = 0
    | n > n' = 1
    | otherwise = -1
```

在 GHCi 中执行 `compare (Age 1) (Age 2)` 会返回如下错误信息

```haskell
<interactive>:101:1: error:
    Ambiguous occurrence ‘compare’
    It could refer to
       either ‘Prelude.compare’,
              imported from ‘Prelude’ at src/Chapter6/doNotWrite.hs:1:1
              (and originally defined in ‘GHC.Classes’)
           or ‘Main.compare’, defined at src/Chapter6/doNotWrite.hs:28:3
```

发生了歧义错误, 因此我们需要如下调用 `compare`

```haskell
Prelude.compare (Age 1) (Age 2) -- output: Lt
Main.compare (Age 1) (Age 2) -- output: -1
```
