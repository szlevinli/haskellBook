# Chapter 7: More functional patterns

## Anonymous functions

```haskell
trip :: Integer -> Integer
trip = \x -> x * 3
```

## Pattern matching

_**Pattern matching**_ is a syntactic way of deconstructing product and sum types to get at their inhabitants.

> _**Pattern matching**_ *是一种语法, 用于解构 product 类型和 sum 类型, 从而可以提取它们中的"居住者".*

```haskell
-- nullary data constructor
-- not a sum or product
-- just a single value.
data Blah = Blah
```

Pattern matching on `Blah` can only do one thing.

```haskell
blahFunc :: Blah -> Bool
blahFunc Blah = True
```

```haskell
-- unary data constructor
-- not a sum or product
-- only contains a value.
data Identity a =
  Identity a
  deriving (Eq, Show)

-- can unpack and expose the 'a'
unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x

-- can ignore the contents of Identity
ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True

-- or ignore it completely since
-- matching on a non-sum constructor
-- changes nothing.
ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True
```

```haskell
-- binary data constructor
data Product a b =
  Product a b
  deriving (Eq, Show)

-- choose one of the values
-- in the a and b
productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

-- choose both of the values
-- in the a and b
productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)
```

Now we can discriminate by the inhabitants of the sum and choose to do different things based on which constructor in the sum they are.

```haskell
-- sum type data constructor
data SumOfThree a b c =
    FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2

-- we can selectively ignore
-- inhabitants of the sum
sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt _ = 1
```

*Pattern matching* is a way of matching values against pattern and, where appropriate, binding variables to successful matches.

It is worth noting here that *pattern* can including things as diverse as undefined variables, numeric literals, and list syntax. As we will see, pattern matching matches on any and all data constructors.

Patterns are matched against values, or data constructors, *not* types.

```haskell
isItTwo :: (Eq a, Num a) => a -> Bool
isItTwo 2 = True
isItTwo _ = False
```

## Pattern matching against data constructors

_**Pattern matching**_ 使我们能够

- 在给定不同输入的情况下改变函数的功能.
- 允许我们解包(unpack)和公开(expose)数据的内容.

> see  ./src/Chapter7/registeredUser.hs and ./src/Chapter7/penguins.hs

## Case expressions

```haskell
func :: (Eq a, Num a) => a -> [Char]
func x =
  case x + 1 == 1 of
    True -> "T"
    False -> "F"
```

## Higher-order functions

*Higher-order functions* (HOFs) are functions that accept functions as arguments or return functions as results.

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

## Guards

```haskell
f :: Integer -> Integer
f x
  | x < 0 = (-x)
  | otherwise = x
```

## Pointfree style

Pointfree refers to a style of composing function without specifying their arguments.

一种编程风格, 定义一个没有参数的函数 *f*, 该函数 *f* 返回值是一个函数.

```haskell
f = negate . sum

f [1, 2, 3, 4, 5] -- output: -15
```

```haskell
-- not pointfree
f :: Int -> [Int] -> [Int]
f x xs = foldr (+) x xs

-- pointfree
f :: Int -> [Int] -> [Int]
f = foldr (+)
```

详细分析以下 pointfree 函数

```haskell
-- no pointfree
reverseTuple (a, b) = (b, a)
-- pointfree version
reverseTuple = uncurry (flip (,))
```

以下是用到的三个函数的签名:

- `uncurry :: (a -> b -> c) -> (a, b) -> c`
- `flip :: (a -> b -> c) -> b -> a -> c`
- `(,) :: a -> b -> (a, b)`

函数应用:

- `reverseTuple` 实际上是 `uncurry` 函数的 partial application 形式, 也就是说 `reverseTuple` 函数是 `uncurry` 函数应用了第一个参数后返回的结果, 这里应用了第一个参数指的是 `flip` 函数, 从底层上看, `reverseTuple` 是 `uncurry :: (flip') -> (a, b) -> c`, `reverseTuple` 函数等待'用户'传入 `(a, b)` 形式的参数, 也就是 `uncurry` 的第二个参数
- `flip'` 是 `flip` 函数的 partial application 形式, 即 `flip :: ((,)) -> b -> a -> c`
- 现在我们传入参数 `(x, y)`, 应用顺序如下
  1. `reverseTuple (x, y)`
  2. `uncurry` 将传入的 `(x, y)` 作为其第二个参数应用, 将 `(x, y)` 拆解开传递给 `flip'` 函数
  3. `flip'` 函数接收到 `x` 和 `y` 参数, 接着将参数的位置对调后应用到 `(,)` 函数
  4. 现在 `(,)` 函数形式为 `y -> x -> (y, x)` 返回结果 `(y, x)`
  5. `(y, x)` 作为 `uncurry` 的最终结果返回
  6. `reverseTuple (x, y)` 得到的结果为 `(y, x)`

我们可以看到 pointfree 形式的函数是将多个函数组合到一起, 理解起来比较难, 但非常符合函数式编程的思想, 即通过组合函数的形式来解决问题, 而不是再新建一套处理逻辑.

我们再回到上面这个例子, `reverseTuple` 函数的意图从函数名称上看就很直观, 反转一个 tuple, 也就是说它需要接收一个 tuple 返回一个对调了顺序的 tuple.

- `uncurry` 函数在这里的目的是拆解 tuple
- `flip` 函数的目的是对调传入参数的顺序
- `(,)` 函数的目的是合并参数返回 tuple
