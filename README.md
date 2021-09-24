# Haskell Programming from First Principles

这是关于 *Haskell Programming from First Principles* 这本书的阅读笔记.

> *First Principle* 是基本原理的意思

## Chapter 1: All You Need is Lambda

*Lambda calculus* 是一种计算模型. 这里的 *calculus* 可以翻译为"演算".

*Calculus* 演算是一种计算或推理的方法.

*Lambda calculus* 是一种形式化(*formalizing*)方法的过程.

与图灵机一样, *lambda calculus* 形式化(*formalize*)了有效可计算性的概念, 从而确定哪些问题或哪类问题可以被解决.

### What is functional programming?

The essence of functional programming is that programs are a combination of *expressions*.

> *函数式编程的本质是程序, 是表达式的组合.*

Expressions include concrete values, variables, and also functions.

> *表达式包括具体的值, 变量和函数.*

Functions are expressions that are applied to an argument or input, and once applied, can be reduced or evaluated.

> *函数是应用于参数或输入的表达式, 一旦应用, 就可以被简化或求值.*

In Haskell, and in functional programming more generally, function are first-class: they can be used as values or passed as arguments, or inputs, to yet more functions.

> *在 Haskell 和更普遍的函数编程中, 函数是"头等公民": 他们可以用作值, 也可以用作参数或输入传递给更多的函数.*

The word *purity* in functional programming is sometimes also used to mean what is more properly called *referential transparency*.

> *在函数式编程中, purity 这个词有时也可以更贴切的称为"引用透明性"*

Referential transparency means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, as they do in math.

> *引用透明性意味着相同的函数, 给定相同的值, 在纯函数编程中总是返回相同的结果, 就像在数学中一样*.

### What is a function

A function is a relation between a set of possible inputs and a set of possible outputs. The function itself defines and represents the relationship.

> *函数是可能的输入集合与可能的输出集合间的关系. 函数本身定义并表示了这种关系.*

### The structure of lambda terms

The lambda calculus has three basic components, or *lambda terms*: expressions, variables, and abstractions.

> *lambda calculus 由三个基本组件组成, 或将这三个基本组件简称为 lambda terms: 他们是 表达式, 变量, 抽象.*

The word *expression* refers to a superset of all those things: an expression can be a variable name, an abstraction, or a combination of those things.

> *表达式指的是这些所有东西的超集: 一个表达式可以是一个变量名, 一个抽象, 或是这些东西的组合.*

Variables here have no meaning or value, they are only names for potential inputs to functions.

> *这里的变量是没有意义和值的, 他们只是函数潜在可输入数据的名称.*

An *abstraction* is a *function*. It is a lambda term that has a head (a lambda) and a body and is applied to an argument. An *argument* is an input value.

> *抽象是一个函数. 他是一个 lambda 术语. 有一个头和一个主体, 以及一个应用参数. 参数是一个输入值.*

### Alpha equivalence

There's a form of equivalence between lambda terms called *alpha equivalence.*

> *lambda 项之间有一种等价形式, 称之为 alpha 等价.*

下面所有的函数都是相同的

- $\lambda x.x$
- $\lambda d.d$
- $\lambda z.z$

### Beta reduction

When we apply a function to an argument, we substitute the input expression for all instances of bound variables within the body of the abstraction. You also eliminate the head of the abstraction, since its only purpose was to bind a variable. This process is called *beta reduction.*

> *当我们将函数应用到参数时, 我们用输入表达式替换抽象体中绑定变量的所有实例. 同时还消除了抽象的头, 因为它唯一的目的就是绑定变量. 这个过程称之为 beta reduction.*

下面我们将函数 $\lambda x.x$ 应用到参数 2 上:

1. $(\lambda x.x) \; 2$ *-- 应用函数到参数上*
2. 2 *-- 消除抽象头*

> $f(x) = x$ 与 $\lambda x.x$ 的区别:
>
> - $f(x) = x$ 是一个声明. 是一个具函数名称为 $f$ 的声明(declaration)
> - $\lambda x.x$ 是一个函数

### Free variables

Sometimes the body expressions has variables that are not named in the head. We call those variables *free variables.*

> *有时候表达式体中有未在表达式头中命名的变量. 这些变量被称为 free variables.*

比如: $\lambda x.xy$ 其中变量 $y$ 就是 free variables.

### Combinators

A combinator is a lambda term with no free variable.

以下是 combinator

- $\lambda x.x$
- $\lambda xy.x$
- $\lambda xyz.xz(yz)$

以下不是 combinator

- $\lambda y.x$
- $\lambda x.xz$

### Divergence

*Divergence* 是分散或发散的意思, 在这里指的是: **无法收敛(_converge_)的 Lambda terms**.

> **_Diverge_** 和 **_Converge_** 是一对反义词.

并非所有的 *reducible lambda term* 都可以化为 *beta normal form* (也就是最简式).

比如下面的 *lambda term* 也被称为 *omega* 就是 *diverge*, 也就是属于 *reducible lambda term* 但是无法化为 *beta normal form*

1. $(\lambda x.xx)(\lambda x.xx)$
2. $([x := (\lambda x.xx)]xx)$
3. $(\lambda x.xx)(\lambda x.xx)$

上面的 **lambda term** 在进行 *reduce* 时会进入无限循环 $1 \rightarrow 2 \rightarrow 3 \rightarrow 2 \rightarrow 3 \rightarrow...$

### Summary

- Functional programming is based on expressions that include variables or constant values, expressions combined with other expressions, and functions.
- Functions have a head and body and are those expressions that can be applied to arguments and reduced, or evaluated, to a result.
- Variables may be bound in the function declaration, and every time a bound variable shows up in a function, it has the same value.
- All functions take one argument and return on result.
- Functions are a mapping of a set of inputs to a set of outputs. Given the same value, they always return the same result.

## Chapter 2: Hello Haskell

### Arithmetic function in Haskell

`div`, `mod`, `quot` and `rem` 的区别, 可以通过下面的公式理解

```haskell
(div x y) * y + (mod x y) == x

(quot x y) * y + (rem x y) == x
```

关于 `div` 和 `quot` 的区别, 根据 [Stack Overflow](https://stackoverflow.com/questions/8111120/integral-operators-quot-vs-div/8111203#8111203) 上的解释:

> `quot` is integer division truncated toward zero, while `div` is truncated toward negative infinite.

也就是说 `div 20 (-6)` 返回 `-4` (*toward negative infinite*), 而 `quot 20 (-6)` (*toward zero*) 返回 `-3`.

对于 `mod` 和 `rem` 的区别, if one or both arguments are negative, the result of `mod` will have the same sign as the divisors, while the result of `rem` will have same sign as the dividend.

`mod (-9) 7` 返回 `5`, 而 `rem (-9) 7` 返回 `-2`.

### Let and Where

- `let` introduces an *expression*
- `where` is a *declaration*

```haskell
module FunctionWithWhere where

printInc :: (Show a, Num a) => a -> IO ()
printInc n = print plusTwo
  where
    plusTwo = n + 2
```

```haskell
module FunctionWithLet where

printInc :: (Show a, Num a) => a -> IO ()
printInc n =
  let plusTwo = n + 2
   in print plusTwo
```

下面的代码时一样的

```haskell
-- this should work in GHCi
let x = 5; y = 6 in x * y

-- this in file
mult1 = x * y
  where
    x = 5
    y = 6
```

## Chapter 3: String

Complementary function:

- `head` and `tail`
- `take` and `drop`
- `!!`

## Chapter 4: Basic DataTypes

- Bool
- Char
- Int
- Integer
- Float
- Double
- Rational
- Scientific

## Chapter 5: Types

### Types

一个数据类型 (datatype) 声明定义了一个类型构造器 (type constructor) 和若干数据构造器 (data constructors).

数据构造器 (data constructors) 是具体类型的值. 他们也是函数, 可以让我们创建具体类型的数据 (data) 或者值 (values).

### Sectioning

The term *sectioning* specifically refers to partial application of infix operators, which has a special syntax and allow you to chose whether the argument you're partially applying the operator to is the first or second argument:

```haskell
-- in GHCi
let x = 5
let y = (2^)
let z = (^2)

y x -- 2 ^ 5 return 32
z x -- 5 ^ 2 return 25
```

```haskell
-- GHCi
let c = (`elem` [1..10])
c 9 -- output: True
c 25 -- output: False
```

`sectioning` 用于将 `infix` 函数的参数顺序进行调整, 它与 *partial application* 略有不同, *partial application* 不能调整参数顺序.

### Polymorphism

*Polymorph* is word of relatively recent provenance.
> *多态* 是一个起源相对较近的单词.

It was invented in the early 19th century from the Greek words *poly* for "many" and *morph* form "form". The *-ic* suffix in polymorphic means "made of". So, 'polymorphic' means "made of many forms". In programming, this is understood to be in contrast with *monomorphic*, "made of one form".

Broadly speaking, type signatures may have three kinds of types:

- **concrete**: Int, Bool
- **parametrically polymorphism**: a, b
- **constrained polymorphism**: Num a, Ord b

## Chapter 6: TypeClass

### What are typeclasses?

Why typeclasses are a means of ad hoc polymorphism - *ad hoc* because typeclass are a dispatched by type.

> *ad hoc* 这里应该翻译成"临时"? *dispatched by type* 是什么意思?

### Writing TypeClass instances

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

### Partial functions

A partial function is one that doesn't handle all the possible cases, so there are possible scenarios in which we haven't defined any way for the code to evaluate.

### Type-defaulting typeclasses

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

### Enum

This typeclass covers types that are enumerable, therefore have known ***predecessors*** and ***successors***.

```haskell
succ 4 -- output: 5
pred 'd' -- output: 'c'
```

### Instances are dispatched by type

Typeclass are dispatched by type.

Typeclass are defined by the set of operations and values all instances will provide.

Typeclass *instances* are unique pairing of the typeClass and a type.

- a typeclass defines a set of functions and/or values;
- types have instances of that typeclass;
- the instances specify the ways that type use the functions of the typeclass.

### 关于 override

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

## Chapter 7: More functional patterns

### Anonymous functions

```haskell
trip :: Integer -> Integer
trip = \x -> x * 3
```

### Pattern matching

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

#### Pattern matching against data constructors

_**Pattern matching**_ 使我们能够

- 在给定不同输入的情况下改变函数的功能.
- 允许我们解包(unpack)和公开(expose)数据的内容.

> see  ./src/Chapter7/registeredUser.hs and ./src/Chapter7/penguins.hs

### Case expressions

```haskell
func :: (Eq a, Num a) => a -> [Char]
func x =
  case x + 1 == 1 of
    True -> "T"
    False -> "F"
```

### Higher-order functions

*Higher-order functions* (HOFs) are functions that accept functions as arguments or return functions as results.

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

### Guards

```haskell
f :: Integer -> Integer
f x
  | x < 0 = (-x)
  | otherwise = x
```

### Pointfree style

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

## Chapter 8: Recursion

### Factorial

```haskell
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)
```

`incTimes 5 5`:

1. `1 + (incTimes (5 - 1) 5)`
2. `1 + (1 + (incTimes (4 - 1) 5))`
3. `1 + (1 + (1 + (incTimes (3 - 1) 5)))`
4. `1 + (1 + (1 + (1 + (incTimes (2 - 1) 5))))`
5. `1 + (1 + (1 + (1 + (1 + (incTimes (1 - 1) 5)))))` # 转折点
6. `1 + (1 + (1 + (1 + (1 + 5))))`
7. `1 + (1 + (1 + (1 + 6)))`
8. `1 + (1 + (1 + 7))`
9. `1 + (1 + 8)`
10. `1 + 9`
11. `10`

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f. applyTimes (n - 1) f $ b
```

`applyTimes 3 (+1) 5`:

1. `(+1) . applyTimes (3 - 1) (+1) $ 5`
2. `(+1) . (+1) . applyTimes (2 - 1) (+1) $ 5`
3. `(+1) . (+1) . (+1) . applyTimes (1 - 1) (+1) $ 5` # 转折点
4. `(+1) . (+1) . (+1) $ 5`
5. `(+1) . (+1) $ 6`
6. `(+1) $ 7`
7. `8`

### Bottom

Bottom is a term used in Haskell to refer to computations that do not successfully result in a value.

### Fibonacci numbers

```haskell
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

`fibonacci 6`:

1. `fibonacci 5 + fibonacci 4`
2. `fibonacci 4 + fibonacci 3 + fibonacci 3 + fibonacci 2`
3. `fibonacci 3 + fibonacci 2 + fibonacci 2 + fibonacci 1 + fibonacci 2 + fibonacci 1 + fibonacci 1 + fibonacci 0`
4. `fibonacci 2 + fibonacci 1 + fibonacci 1 + fibonacci 0 + fibonacci 1 + fibonacci 0 + 1 + fibonacci 1 + fibonacci 0 + 1 + 1 + 0`
5. `fibonacci 1 + fibonacci 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + 1 + 0`
6. `1 + 0 + 1 + 1 + 0 + 1 + 0 + 1 + 1 + 0 + 1 + 1 + 0`
7. `8`

上面采用 6 有点繁琐, 不容易看清, 改用 3

`fibonacci 3`:

1. `fibonacci 2 + fibonacci 1`
2. `fibonacci 1 + fibonacci 0 + 1`
3. `1 + 0 + 1`
4. `2`

`fibonacci` 递归函数的关键在于理解参数 `n` 的含义, 这个参数是计算第 `n` 个菲波那切数列的值, 根据菲波那切数列的定义, 第 `n` 个菲波那切数列的值等于第 `n-1` 个菲波那切数列的值加上第 `n-2` 个菲波那切数列的值.

### Integral division from scratch

```haskell
type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

type Remainder = Integer

dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy num denom = go num denom 0
  where
    go n d c
      | n < d = (n, c)
      | otherwise = go (n - d) d (c + 1)
```

`type` 语法是创建类型别名, 目的是为了更好的理解代码

`go` 函数是一种习惯用法, 目的是使用比顶层函数(`dividedBy`)参数更多的参数来计算.

上面的除法函数并不完整, 因为没有考虑除零异常和负数的情况, 下面是一个完善后的除法函数.

```haskell
type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

type Remainder = Integer

data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

-- 实现除法 (完整版)
-- 10 / 2 = 5
-- 10 / -2 = -5
-- -10 / 2 = -5
-- -10 / -2 = 5
dividedBy' :: Numerator -> Denominator -> DividedResult
dividedBy' num denom = go num denom 0
  where
    go n d c
      | d == 0 = DividedByZero
      | d < 0 = case dividedBy' n (- d) of
        DividedByZero -> DividedByZero
        Result r -> Result (- r)
      | n < 0 = case dividedBy' (- n) d of
        DividedByZero -> DividedByZero
        Result r -> Result (- r)
      | n < d = Result c
      | otherwise = go (n - d) d (c + 1)
```

---

## Definitions

- **Typeclass inheritance** is when a typeclass has a superclass. This is a way of expressing that a typeclass requires *another* typeclass to be available for a given type before you can write an instance.

  ```haskell
  class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a
  ```

  Here the typeclass `Fractional` *inherits* from `Num`. We could also say that `Num` is a *superclass* of `Fractional`. The long and short of it is that if you want to write an instance of `Fractional` for some $a$, must already have an instance of `Num` before you may do so.

  ```haskell
  newtype Nada = Nada Double deriving (Eq, Show)

  instance Fractional Nada where
    (/) (Nada x) (Nada y) = Nada (x / y)
    recip (Nada n) = Nada (recip n)
    fromRational r = Nada (fromRational r)
  ```

  Then if you try to load it:

  ```text
  • No instance for (Num Nada)
      arising from the superclasses of an instance declaration
  • In the instance declaration for ‘Fractional Nada’
  ```

  You need a `Num` instance first. Can't write one that makes sense? Then you're not allowed to have a `Fractional` instance either. Them is the rules.
