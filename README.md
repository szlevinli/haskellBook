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

### Polymorphism

Broadly speaking, type signatures may have three kinds of types:

- **concrete**: Int, Bool
- **parametrically polymorphism**: a, b
- **constrained polymorphism**: Num a, Ord b

## Chapter 6: TypeClass

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
