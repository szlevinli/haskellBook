# Algebraic datatypes

## 11.1 Algebraic datatypes

Writing your own datatypes can help you leverage some of Haskell's most powerful features - pattern matching, type checking, and inference - in a way that makes your code more concise and safer.

> 编写你自己的数据类型可以帮助你利用 Haskell 的一些强大特性: 模式匹配, 类型检查和类型推断, 从而是你的代码更加简洁和安全.

A type can be thought of as an enumeration of constructors that have zero or more arguments.

> 类型可以看作是具有零个或多个参数的构造器的枚举.

## 11.2 Data declarations review

When we talk about a data declaration, we are talking about the definition of the entire type.

## 11.3 Data and type constructors

There are two kinds of constructors in Haskell: type constructors and data constructors.

> 在 Haskell 中有两类构造器: 类型构造器和数据构造器

Type constructors are used only at the type level, in type signatures and typeclass declarations and instances.

> 类型构造器仅用于类型级别, 涉及类型签名和类型类声明及类型类实例化.

Data constructors construct the values at term level, values you can interact with at runtime.

> 数据构造器在 term 级别构造值, 这些值可以在运行时互相交互. (与之相对的是, 类型构造器定义的类型是静态的在编译时解析.)

We call them constructors because they define a means of creating or building a type or a value.

> 我们称他们为构造器, 是基于他们定义了一种创建类型或值的方法.

为了区别构造器是否有参数, 我们称无参数的构造器为 *constant*. 比如 `Bool` 类型

```haskell
-- Bool 是类型构造器, 因无参数, 可称其为 constants
-- False 和 True 是数据构造器, 同样可称之为 constants
data Bool = False | True
```

```haskell
data Trivial = Trivial'
--     [1]       [2]

data UnaryTypeCon a = UnaryValueCon a
--       [3]               [4]
```

1. 类型构造器 `Trivial`, 定义在 type level. 可称之为 *type constants*.
2. 数据构造器 `Trivial'`, 定义在 item level (runtime space).
3. 类型构造器 `UnaryTypeCon`, 具有一个参数. 它很像函数, 等待一个 type constant 去应用, 但其行为与函数是完全不同的.
4. 数据构造器 `UnaryValueCon`, 具有一个参数. 等待一个值去应. 再次提一下, 虽然像函数, 但其行为与真正的函数还是不同的.

## 11.4 Type constructors and kinds

Kinds are the types of types, or types on level up.

> kinds 是类型的类型, 或者说是类型的上一级.

We represent kinds in Haskell with `*`.

We know something is a fully applied, concrete type when it is represented as `*`. When it is `* -> *`, it, like a function, is still waiting to be applied.

> 当某事物表示为 `*` 时, 我们就知道它是一个完全应用的, 具体的类型. 当表示为 `* -> *`, 它就像一个函数, 还在等待被应用.

```haskell
Prelude> :k Bool
Bool :: *
Prelude> :k [Int]
[Int] :: *
Prelude> :k []
[] :: * -> *
```

`Bool` 和 `[Int]` 都是完全应用的, 有具体类型的, 因此他们的 kind 签名没有函数箭头(`->`).

`[]` 的 kind 签名是 `* -> *`, 表示它仍然需要去呗应用到一个具体的类型中.

## 11.5 Data constructors and values

Haskell 对类型构造器进行了区分, 分别是: type constructors 和 type constants. 同样的, 也可以对数据构造器进行区分, 分为: data constructors 和 constants.

```haskell
data PugType = PugData

data HuskyType a = HuskyData

data DogeDeBordeaux doge = DogeDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)
```

1. Is `Doggies` a type constructor or a data constructor?
    > a type constructor
2. What is the kind of `Doggies`?
    > `* -> *`
3. What is the kind of `Doggies String`?
    > `*`
4. What is the type of `Husky 10`?
    > `Num a => Doggies a`
5. What is the type of `Husky (10 :: Integer)`?
    > `Doggies Integer`
6. What is the type of `Mastiff "Sco Doo"`?
    > `Doggies String`
7. Is `DogeDeBordeaux` a type constructor or a data constructor?
    > All
8. What is the type of `DogeDeBordeaux`?
    > `doge -> DogeDeBordeaux doge`
9. What is the type of `DogeDeBordeaux "doggie!"`
    > `DogeDeBordeaux String`

## 11.6 What's a type and what's data?

Data constructors can take arguments, those arguments will be specific types, but not specific values.

> 数据构造器可以有参数, 这些参数可以是指定的类型, 但不能是指定的值.

## 11.7 Data constructor arities

We want to start demonstrating why we call them "algebraic".

Arity refers to the number of arguments a function or constructor takes.

Data constructors that zero argument are called *nullary*, take one argument are called *unary*, take more than one argument are called *products*.

```haskell
-- nullary
data Example0
  = Example0
  deriving (Eq, Show)

-- unary
data Example1
  = Example1 Int
  deriving (Eq, Show)

-- product of Int and String
data Example2
  = Example2 Int String
  deriving (Eq, Show)
```

## 11.8 What makes these datatypes algebraic?

Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two operations: sum and product.

> Haskell 中的代数数据类型是代数的, 因为我们可以使用两种基本操作: sum 和 product 来描述参数结构的模式.

The most direct way to explain why they're called sum and product is to demonstrate sum and product in terms of *cardinality*. This can be understood in terms of the cardinality you see with finite set.

> 解释为什么它们被称作 sum 和 product 最直接的方法是用基数来证明. 这可以用有限集合的基数来理解.

The cardinality of a datatype is the number of possible values it defines.

> 数据类型的基数是它定义的可能值的数量.

## 11.9 newtype

A `newtype` cannot be a product type, sum type, or contain nullary constructors, but it has a few advantages over a vanilla `data` declaration.

> `newtype` 不能用于 product type, sum type, 或者包含 nullary 构造器, 但是它比使用 `data` 声明的数据类型具有一些优势.

One is that it has no runtime overhead, as it reuse the representation of the type it contains.

> 一个优势是它没有运行时开销, 因为它重用了其所包含的类型. 因为 `newtype` 和它所包含的 type 之间的区别在编译时就消失了. (所以才说"在运行时没有开销")

这里举了一个列子来阐述上面所说的意思. 我们创建一个函数用于指示是否我们有太多山羊了

```haskell
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42
```

上面这个函数有个问题是如果我们还想知道是否奶牛太多了, 如果不小心调用了这个函数就会得到错误的结果(因为奶牛的阈值不一定和山羊的阈值相同). 也就是使用 `Int` 作为入参其含义过于模糊, 在使用过程中容易犯错, 此时我们可以使用 `newtype` 来解决:

```haskell
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)
```

此时我们将 `tooManyGoats` 函数改写为:

```haskell
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
```

也就是说使用 `newtype` 定义的 Goats 可以看做是 `Int` 的 type synonyms.

使用 `newtype` 可以提供与类型类实例化相关的其他优势. 为了理解这些优势, 我们需要将 `newtype` 分别与类型同义词 (type synonyms) 和 regular data declaration 进行对比.

我们先来看看与类型同义词间的对比.

A `newtype` is similar to a type synonym in that the representation of named type and the type it contains are identical and any distinction between them is gone at compiler time.

> `newtype` 类似于类型同义词, 因为命名类型的表示和它所包含的类型是相同的, 他们之间的区别在编译时就消失了.

从编写代码和读代码的角度看, `newtype` 比 type synonym 更利于人们理解数据来自何方, 将用于何处, 但对于编译器来说, 它们之间没什么不同. (比如上面的 Goats 和 Int, 对于编译器来说它就是 Int).

However, one key contrast between a `newtype` and a type alias is that you can define typeclass instance of `newtype` that differ from the instances for their underlying type. You can't do that for type synonyms.

> 然而, `newtype` 和类型别名之间的一个关键区别是, 你可以为 `newtype` 定义不同于其基础类型实例的类型类实例. 但, 你不能为 type synonym 这么做.

```haskell
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
```

上面这段代码如果使用 `tooMany 42` 方式调用, 将发生错误, 因为 42 是一个多态类型 (`Num a => a`), 因此我们必须这样调用 `tooMany (42 :: Int)`.

但是如果我们换成 `newtype` 定义的 `Goats`, 则不会出现这个问题.

```haskell
newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 42
```

此时我们可以使用 `tooMany (Goats 42)` 来调用.

On the other hand, what about the case where we want to reuse the typeclass instances of the type our newtype container?

> 另一方面, 如果我们想要重用 `newtype` 包含的类型的类型类实例, 又该怎么办. (这句想要描述的是: 我们该如何在 `newtype Goats` 的定义中使用 `deriving` 命令来继承用户自义定的类型类(比如上面提到的 `TooMany` 类型类))

For user-defined typeclasses, we can use a language extension called `GeneralizedNewTypeDeriving`. Language extensions, enable in GHC by the `LANGUAGE` pragma, tell the compiler to process input in ways beyond what the standard provides for.

In this case, this extension will tell the compiler to allow our `newtype` to rely on a typeclass instance for the type it contains.

We can do this because the representations of the `newtype` and the type it contains are the same. Still, it is outside of the compiler's standard behavior so we must give it the special instruction to allow us to do this.

Now we'll add the pragma at the top of our source file:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- 在此之前, 必须这么写. 等于需要定义两遍一样的 tooMany 函数
--
-- newtype Goats = Goats Int deriving (Eq, Show)
--
-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n
-- 
-- 现在我们可以直接使用 deriving 来避免再写一次相同的逻辑
-- 同时为了告知编译器我们的做法, 需要通过 pragma, 也就是在文件头设置编译器指令, 如下
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
```

## 11.10 Sum types

```haskell
import Data.Int (Int8)

data NumberOrBool
  = NumNum Int8
  | BoolBool Bool
  deriving (Eq, Show)
```

上面的 `NumberOrBool` 是 sum types, 它的基 (cardinality) 是 258 (Int8=256, Bool=2).

## 11.11 Product types

Any data constructor with two or more type arguments is a product.

### Record syntax

```haskell
data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show)

*Main> let levin = Person "levin" 18
*Main> levin
Person {name = "levin", age = 18}
*Main> name levin
"levin"
*Main> age levin
18
*Main> :t name
name :: Person -> String
*Main> :t age
age :: Person -> Int
```

## 11.12 Normal form

这章节想说的是隐藏在 Algebraic datatypes 背后的代数 (数学).

当然最终目的还是要阐明数据类型的基(cardinality), 理解它的计算方式, 从而得到其规模.

用数学的乘法分配律来说明什么是"sum of products"(乘积和), 以及什么是"in normal form".

```test
2 * (3 + 4)
根据乘法分配律可以写为
2 * 3 + 2 * 4
```

如果将上述表达式中数字视为基数集合的表示, 那么乘积和表达式就是标准形式(in normal form), 因为不需要执行计算了.

```haskell
data Fiction = Fiction deriving (Show)

data NonFiction = NonFiction deriving (Show)

data BookType
  = FictionBook Fiction
  | NonFictionBook NonFiction
  deriving (Show)

type AuthorName = String

newtype Author = Author (AuthorName, BookType)
```

上面的代码只为了说明 `Author` 这个类型所采用的定义方式不是 in normal form.

下面采用 in normal form 方式来定义 `Author`.

```haskell
type AuthorName = String

data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)
```

如果我们从 `a * (b + c)` 的角度来看, `BookType` 就好比是其中的 `b` 和 `c`, 最后定义的 `Author` 类型就仿佛使用乘法分配律将 `a * (b + c)` 改为 `a * b + a * c`. (这里对于 `a` 来讲代表了两个不同的值: `Fiction` 和`Nonfiction`, 我们可以认为他们都是 `Author` 的数据构造器就好理解了)

```haskell
data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr
```

上面的 `Expr` 也是标准形式(in normal form), 它的 sum of product: (Number Int) + Add (Expr Expr) + Minus Expr + ...

将下面的 `Garden` 定义改为 in normal form:

```haskell
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
```

The sum of products normal form of `Garden`:

```haskell
data Garden'
  = Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Garden
  | Lilac' Garden
  deriving (Show)
```

## 11.13 Constructing and deconstructing values

我们用一个 value 可以做两件事: 我们可以创建或构造一个 value, 或者可以 match 它和 consume 它.
