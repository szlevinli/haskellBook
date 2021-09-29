# Chapter 9: Lists

## 9.1 Lists

在 Haskell 中 List 有两个责任:

1. 提供对集合或多个值的操作
2. 提供无限数据的系列

## 9.2 The list datatype

```haskell
data [] a = [] | a : [a]
```

- `data []`: type constructor for list
- `[]`: data constructor for nullary list
- `a : [a]`: data constructor for list

`a : [a]` 也是递归的 data constructor, 所谓递归指的是可以使用下面的方式构造 lists

```haskell
1 : 2 : 3 : []
```

## 9.3 Pattern Matching on Lists

```haskell
myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x : _) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail [x] = Nothing
myTail (_ : xs) = Just xs
```

## 9.4 List's Syntactic Sugar

```haskell
-- list's syntactic sugar
[1, 2, 3] ++ [4]

-- equal
(1 : 2 : 3 : []) ++ (4 : [])
```

## 9.5 Using Ranges to Construct Lists

```haskell
[1..5] -- output: [1, 2, 3, 4, 5]

-- equal
enumFromTo 1 5

[1, 3..6] -- output: [1, 3, 5]

-- equal
enumFromThenTo 1 3 6
```

## 9.6 Extracting Portions of Lists

- `take :: Int -> [a] -> [a]`
- `drop :: Int -> [a] -> [a]`
- `splitAt :: Int -> [a] -> ([a], [a])`
- `takeWhile :: (a -> Bool) -> [a] -> [a]`
- `dropWhile :: (a -> Bool) -> [a] -> [a]`

需要留意, `takeWhile` 是从 `list` 的第一个元素开始判断, 一旦不满足就停止, 因此下面的代码返回的是 `[]` 而不是 `[7, 8, 9, 10]`

```haskell
takeWhile (>6) [1..10] --output: []
```

## 9.7 List Comprehension

List comprehensions are a means of generating a new list from a list or lists.

```haskell
[x^2 | x <- [1..10]]
```

- `x^2`: This is the output function that will apply to the numbers of the list we indicate.
- `|`: The pipe here designates the separation between the output function and the input.
- `x <- [1..10]`: This is the input set: a generator list and a variable that represents the elements that will be drawn from that list. This says, "from a list of numbers from 1-10, take (`<-`) each element as an input to the output function."

### Adding Predicates

```haskell
[x^2 | x <- [1..10], rem x 2 == 0]
```

### Multiple Generator

有一点需要注意, 最右边的 generator 将优先被"耗尽" (exhausted), 接着是右数第二个 generator, 依次进行.

```haskell
[x^y | x <- [1..5], y <- [2, 3]]
-- output:
-- [1,1,4,8,9,27,16,64,25,125]
```

### List Comprehensions with Strings

`elem :: (Foldable t, Eq a) => a -> t a -> Bool`

## 9.8 Spines and Non-strict evaluation

When we talk about data structures in Haskell, particularly lists, sequences, and trees, we talk about them have a ***spines***.

> 当我们在 Haskell 提及数据结构时, 特别是 lists, sequences, and trees, 我们说他们有一个主干.

**Spine** 是一个将值的集合联系在一起的连接结构. (*This is the connective structure that ties the collection of values together.*)

```text
 : <------|
/ \       |
_ : <-----| This is the "spine"
 / \      |
 _ : <----|
  / \
  _ []
```

这里有两个术语 `spine` 和 `non-strict` 比较难理解, 下面用两个小结来介绍它们.

### Lazy vs. non-strict

> [Source](https://wiki.haskell.org/Lazy_vs._non-strict)

Haskell is often described as a lazy language. However, the language specification simply states that Haskell is non-strict, which is not quite the same things as lazy.

*Haskell 通常被称为是一种 lazy 语言. 然而, Haskell 的语言规范只是声称自己是一个 no-strict 语言, 这与 lazy 语言并不完全相同.*

**Non-strictness** 指的是一种*由外而内的 reduction (evaluation 的数学术语) 过程*, 比如 `(a+(b*c))` 首先 reduction `+` 接着 reduction 里面的 `(b*c)`. **Strict** 语言则是另一种工作方式, 它是由内而外的.

这对语义很重要, 对于 **Strict** 语言因为是由内而外的工作方式, 因此必选要找到最内部的数据 (*bottom value*), 而对于 **Non-strict** 语言, 因为它是由外而内的, 因此可以先计算外部的子表达式, 也就说不用寻找 *bottom value*.

**Lazy evaluation** 是另一个层面的事情, 它表示当需要结果的时候才计算 (evaluation)一个表达式 (请注意, 这里使用的是 evaluation 而不是 reduction). 当计算引擎看到一个表达式, 它会构建一个称之为 thunk 的数据结构, 其中包含求值表达式所需的任何值, 以及一个指向表达式本身的指针. 当实际需要结果时, 计算引擎调用表达式, 然后用结果替换 thunk 以供将来引用.

显然, 在 thunk 和 partly-evaluated expression 间有很强的对应关系. 因此在大部分情况下 lazy 和 non-strict 是同义词. 但它们又不完全相同.

### Spine

先来看个例子

```haskell
> length (undefined : 3 : 4 : undefined : [])
4
> length (2 : 3 : 4 : 5 : undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:132:25 in interactive:GHCi15
```

第一个列表包含 bottom value 作为列表元素, 而且列表的 "shape" 也完全定义了. 大体来说, 每一个列表 cell 都有一个清晰明确的 "pointer" 定义, 指向它的下一个元素. 这个 "shape" 被称作 spine.

与第一个列表比较而言, 第二个列表有完整的元素定义, 但是它的 spine 是没有定义的. 这是因为它没有以 `[]` 结尾, 而且还使用了一个 non-terminating 表达式 `undefined`. 这种情况下就表示 spine 没有定义.

函数 `length` 关心的是 spine 而不是元素, 因此它可以用在第一个例子中, 但第二个不行.

### Spines are evaluated independently of values

Values in Haskell get reduced to weak head normal form by default. *(默认情况下, 在 Haskell 中的值被简化为弱头范式.)*

By 'normal form' we mean that the expression is fully evaluated. *(我们所说的'范式'指的是表达式已被完全计算.)*

'Weak head normal form' means the expression is only evaluated as far as is necessary to reach a data constructor. *('弱头范式'意味着表达式仅在需要达到数据构造器时进行计算.)*

'Weak Head Normal Form' 缩写为 'WHNF', 'Normal Form' 缩写为 'NF'

```haskell
(1, 2) -- WHNF & NF
```

上面的表达式既是 WHNF 也是 NF, 因为这是一个完全计算的表达式, 所以是一个 NF, 同时因为 NF 属于 WHNF, 因此它也是 WHNF.

```haskell
(1, 1 + 1) -- WHNF
```

上面的表达式是 WHNF, 不是 NF. 因为 `(+)` 应用了参数, 但还未计算.

```haskell
\x -> x * 10 -- WHNF & NF
```

上面的匿名函数是 NF, 所以是 WHNF.

```haskell
"Web" ++ "Page"
```

上面的表达式既不是 WHNF 也不是 NF. 这是因为它最外层的组件是一个函数, `(++)`, 它的参数已经被应用, 但是还没有计算.

**判断方法:**

- 如果表达式最外层不是数据构造函数, 则表达式既不是 WHNF 也不是 NF.
- 如果表达式的任何部分未求值, 则它不是 NF.

```haskell
-- NF & WHNF
[1, 2, 3, 4, 5]

-- WHNF
1 : 2 : 3 : 4 : _

-- neither
enumFromTo 1 10

-- neither
length [1, 2, 3, 4, 5]

-- neither
sum (enumFromTo 1 10)

-- neither
['a'..'m'] ++ ['n'..'z']

-- WHNF
(_, 'b')
```

## 9.9 Transforming Lists of Values

```haskell
map :: (a -> b) -> [a] -> [b]

fmap :: Functor f => (a -> b) -> f a -> f b
```

这里需要注意 `map` 函数的 reduce 方法是 non-strict 的, 见下

```haskell
map (+1) [1, 2, 3]
-- de-sugared, (:) is infix-r 5,
-- so it's right-associative
map (+1) (1 : (2 : (3 : [])))

-- Apply (+1)
(+1) 1 :
  ((+1) 2 :
    ((+1) 3 :
      ((+1) [])))

-- Now we reduce, non-strict
2 : ((+1) 2 : ((+1) 3 : []))
2 : 3 : (+1) 3 : []
2 : 3 : 4 : [] -- == [2, 3, 4]
```

Using the spine syntax:

```text
       :
      / \
(+1) 1   :
        / \
  (+1) 2   :
          / \
    (+1) 3  []
```

### 9.10 Filtering Lists of Values
