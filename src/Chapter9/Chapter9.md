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

## 9.8 Spins and nonstrict evaluation

When we talk about data structures in Haskell, particularly lists, sequences, and trees, we talk about them have a ***spines***.

> 当我们在 Haskell 提及数据结构时, 特别是 lists, sequences, and trees, 我们说他们有一个主干.
