# Folding Lists

## 10.1 Folds

Folds 作为一个一般的概念称为"变质"作用. (*Folds as a general concept are called catamorphisms*).

Catamorphisms 的字根是 "morphism", 表示 "态射" 的意思. ("态射"是数学的一个分支称为范畴论中的一个术语). "cata-" 的意思是 "down" 或者 "against".

Catamorphisms 的意思是分解数据结构 (destructing data).

A *fold* is a higher-order function which, given a function to accumulate the results and a recursive data structure, returns the built up value.

> *fold* 是一个高阶函数, 它由一个累积计算函数和一个递归数据结构构成, 并返回一个值.

Usually a "start value" for the accumulation is provided alone with a function that can combine the type of values in the data structure with the accumulation.

> 通常一个用于计算累计值的"初始值"会随着函数一同提供, 这个函数能够将数据结构中的值类型与前面的初始值累加起来.

The term fold is typically used with reference to collection of values referenced by a recursive datatype.

> 术语 fold 通常用于引用递归数据类型中所含值的集合.

A *catamorphism* is a generalization of folds to arbitrary data-types.

> *Catamorphism* 是将 fold 推广到任何数据类型的泛型化.

Where a fold allows you to break down a list into an arbitrary datatype, a catamorphism is a means of breaking down the structure of any datatype.

> *Fold* 允许你将列表分解为任意数据类型, 那么 *catamorphism* 就意味着分解任意数据类型的结构.

The `bool :: a -> a -> Bool -> a` function in Data.Bool is an example of a simple catamorphism for a simple, non-collection datatype.

下面的函数都属于 *catamorphism*:

- `bool`: 分解 `Bool` 数据结构, 并将其中值转换为任意类型
- `maybe`: 分解 `Maybe a` 数据结构, 并将其中值转换为任意类型
- `either`: 分解 `Either a b` 数据结构, 并将其中值转换为任意类型

```haskell
data Bool = False | True
-- bool x y p evaluate to x when p is False,
--            evaluate to y when p is True
bool :: a -> a -> Bool -> a

data Maybe a = Nothing | Just a
-- The `maybe` function takes a default, a function, and a `Maybe` value.
-- If the `Maybe` value is `Nothing`, the function returns the default value.
-- Otherwise, it applies the function to the value inside the `Just` and returns the value.
maybe :: b -> (a -> b) -> Maybe a -> b

data Either a b = Left a | Right b
-- If the value is `Left a`, apply the first function to a.
-- If it is `Right b`, apply the second function to b.
either :: (a -> c)
       -> (b -> c)
       -> Either a b
       -> c
```

## 10.2 Bringing you into the fold

我们先来看看函数 `foldr`, 表示 "fold right".

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

下面的例子用 `map` 和 `foldr` 做了对齐比较, 这对理解 `fold` 的本质非常有帮助.

```haskell

-- map
map    :: (a -> b) -> [a] -> [b]
map (+ 1) 1  :        2  :        3 : []
    (+ 1) 1  :  (+ 1) 2  :  (+ 1) 3 : []

-- foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr         (+)         0   (1 :  2 :  3 : [])
                               1 + (2 + (3 + 0))
```

`map` 应用一个函数到列表中的每个元素, 返回一个新列表.
`foldr` 使用一个函数替换列表的 cons constructors, reduce 这个列表, 返回一个值.

## 10.3 Recursive patterns

这章使用 `sum`, `length`, `product`, `concat` 函数的递归实现方法, 来阐述他们的共同点, 提取这些共同点, 进一步泛化就能得到 `foldr` 的实现.

```haskell
sum :: [Integer] -> Integer
sum [] = 0
sum (x : xs) = x + sum xs

length :: [a] -> Integer
length [] = 0
length (_ : xs) = 1 + length xs

product :: [Integer] -> Integer
product [] = 1
product (x : xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs
```

以上 4 个函数的 identity 分别是 0, 0, 1, [], 此外, 它们每个都有一个主函数, 该函数具有与右侧关联的递归模式. 列表的头被求值, 放到一边, 接着函数向右移动, 求下一个头, 以此类推.

> `identity` 的含义是,在 `infix` 函数操作中, 任何值与 `identity` 计算仍然是那个值, 比如: `(+)` 函数的 `identity` 是 `0`, `(*)` 是 `1`, `(++)` 是 `[]`

## 10.4 Fold right

We call `foldr` the "right fold" because the fold is right associative.

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x : xs) = f x (foldr f b xs)
```

### How foldr evaluates

这章主要解释了 `foldr` 函数, 也就是 "fold right" 中 "right" 的含义.

"right" 指的是 `(a -> b -> b)` 这个函数中的 'b', 也是该函数有两个参数, 右边的那个参数即为 "right", 在 `foldr` 函数的实现中, 主函数中的递归用于 'b', 也就是 "right", 这就是这个名字的实际意思.

使用 `foldr` 可以很方便的改写 `sum` 函数

```haskell
sum :: [Integer] -> Integer
sum = foldr (+) 0
```

#### Folding has Two Stage

Folding is that it happens in two stage, traversal and folding.

> *Folding* 有两个阶段, 一个是 traversal, 一个是 folding.

Traversal is the stage in which the fold recurses over the spine.

> *Traversal* 是递归 spine 的阶段.

Folding refers to the evaluation or reduction of the folding function applied to the values.

> *Folding* 是指应用到值的 folding 函数的 evaluation 或 reduction.

#### Accumulation Function

`Fold` 函数第一个参数是一个函数, 我们通常称这个函数为"累积函数". (*accumulation function*)

累积函数有如下几种形式:

- strict
- non-strict
- non-strict with unconditionally

`(+)` is strict in both of its arguments, so it forces the next iteration, but it's *unconditionally* so, so we're going to proceed to the next recursion of `foldr`.

`const` is non-strict and unconditionally.

## 10.5 Fold Left

We call `foldl` the "left fold" because the fold is right associative.

Left fold 和 right fold 穿过 spine 的方向是一样的, 但 folding 的方向恰好相反.

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b
foldl f b (x : xs) = foldl f (f b x) xs
```

### Associativity and Folding

```haskell
[1..3] == 1 : 2 : 3 : []

foldr f z [1, 2, 3]
f 1 (foldr f z [2, 3])
f 1 (f 2 (foldr f z [3]))
f 1 (f 2 (f 3 (foldr f z [])))
f 1 (f 2 (f 3 z))
-- f: (+)
-- z: 0
-- 1 + 2 + 3 + 0

foldl f z [1, 2, 3]
foldl f (f z 1) [2, 3]
foldl f (f (f z 1) 2) [3]
foldl f (f (f (f z 1) 2) 3) []
f (f (f (f z 1) 2) 3)
-- f: (+)
-- z: 0
-- 0 + 1 + 2 + 3
```

我们也可以使用 `scanr` 和 `scanl` 函数来更深入的理解它们不同连接方式

```haskell
Prelude> scanr (+) 0 [1..3]
[6,5,3,0]

Prelude> scanl (+) 0 [1..3]
[0,1,3,6]
```

换一个累积函数可以更好的观察它们的不同

```haskell
foldr (^) 2 [1..3]
1 ^ (2 ^ (3 ^ 2))
1 ^ (2 ^ 9)
1 ^ 512
1

foldl (^) 2 [1..3]
((2 ^ 1) ^ 2) ^ 3
(2 ^ 2) ^ 3
4 ^ 3
64
```

比较 `foldr (:) [] [1, 2, 3]` 和 `foldl (flip (:)) [] [1, 2, 3]`

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x : xs) = f x (foldr f b xs)

0. foldr (:) [] [1, 2, 3]
1. (:) 1 (foldr (:) [] [2, 3])
2. (:) 1 ((:) 2 (foldr (:) [] [3]))
3. (:) 1 ((:) 2 ((:) 3 (foldr (:) [] []))
   -- foldr hit base case, so return `b` ([])
4. (:) 1 ((:) 2 ((:) 3 []))
   -- 将上面的表达式换成 `infix` 的写法, 这样更加容易看的清楚
   1 : (2 : (3 : []))
5. 1 : (2 : [3])
6. 1 : [2, 3]
7. [1, 2, 3]
```

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b
foldl f b (x : xs) = foldl f (f b x) xs

0. foldl (flip (:)) [] [1, 2, 3]
                    ^
                    b
1. foldl (flip (:)) ((flip (:)) [] 1) [2, 3]
                    ^^^^^^^^^^^^^^^^^
                            b
2. foldl (flip (:)) ((flip (:)) ((flip (:)) [] 1) 2) [3]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                    b
3. foldl (flip (:)) ((flip (:)) ((flip (:)) ((flip (:)) [] 1) 2) 3) []
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                           b
   -- foldl hit base case, so return `b`
4. ((flip (:)) ((flip (:)) ((flip (:)) [] 1) 2) 3)
   -- 将上面的表达式换成 `infix` 的写法
   -- 注意 `flip (:)` 会调转参数的顺序
   3 : (2 : (1 : []))
5. 3 : (2 : [1])
6. 3 : [2, 1]
7. [3, 2, 1]
```

对比 `foldl (*) 1 [1..3]` 和 `foldl (flip (*)) 1 [1..3]`

```haskell
0. foldl (*) 1 [1..3]
              ^     ^   ^
              f     b   xs
1. foldl (*) ((*) 1 1) [2, 3]
             ^^^^^^^^^
                 b
2. foldl (*) ((*) ((*) 1 1) 2) [3]
             ^^^^^^^^^^^^^^^^^
                     b
3. foldl (*) ((*) ((*) ((*) 1 1) 2) 3) []
             ^^^^^^^^^^^^^^^^^^^^^^^^^
                         b
   -- foldl return `b`
4. ((*) ((*) ((*) 1 1) 2) 3)
   -- 将上面的表达式换成 `infix` 的写法
   ((1 * 1) * 2) * 3
5. (1 * 2) * 3
6. 2 * 3
7. 6
```

```haskell
0. foldl (flip (*)) 1 [1..3]
              ^     ^   ^
              f     b   xs
1. foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
                    ^^^^^^^^^^^^^^^^
                            b
2. foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                   b
3. foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                           b
   -- foldl return `b`
4. ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
   -- 将上面的表达式换成 `infix` 的写法
   -- 注意 `flip (*)` 会调转参数的顺序
   3 * (2 * (1 * 1))
5. 3 * (2 * 1)
6. 3 * 2
7. 6
```

### Exercise: Fix some problem

- `foldr (++) ["hello", "HELLO", "hello"]`

    > 缺失一个参数, 即累积函数的初始值
    >
    > ```haskell
    > Prelude> foldr (++) [] ["hello", "HELLO", "hello"]
    > "helloHELLOhello"
    > ```
    >
    > END

- `foldr max [] "fear is the little death"`

    > 不满足 `max :: Ord a => a -> a -> a` 函数的签名
    >
    > ```haskell
    > Prelude> foldr max ' ' "fear is the little death"
    > 't'
    > ```
    >
    > END

- `foldr and True [False, True]`

    > 不满足 `and :: Foldable t => t Bool -> Bool` 函数的签名, 第一个参数需要 `Foldable`, 因此需要更换一个函数, 使用 `(&&) :: Bool -> Bool -> Bool`
    >
    > ```haskell
    > Prelude> foldr (&&) True [False, True]
    > False
    > ```
    >
    > END

- `foldl ((++) . show) "" [1..5]`

    > 不满足 `((++) . show) :: Show a => a -> [Char] -> [Char]` 函数的签名, 因为这个函数的签名类似 `a -> b -> b`, 这与 `foldl` 所需要的累积函数签名 `b -> a -> b` 不同, 因此需要调整参数位置.
    >
    > ```haskell
    > Prelude> foldl (flip ((++) . show)) "" [1..5]
    > "54321"
    > ```
    >
    > 如果使用 `foldr` 函数则
    >
    > ```haskell
    > Prelude> foldr ((++) . show) "" [1..5]
    > "12345"
    > ```
    >
    > END

- `foldr const 'a' [1..5]`

    > 不满足 `const :: a -> b -> a` 函数的签名, 不满足 `foldr` 所需累积函数的签名 `a -> b -> b`, 因此需要更换函数顺序
    >
    > ```haskell
    > Prelude> foldr (flip const) 'a' [1..5]
    > 'a'
    > ```
    >
    > END

- `foldr const 0 "hello"`

    > 不满足 `const :: a -> b -> a` 函数的签名, 不满足 `foldr` 所需累积函数的签名 `a -> b -> b`, 因此需要更换函数顺序
    >
    > ```haskell
    > Prelude> foldr (flip const) 0 "hello"
    > 0
    > ```
    >
    > END

- `foldl (flip const) 0 "hello"`

    > 不满足 `flip const :: a -> b -> b` 函数的签名, 不满足 `foldl` 所需累积函数的签名 `b -> a -> b`, 因此不需要更换函数顺序
    >
    > ```haskell
    > Prelude> foldl const 0 "hello"
    > 0
    > ```
    >
    > END

- `foldl (flip const) 'z' [1..5]`

    > 不满足 `flip const :: a -> b -> b` 函数的签名, 不满足 `foldl` 所需累积函数的签名 `b -> a -> b`, 因此不需要更换函数顺序
    >
    > ```haskell
    > Prelude> foldl const 'z' [1..5]
    > 'z'
    > ```
    >
    > END

### Unconditional spine recursion

***这章是真的看不懂. 以后再理解.***

## 10.6 How to write fold functions

***这章较为简单, 跳过.***

## 10.7 Folding and evaluation

What differentiates `foldr` and `foldl` is associativity. The right associativity of `foldr` means the folding function evaluates from the innermost cons cell to outermost (the head).

One the other hand, `foldl` recurses unconditionally to the end of the list through self-calls and the the folding function evaluates from the outermost cons cell to innermost.

## 10.8 Summary

### foldr

1. The rest of the fold (recursive invocation of `foldr`) is an argument to the folding function you passed to `foldr`. It doesn't directly self-call as a tail-call like `foldl`. You could think of it as alternating between applications of `foldr` and your folding function `f`. The next invocation of `foldr` is conditional on `f` having asked for more of the results of having folded the list. That is:

    > *the rest of the fold* 指的是参数, `foldr` 函数第一个输入 folding 函数的一个参数(folding 函数指的是 `foldr` 函数的第一个 input `(a -> b -> b)`). 它并不像 `foldl` 函数那样, 通过尾调用方式来自调用. 可以这么认为, 它是 `foldr` 和 folding 函数 `f` 之间的交替应用. 下一次调用 `foldr` 的条件是 `f` 请求 fold 列表的更多结果.

    ```haskell
    foldr :: (a -> b -> b) -> b -> [a] -> b
    --             ^
    ```

    That `b` we're pointing at in (a -> b -> b) is *the rest of the fold*.
    Evaluating that evaluates the next application of `foldr`.

2. Associates to the right.
3. Works with infinite lists.

    ```haskell
    Prelude> foldr const 0 [1..]
    1
    ```

4. Is a good default choice whenever you want to transform data structures, be they finite or infinite.

### foldl

1. Self-calls as tail-call through the list, only beginning to produce values after reaching the end of list.
2. Associates to the left.
3. Cannot be used with infinite lists.
4. Is nearly useless and should almost always be replaced with `foldl'`.

## 10.9 Scan

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
scanr ::               (a -> b -> b) -> b -> [a] -> [b]

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
scanl ::               (b -> a -> b) -> b -> [a] -> [b]
```

fold 和 scan 从签名上看最主要的区别是, scan 必须返回一个列表, 而 fold 可以是任意类型的数据.

```haskell
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls =
    q : (case ls of
            [] -> []
            (x: xs) -> scanl f (f q x) xs)
```

下面 `fibs` 函数是一个无限递归函数

```haskell
fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs
```

0. `fibs`
1. `1 : scanl (+) 1 fibs`
2. `1 : scanl (+) 1 (1 : scanl (+) 1 fibs)`
3. `1 : scanl (+) 1 (1 : scanl (+) 1 (1 : scanl (+) 1 fibs))`
4. 在此处终止递归
5. `1 : scanl (+) 1 (1 : scanl (+) 1 [1, 1])`
6. `1 : scanl (+) 1 [1, 1, 2, 3]`
7. `[1, 1, 2, 3, 5, 8]`

上面的无限递归比较难理解, 可以通过下面的例子辅助

```haskell
Prelude> let x = 1 : x
Prelude> take 5 x
[1,1,1,1,1]
```

---

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x : xs) = f x (foldr f b xs)
```

![fold right](img/Right-fold-transformation.png)

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b
foldl f b (x : xs) = foldl f (f b x) xs
```

![fold left](img/Left-fold-transformation.png)
