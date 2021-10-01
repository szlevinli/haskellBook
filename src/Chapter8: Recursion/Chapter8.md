# Chapter 8: Recursion

## Factorial

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

## Bottom

Bottom is a term used in Haskell to refer to computations that do not successfully result in a value.

## Fibonacci numbers

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

## Integral division from scratch

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
