# Chapter 2: Hello Haskell

## Arithmetic function in Haskell

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

## Let and Where

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
