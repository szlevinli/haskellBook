# Chapter 5: Types

## Types

一个数据类型 (datatype) 声明定义了一个类型构造器 (type constructor) 和若干数据构造器 (data constructors).

数据构造器 (data constructors) 是具体类型的值. 他们也是函数, 可以让我们创建具体类型的数据 (data) 或者值 (values).

## Sectioning

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

## Polymorphism

*Polymorph* is word of relatively recent provenance.
> *多态* 是一个起源相对较近的单词.

It was invented in the early 19th century from the Greek words *poly* for "many" and *morph* form "form". The *-ic* suffix in polymorphic means "made of". So, 'polymorphic' means "made of many forms". In programming, this is understood to be in contrast with *monomorphic*, "made of one form".

Broadly speaking, type signatures may have three kinds of types:

- **concrete**: Int, Bool
- **parametrically polymorphism**: a, b
- **constrained polymorphism**: Num a, Ord b
