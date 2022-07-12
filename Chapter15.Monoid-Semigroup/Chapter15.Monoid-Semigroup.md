# Monoid, Semigroup

## 15.1 Monoids and semigroups

## 15.2 What we talk about when we talk about algebras

An algebra refers to some operations and the set they operate over.

> 代数指的是一些运算和它们所对应的集合.

## 15.3 Monoid

A monoid is a function that takes two arguments and follows two laws:

- associativity 结合律
- identity 恒等式

## 15.4 How Monoid is defined in Haskell

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

## 15.5 Examples of using Monoid

### List

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

## 15.6 Why integer doesn't have a Monoid

It isn't clear if those should be added or multiplied as a `mappend` operation. It says there's no `Monoid` for those `Integer` for that reason.

To resolve the conflict, we have the `Sum` and `Product` newtypes to wrap numeric values and signal which `Monoid` instance we want.

**_Integers form a monoid under summation and multiplication._** We can similarly say that lists form a monoid under concatenation.

### Why newtype?

First, there's not much semantic difference (except for circumstances involving `bottom`, explained later) between the following datatypes:

```haskell
data Server = Server String

newtype Server' = Server' String
```

#### In Summary, why you might use newtype

1. To signal intent: using `newtype` makes it clear that you only intend for it to be a wrapper for the underlying type. The newtype cannot eventually grow into a more complicated sum or product type, while a normal datatype can.
2. To improve type safety: avoid mixing up many values of the same representation, such as `Text` or `Integer`.
3. To add different typeclass instances to a type that is otherwise unchanged representation, such as with `Sum` and `Product`.

### More on Sum and Product

```haskell
Prelude> :info Sum
newtype Sum a = Sum {getSum :: a}
...
instance Num a => Monoid (Sum a)

Prelude> :info Product
newtype Product a = Product {getProduct :: a}
...
instance Num a => Monoid (Product a)
```

The instance say that we can use `Sum` or `Product` values as a `Monoid` as long as they contain numeric values.

## 15.7 Why bother?

A common use of monoids is to structure and describe common modes of processing data.

> Monoids 的一个常用用途是构造和描述处理数据的公共模式.

Monoid 通常可以用于如下场景:

- 增量处理大型数据集的 API
- 保证并行, 并发或分布式处理框架中进行聚合操作

```haskell
Prelude Data.Monoid> foldr mappend mempty ([2,4,6] :: [Product Int])
Product {getProduct = 48}

Prelude Data.Monoid> foldr mappend mempty ([2,4,6] :: [Sum Int])
Sum {getSum = 12}
```

## 15.8 Laws

`Monoid` instances must abide by the following laws:

```haskell
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) =
  mappend (mappend x y) z

mconcat = foldr mappend mempty
```

## 15.9 Different instance, same representation

`Monoid` is somewhat different from other typeclasses in Haskell, in that many datatypes have more than one valid monoid.

> `Monoid` 在 Haskell 中和其他的 typeclass 有些不同, 即在很多 datatypes 中都有超过一个的 monoid.

Boolean values have two possible monoids -- a monoid of conjunction and one of disjunction. `All` for conjunction and `Any` for disjunction.

```haskell
Prelude> import Data.Monoid

Prelude> All True <> All True
All {getAll = True}
Prelude> All True <> All False
All {getAll = False}

Prelude> Any True <> Any False
Any {getAny = True}
Prelude> Any False <> Any False
Any {getAny = False}
```

The `Maybe` type has more than two possible `Monoids`. We'll look at each in turn, but the two that have an obvious relationship are `First` and `Last`.

`First` returns the first or leftmost non-Nothing value:

```haskell
Prelude> First (Just 1) `mappend` First (Just 2)
First {getFirst = Just 1}

Prelude> First Nothing `mappend` First (Just 2)
First {getFirst = Just 2}
```

`Last` returns the last or rightmost non-Nothing value:

```haskell
Prelude> Last (Just 1) `mappend` Last (Just 2)
Last {getLast = Just 2}

Prelude> Last (Just 1) `mappend` Last Nothing
Last {getLast = Just 1}
```

Neither can, for obvious reasons, return anything if all values are Nothing:

```haskell
Prelude> First Nothing `mappend` First Nothing
First {getFirst = Nothing}

Prelude> Last Nothing `mappend` Last Nothing
Last {getLast = Nothing}
```

## 15.9 Reusing algebras by asking for algebras

We alluded to there being more possible `Monoids` for `Maybe` that just `First` and `Last`. Let's write that other `Monoid` instance. We will now be concerned not with choosing one value out of a set of values but of combining the `a` values contained within the `Maybe a` type.

First, try to notice a pattern:

```haskell
instance Monoid b => Monoid (a -> b)

instance (Monoid a, Monoid b) =>
         Monoid (a, b)
instance (Monoid a, Monoid b, Monoid c) =>
         Monoid (a, b, c)
```

What these `Monoids` have in common is that they are giving you a new `Monoid` for a larger type by reusing the `Monoid` instances of types that represent components of the larger type.

> 这些 `Monoids` 的共同之处是, 它们通过重用代表较大类型组件的类型 `Monoid` 实例, 为你提供了一个新的 `Monoid`.

### Exercise: Optional Monoid

Write the `Monoid` instance for our `Maybe` type renamed to `Optional`.

```haskell
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance
  Semigroup a =>
  Semigroup (Optional a)
  where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only b) = Only $ a <> b
  Nada <> Nada = Nada

instance
  Monoid a =>
  Monoid (Optional a)
  where
  mempty = Nada
  mappend = (<>)
```

Expected output:

```haskell
Prelude> Only (Sum 1) `mappend` Only (Sum 1)
Only (Sum {getSum = 2})

Prelude> Only (Product 4) `mappend` Only (Product 2)
Only (Product {getProduct = 8})

Prelude> Only (Sum 1) `mappend` Nada
Only (Sum {getSum = 1})

Prelude> Only [1] `mappend` Nada
Only [1]

Prelude> Nada `mappend` Only (Sum 1)
Only (Sum {getSum = 1})
```
