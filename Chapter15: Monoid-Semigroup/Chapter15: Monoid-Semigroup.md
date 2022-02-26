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
