# Haskell Programming from First Principles

这是关于 *Haskell Programming from First Principles* 这本书的阅读笔记.

> *First Principle* 是基本原理的意思

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
