# Signaling adversity

## 12.1 Signaling adversity

这章的标题指的的应该是在 Haskell 中的错误处理机制.

- `Nothing`, or `Just Maybe`;
- `Either` left or right, but not both;
- higher-kindedness;
- anamorphisms, but not animorphs.

## 12.2 How I learned to stop worrying and love Nothing

这章主要学习 `Maybe` 和 `Either`, 因为这两个数据类型比较好立即, 因此不再赘述.

## 12.3 Bleating Either

这章主要是用 `Maybe` 和 `Either` 解决实际的问题.

```haskell
module Chapter12 where

type Name = String

type Age = Integer

type ValidatePerson a =
  Either [PersonInvalid] a

data Person
  = Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Show)

ageOkay :: Age -> ValidatePerson Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name
  | name /= "" = Right name
  | otherwise = Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' ::
  ValidatePerson Name ->
  ValidatePerson Age ->
  ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
  Right $ Person nameOk ageOk
mkPerson' (Left badName) (Left badAge) =
  Left (badName ++ badAge)
mkPerson' (Left badName) _ =
  Left badName
mkPerson' _ (Left badAge) =
  Left badAge
```

## 12.4 Kinds, a thousand starts in your types

Kinds 是 type 的上一级. 通常用于描述 `type constructor` 的 type.

术语 *higher-kinded types* 源自 *higher-order function*, 意思是有参数的类型. `type constructor` 就是 *higher-kinded types*, 而 `type constant` 不是.

- `type constant` 表示其本身就是一个 type.
- `type constructor` 表示必须应用参数后才能成为一个 type.

Prelude 中的:

- type constant
  - Int
  - Bool
  - Char
- type constructor
  - Maybe
  - Either

在 GHCI 中使用 `:kind` 的返回结果也可以用于区别

- type constant
  - `*`
- type constructor
  - `* -> * -> ... -> *`

### Lifted and unlifted types

> 他们的概念至今也没很好的理解, 这里全文照抄待未来能够更好的理解他们的概念.

To be precise, kind * is the kind of all standard lifted types, while types that then kind # are unlifted.

> 确切的说, kind * 是所有标准 lifted types, 而具有 kind # 的类型是 unlifted types.

A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by *bottom*.

> A lifted type 包括您可以自己定义的任何数据类型, 它是任何可以被 *bottom* 占用的类型. (*不理解这里的 bottom 到底指的什么意思*)
>
> From HaskellWiki. The term bottom refers to a computation which never completes successfully. That includes a computation that fails due to some kind of error, and a computation that just goes into a infinite loop (without returning any data). The mathematical symbol for bottom is '⊥'.
>
> 来自 HaskellWiki 的解释. 术语 bottom 指的是永远无法成功完成的计算. 这包括由于某种错误而失败的计算, 以及进入无限循环(不返回任何数据)的计算. 底部的数学符号是 '⊥'.

Lifted types represented by a pointer and include most of the datatypes we've seen and most that you're likely to encounter and use.

> Lifted types 由指针表示, 包括大部分我们已经看到过的 datatypes, 以及你可能遇到和使用的大部分 datatypes.

Unlifted types are any type which *cannot* be inhabited by bottom.

> Unlifted types 是任何不能在底部居住的类型. (*依然不能理解这个 bottom 的含义*)

Types of kind # are often native machine types and raw points.

> Type of kind # 通常是原生的机器类型以及原始指针.

Newtypes are a special case in that they are kind *, but are unlifted because their representation is identical to that of the type they contains, so the newtype itself is not creating any new pointer beyond that of the type it contains.

> Newtypes 是一种特殊情况, 他们是 kind *, 但却是 unlifted type. 这是因为他们的表示和他们所包含的类型是一致的, 也就是说 newtype 自身并不会创建任何新的指针超过他们所包含的类型.

That fact means that the newtype itself cannot be inhabited by bottom, only the thing it contains can be, so newtypes are unlifted.

> 这一事实意味着 newtype 自身不能居住在 bottom, 只有它所包含的东西可以居住, 所以新类型是 unlifted.

### Data constructor are functions
