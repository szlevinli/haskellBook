# Haskell Programming from First Principles

这是关于 *Haskell Programming from First Principles* 这本书的阅读笔记.

> *First Principle* 是基本原理的意思

## Chapter 1: All You Need is Lambda

## Chapter 2: Hello, Haskell

- use Haskell code in the interactive environment and also from source files;
- understand the building blocks of Haskell: expression and functions;
- learn some features of Haskell syntax and conventions of good Haskell style;
- modify simple function.

## Chapter 3: String

- take an introductory look at types to understand the data structure called `String`;
- talk about the special syntax, or syntactic sugar, used for string;
- print strings in the REPL environment;
- work with some standard functions that operate on this datatype.

## Chapter 4: Basic datatypes

- review types we have seen in previous chapters;
- learn about datatypes, type constructors, and data constructors;
- work with predefined datatypes;
- learn more about type signatures and a bit about typeclasses.

## Chapter 5: Types

- learn more about querying and reading type signatures;
- see that currying has, unfortunately, nothing to do with food;
- take a closer look at different kinds of polymorphism;
- look at type inference and how to declare types for our functions.

## Chapter 6: Typeclasses

- examine the typeclasses `Eq`, `Num`, `Ord`, `Enum`, and `Show`;
- learn about type-defaulting typeclasses and typeclasses inheritance;
- look at some common but often implicit function that create side effects.

## Chapter 7: More function patterns

- Haskell functions are first-class entities that
- can be values in expressions, lists, or tuples;
- can be passed as arguments to a function;
- can be returned from a function as a result;
- make use of syntactic patterns.

## Chapter 8: Recursion

- explore what recursion is and how recursive functions evaluate;
- go step-by-step through the process of writing recursive functions;
- have fun with *bottom*.

## Chapter 9: Lists

- explain list's datatypes and how to pattern match on lists;
- practice many standard library functions for operating on lists;
- learn about the underlying representations of lists;
- see what that representation means for their evaluation;
- and do a whole bunch of exercise!

## Chapter 10: Folding lists

- explain what folds are and how they work;
- detail the evaluation processes of folds;
- walk through writing folding functions;
- introduce scans, functions that are related to folds;
- explain catamorphism.

## Chapter 11: Algebraic datatypes

- explain the "algebra" of algebraic datatypes;
- analyze the construction of data constructors;
- spell out when and how to write your own datatypes;
- clarify usage of type synonyms and `newtype`;
- introduce *kinds*;
- As-patterns (`@` symbol).

## Chapter 12: Signaling adversity

这章的标题指的的应该是在 Haskell 中的错误处理机制.

- `Nothing`, or `Just Maybe`;
- `Either` left or right, but not both;
- higher-kindedness, lifted and unlifted types;
- anamorphisms, but not animorphs.
