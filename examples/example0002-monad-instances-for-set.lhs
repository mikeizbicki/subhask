In this example, we will use two different monad instances on sets.
In standard haskell, this is impossible because sets require an `Ord` constraint;
but in subhask we can make monads that require constraints.
The key is that set is not a monad over Hask.
It is a monad over the subcategories `OrdHask` and `Mon`.
`OrdHask` contains only those objects in Hask that have `Ord` constraints.
`Mon` is the subcategory on `OrdHask` whose arrows are monotonic functions.

Now for the preliminaries:

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE RebindableSyntax #-}
> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
>
> import SubHask
> import SubHask.Category.Trans.Constrained
> import SubHask.Category.Trans.Monotonic
> import SubHask.Compatibility.Containers
> import System.IO

We'll do everything within the `main` function so we can print some output as we go.

> main = do

Before we get into monads, let's take a quick look at the `Functor` instances.
We start by defining a set:

>   let xs = [1..5] :: LexSet Int

There are multiple types for sets in SubHask, each with slightly different semantics.
The `LexSet` type has semantics similar to the `Set` type from the containers package.
In particular, the `Lex` stands for "lexical" because the `Lattice` instance corresponds to a lexical ordering.
The `Set` type in SubHask uses the more traditional subset ordering for its `Lattice` instance.
`Set` is not an instance of `Functor` or `Monad`, so we don't use it in this example.

Next, we'll create two set functions and map those functions onto the set `xs`.
The type signatures below are not mandatory, just added for clarity.

>   -- f is monotonic
>   let f :: Semigroup a => a -> a
>       f x = x+x
>
>       fxs :: LexSet Int
>       fxs = fmap (proveOrdHask f) $ xs
>
>   -- g is not monotonic
>   let g :: (Eq a, Integral a, ClassicalLogic a) => a -> a
>       g x = if x`mod`2 == 0 then x else -x
>
>       gxs :: LexSet Int
>       gxs = fmap (proveOrdHask g) $ xs
>
>   putStrLn $ "xs  = " + show xs
>   putStrLn $ "fxs = " + show fxs
>   putStrLn $ "gxs = " + show gxs

Notice in the code above that when we call `fmap`, we also called the function `proveOrdHask`.
When we map a function over a container, we must explicitly say which `Functor` instance we want to use.
The `proveOrdHask` function transform the functions from arrows in `Hask` to arrows in the `OrdHask` category.
The program would not type check without these "proofs."

Now let's see the `Functor Mon LexSet` instance in action.
This instance applies monotonic functions to the elements of the set.
Monotonic functions can be applied in time O(n), whereas non-monotonic functions take time O(n*log n).

GHC can mechanistically prove when a function in `Hask` belongs in `OrdHask`,
but it cannot always prove when functions in `OrdHask` also belong to `Mon`.
(This proof would require dependent types.)
Therefore we must use the `unsafeProveMon` function, as follows:

>   let fxs' = fmap (unsafeProveMon f) $ xs
>       gxs' = fmap (unsafeProveMon g) $ xs
>
>   putStrLn ""
>   putStrLn $ "fxs' = " + show fxs'
>   putStrLn $ "gxs' = " + show gxs'

Notice that we were able to use the `Functor Mon` instance on the non-monotonic function `g`.
But since the `g` function is not in fact monotonic, the mapping did not work correctly.
Notice that equality checking is now broken:

>   putStrLn ""
>   putStrLn $ "fxs == fxs' = " + show (fxs == fxs')
>   putStrLn $ "gxs == gxs' = " + show (gxs == gxs')

We're now ready to talk about the `Monad` instances.
To test it out, we'll create two functions, the latter of which is monotonic.
The type signatures are provided only to aide reading.

>   let oddneg :: Int `OrdHask` (LexSet Int)
>       oddneg = proveConstrained f
>         where
>             f :: (Integral a, Ord a, ClassicalLogic a) => a -> LexSet a
>             f i = if i `mod` 2 == 0
>                 then [i]
>                 else [-i]
>
>   let times3 :: (Ord a, Ring a, ClassicalLogic a) => a `OrdHask` (LexSet a)
>       times3 = proveConstrained f
>         where
>             f :: (Ord a, Ring a, ClassicalLogic a) => a -> LexSet a
>             f a = [a,2*a,3*a]
>
>   let times3mon :: (Ord a, Ring a, ClassicalLogic a) => a `Mon` (LexSet a)
>       times3mon = unsafeProveMon (times3 $)
>
>   putStrLn ""
>   putStrLn $ "xs >>= oddneg    = " + show (xs >>= oddneg)
>   putStrLn $ "xs >>= times3    = " + show (xs >>= times3)
>   putStrLn $ "xs >>= times3mon = " + show (xs >>= times3mon)

One of the main advantages of monads is do notation.
Unfortunately, that's only partially supported at the moment.
Consider the do block:
```
do
    x <- xs
    times3 x
```
which gets desugared as:
```
xs >>= (\x -> times3 x)
```
The above code doesn't type check because the lambda expression is an arrow in Hask,
but we need an arrow in OrdHask.
This problem can be fixed by modifying the syntactic sugar of the do block to prefix its lambdas with a proof statement.
But for now, you have to do the desugaring manually.
