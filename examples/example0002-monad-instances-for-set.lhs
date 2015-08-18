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
Here we define a set, two functions, and map those functions onto the set.

>   let xs = [1..5] :: LexSet Int
>
>   let f :: Semigroup a => a -> a
>       f x = x+x                               -- monotonic
>       g :: (Eq a, Integral a, Logic a ~ Bool) => a -> a
>       g x = if x`mod`2 == 0 then x else -x    -- not monotonic
>
>   let fxs :: LexSet Int
>       fxs = fmap (proveOrdHask f) $ xs
>       gxs :: LexSet Int
>       gxs = fmap (proveOrdHask g) $ xs
>
>   putStrLn $ "xs  = " + show xs
>   putStrLn $ "fxs = " + show fxs
>   putStrLn $ "gxs = " + show gxs

There's a few important points about the code above:

*   The `LexSet` type above is a simple wrapper around the `Set` container from the containers package.
    In SubHask, the `Lattice` instance for `Set` (without the prefix) is based on the subset relation.
    This ordering is not total,
    which means `Set` is not an instance of `Ord`,
    which means we cannot have a `Set` of a `Set`.
    The `LexSet` uses lexical ordering.
    This ordering is total, and therefore we can have sets of sets.

*   When we map a function over a container, we must explicitly say which `Functor` instance we want to use.
    The `proveOrdHask` functions transform the functions from arrows in `Hask` to arrows in the `OrdHask` category.
    The program would not type check without these "proofs."

Now let's see the `Functor Mon LexSet` instance in action.
GHC can mechanistically prove when a function in `Hask` belongs in `OrdHask`,
but there it cannot prove when functions in `OrdHask` also belong to `Mon`.
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

>   let oddneg :: Int `OrdHask` (LexSet Int)
>       oddneg = proveConstrained f
>         where
>             f :: (Integral a, Ord a, Logic a ~ Bool) => a -> LexSet a
>             f i = if i `mod` 2 == 0
>                 then [i]
>                 else [-i]
>
>   let times3 :: (Ord a, Ring a) => a `OrdHask` (LexSet a)
>       times3 = proveConstrained f
>         where
>             f :: (Ord a, Ring a) => a -> LexSet a
>             f a = [a,2*a,3*a]
>
>   let times3mon :: (Ord a, Ring a) => a `Mon` (LexSet a)
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
