This example introduces subhask's basic linear algebra system.
It starts with the differences between arrays and vectors,
then shows example manipulations on a few vector spaces,
and concludes with links to real world code.

But first the preliminaries:

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE RebindableSyntax #-}
> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
>
> import SubHask
> import SubHask.Algebra.Array
> import SubHask.Algebra.Vector
> import System.IO

We'll do everything within the `main` function so we can print some output as we go.

> main = do

Arrays vs. Vectors
=======================================

Vectors are the heart of linear algebra.
But before we talk about vectors, we need to talk about containers.
In particular, arrays and vectors are different in Subhask than in most standard libraries.
In the context of Subhask; arrays are generic containers suitable for storing both numeric and non-numeric values -
while vectors are elements of a vector space and come with a completely different set of laws.

There are three different types of arrays, each represented differently in memory.
The `BArray` is a boxed array, `UArray` is an unboxed array, and `SArray` is a storable array.

Because arrays are instances of `Constructable` and `Monoid`, they can be built using the `fromList` function.
With the `OverloadedLists` extension, this gives us the following syntax:

>   let arr = [1..5] :: UArray Int
>
>   putStrLn $ "arr  = " + show arr

Like arrays, vectors come in three forms (`BVector`, `UVector` and `SVector`).
We construct vectors using the `unsafeToModule` function.
(Vectors are a special type of module.)

>   let vec = unsafeToModule [1..5] :: SVector 5 Double
>
>   putStrLn $ "vec  = " + show vec

If the dimension of the vector is not known at compile time, it does not need to be specified in the type signature.
Instead, you can provide a string annotation in the type which will represent -- or act as a reference to -- it's size.
In addition, you can use any number of strings to reference the same size -- allowing for more flexible type signatures.

>   let vec' = unsafeToModule [1..5] :: SVector "datapoint" Double
>
>   putStrLn $ "vec' = " + show vec

The laws of the `Constructible` class, ensure that the `Monoid` instance concatenates two containers together.
Vectors are not `Constructible` because their `Monoid` instance is not concatenation.
Instead, it is component-wise addition on each of the elements.
Compare the following:

>   putStrLn ""
>   putStrLn $ "arr  + arr  = " + (show $ arr+arr)
>   putStrLn $ "vec  + vec  = " + (show $ vec+vec)
>   putStrLn $ "vec' + vec' = " + (show $ vec'+vec')

One commonality between vectors and arrays is that they are both indexed containers (i.e. instances of `IxContainer`).
This lets us look up a value in a specific instance using the `(!)` operator:

>   putStrLn ""
>   putStrLn $ "arr !0 = " + show (arr !0)
>   putStrLn $ "vec !0 = " + show (vec !0)
>   putStrLn $ "vec'!0 = " + show (vec'!0)

Unboxed arrays in subhask are more powerful than the unboxed vectors used in standard haskell.
For example, we can make an unboxed array of unboxed vectors like so:

>   let arr1 = fromList $ map unsafeToModule [[1,2],[2,3],[1,3]] :: UArray (UVector "a" Double)
>       arr2 = fromList $ map unsafeToModule [[1,2,2],[3,1,3]]   :: UArray (UVector "b" Double)
>
>   putStrLn ""
>   putStrLn $ "arr1!0 + arr1!1 = " + show (arr1!0 + arr1!1)
>   putStrLn $ "arr2!0 + arr2!1 = " + show (arr2!0 + arr2!1)

Notice how we did not have to know the sizes of the `UVector`s above at compile time in order to unbox them within the `UArray`.
Nonetheless, because we have annotated the sizes with different strings, the following code will not type check:

```
    putStrLn $ "arr1!0 + arr2!0 = " + show (arr1!0 + arr2!0)
```

And this is exactly what we want!
It doesn't make sense to add a vector of dimension 2 to a vector of dimension 3 however we, ourselves, may not know what kind of dimentionality we are dealing with.
Instead of requiring the us to have this knowledge, we can offload the work to the type system to prevent this!

I've found this distinction between vectors and arrays greatly simplifies the syntax when using linear algebra.


Instances of `IxContainers` are also updateable at a specific index with the `(!~)` operator. This is especially useful with the
`(&)`-Operator which is just flipped function-application:

>   putStrLn $ "vec & 3 !~ 42 = " + (show $ vec & 3 !~ 42)

Similarly we can also modify the entry with a function using `(%~)` and even combine this with the use of function-composition:

>   putStrLn $ "vec & 0 %~ (*5) = " + (show $ vec & 0 %~ (5))
>   putStrLn $ "vec & 4 %~ (\\x -> x*x) . 3 !~ 42 = " + (show $ vec & (4 %~ (\x -> x*x)) . (3 !~ 42))


Linear Algebra
=======================================

Let's create two vectors and show all the vector operations you might want to perform on them:

>   let u = unsafeToModule [1,1,1] :: SVector 3 Double
>       v = unsafeToModule [0,1,2] :: SVector 3 Double
>
>   putStrLn ""
>   putStrLn $ "add:           " + show (u + v)
>   putStrLn $ "sub:           " + show (u - v)
>   putStrLn $ "scalar mul:    " + show (5  *. u)
>   putStrLn $ "component mul: " + show (u .*. v)

Because `SVector` is not just a vector space but also a [hilbert space][hilbert-wiki] (i.e. instance of `Hilbert`),

we get the following operations as well:

>   putStrLn ""
>   putStrLn $ "norm:          " + show (size u)
>   putStrLn $ "distance:      " + show (distance u v)
>   putStrLn $ "inner product: " + show (u <> v)
>   putStrLn $ "outer product: " + show (u >< v)

Usually, people think of the outer product of two vectors is as a matrix.
But matrices are equivalent to linear functions, and that's the interpretation used in subhask.
The category `(+>)` (also called `Vect`) is the subcategory of `Hask` corresponding to linear functions.

The main advantage of this interpretation is that matrix multiplication becomes the same as function composition,
which also means that we can go about representing a matrix as two `SVector`s composed together with `(+>)`.

>   let matrix1 = u >< v :: SVector 3 Double +> SVector 3 Double
>
>   putStrLn ""
>   putStrLn $ "matrix1*matrix1 = " + show (matrix1*matrix1)
>   putStrLn $ "matrix1.matrix1 = " + show (matrix1.matrix1)

Square matrices (as shown above) are instances of the `Ring` type class.
But non-square matrices cannot be made instances of `Ring`
-- for more information on the ring algebraic structure, [see here][ring-wiki].
The reason is that the type signature for multiplication
```
(*) :: Ring r => r -> r -> r
```
requires that all input and output arguments have the same type.
This simple type signature is needed to support good error messages and type inference.
But function composition from the category class allows the arguments to differ:
```
(.) :: Category cat => cat b c -> cat a b -> cat a c
```
What's more, each of the `a`, `b`, and `c` type variables above corresponds to a dimension of matrix.
So the type system will ensure that your matrix multiplications actually make sense!

Here's an example:

>   let a = unsafeMkSMatrix 3 2 [1..6] :: SVector "a" Double +> SVector 3   Double
>       b = unsafeMkSMatrix 2 3 [1..6] :: SVector 3   Double +> SVector "a" Double
>       c = unsafeMkSMatrix 3 3 [1..9] :: SVector 3   Double +> SVector 3   Double
>
>   putStrLn ""
>   putStrLn $ "b.a     = " + show (b.a)
>   putStrLn $ "b.c.c.a = " + show (b.c.c.a)

Linear functions form a [subcategory of Hask][LinearObjects.hs],
and function application corresponds to right multiplying by a vector:

>   putStrLn ""
>   putStrLn $ "c $ u = " + show (c $ u)

When thinking of linear functions as matrices, the type signature may be slightly confusing.
A linear function that takes a vector of length n to a vector of length m corresponds to a matrix with n columns and m rows.
Thus, the type `SVector 3 Double +> SVector 2 Double` is the type of a 2 by 3 matrix.
The argument order of `unsafeMkSMatrix` is the standard "row, column" order, however.

Linear functions form what's known as a dagger catgory (i.e. `(+>)` is an instance of `Dagger`).
[Dagger categories][dagger-wiki] capture the idea of transposing a function and the ability to left multiply a vector.

>   putStrLn ""
>   putStrLn $ "trans c = " + show (trans c)
>   putStrLn $ "(trans c) $ u = " + show ((trans c) $ u)

Finally, there are many vector spaces besides the three `Vector` types.
For example, the linear functions above are finite dimensional vector spaces,
and ordinary haskell functions are actually infinite dimensional vector space!
Here they are in action:

>   let f x = x.*.x :: SVector 3 Double
>       g x = x + x :: SVector 3 Double
>
>   let h = f.*.g   :: SVector 3 Double -> SVector 3 Double
>
>   putStrLn ""
>   putStrLn $ "h u = " + show (h u)

Going further
=======================================

There's a lot of material about linear algebra this tutorial didn't cover.
You can see some real world machine learning examples in the the HLearn library.
A good place to start is the univariate optimization code:
https://github.com/mikeizbicki/HLearn/blob/master/src/HLearn/Optimization/Univariate.hs

Issues
=======================================

There's a number of warts still in the interface that I'm not pleased with.

* All of the array and vector types are currently missing many instances that they should have, but that I just haven't had time to implement.
I'd greatly appreciate any pull requests :)

* I'd like a good operator for function application on the left.
I think a mirror image dollar sign would work well, but I haven't found a unicode code point for that.

* Currently, you cannot make a multiparameter linear function (e.g. `a +> b +>`).
These multiparameter functions correspond to higher order tensors.
The reason for this limitation is type system issues I haven't figured out.

There are many more FIXME annotations documented in the code.

[LinearObjects.hs]: https://github.com/mikeizbicki/subhask/blob/master/src/SubHask/Category/Linear/Objects.hs
[dagger-wiki]: https://en.wikipedia.org/wiki/Dagger_category
[ring-wiki]: https://en.wikipedia.org/wiki/Ring_(mathematics)
[hilbert-wiki]: https://en.wikipedia.org/wiki/Hilbert_space

