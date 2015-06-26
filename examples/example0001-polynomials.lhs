This first example shows how to use polynomials.
It should give you a taste of using categories for numerical applications.
First, some preliminaries:

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE RebindableSyntax #-}
> import SubHask
> import SubHask.Category.Polynomial
> import System.IO

We'll do everything within the `main` function so we can print some output as we go.

> main = do

To start off, we'll just create an ordinary function and print it's output.
The `Ring` class below corresponds very closely with the Prelude's `Num` class.

>   let f :: Ring x => x -> x
>       f x = x*x*x + x + 3
>
>   let a = 3 :: Integer
>
>   putStrLn $ "f a = " + show (f a)

Now, we'll create a polynomial from our ordinary function.

>   let g :: Polynomial Integer
>       g = provePolynomial f
>
>   putStrLn ""
>   putStrLn $ "g $ a = " + show ( g $ a )

The function `provePolynomial` above gives us a safe way to convert an arrow in Hask into an arrow in the category of polynomials.
The implementation uses a trick similar to automatic differentiation.
In general, every `Concrete` category has at least one similar function.
Finally, in order to apply our polynomial to a value, we must first convert it back into an arrow in Hask.
The function application operator `$` performs this task for us.

Polynomials support operations that other functions in Hask do not support.
For example, we can show the value of a polynomial:

>   putStrLn ""
>   putStrLn $ "g     = " + show g
>   putStrLn $ "g*g+g = " + show (g*g + g)

Polynomials also support decidable equality:

>   putStrLn ""
>   putStrLn $ "g==g     = " + show (g==g)
>   putStrLn $ "g==g*g+g = " + show (g==g*g+g)

Finally, we can create polynomials of polynomials:

>   let h :: Polynomial (Polynomial Integer)
>       h = provePolynomial f
>
>   putStrLn ""
>   putStrLn $ " h          = " + show h
>   putStrLn $ " h $ g      = " + show ( h $ g )
>   putStrLn $ "(h $ g) $ a = " + show (( h $ g ) $ a)

**For advanced readers:**
You may have noticed that function application on polynomials is equivalent to the join operation on monads.
That's because polynomials form a monad on Hask.
Sadly, we can't make `Polynomial` an instance of the new `Monad` class due to some limitatiions in GHC's type system.
This isn't too big of a loss though because I don't know of a useful application for this particular monad.
The monad described above is different than what category theorists call polynomial monads (see: http://ncatlab.org/nlab/show/polynomial+functor).

