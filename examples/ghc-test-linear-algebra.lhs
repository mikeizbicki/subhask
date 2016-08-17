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
> import Data.Primitive.Types

> main = do
>
>   let v = unsafeToModule [1..5] :: UVector "foo" Double
>
>   putStrLn $ " v = " + show v
>   w <- return $ v & (3 !~ 0)
>   putStrLn  $ " w = " + show w
>
>
>   let v' = unsafeToModule [1..5] :: UVector "bar" Double
>   putStrLn $ " v' = " + show v'
>   w' <- return $ v' & (1 !~ 42) . (2 %~ (+5)) . (1 !~ 5) . (3 !~ 0) . (1 !~ 0) . (1 !~ 42)
>   putStrLn  $ " w' = " + show w'
