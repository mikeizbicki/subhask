Test of SubHAsk.Algebra.Matrix

>
> import SubHask
> import SubHask.Algebra.Matrix
> import SubHask.Algebra.Vector (UVector)
> import System.IO
>
> m  :: Matrix (UVector "v" Double) Double "a" "b"
> m  = unsafeToModuleM 3 [0..5]
> 
> m' :: Matrix (UVector "v" Double) Double "b" "c"
> m' = unsafeToModuleM 2 [0..5]
>
> main :: IO ()
> main = do
>   putStrLn $ "m = " ++ show m
>   putStrLn $ "m' = " ++ show m'
>   putStrLn $ "m + m = " ++ show (m+m)
>   putStrLn $ "m + zero = " ++ show (m+zero)
>   putStrLn $ "m - m = " ++ show (m-m)
>   putStrLn $ "m .*. m = " ++ show (m .*. m)
>   putStrLn $ "m ./. m = " ++ show (m ./. m)
>   putStrLn $ "m .+ 1 = " ++ show (m .+ 1)
>   putStrLn $ "m .* 10 = " ++ show (m .* 10)
>   putStrLn $ "mmult m m' = " ++ show (mmult m m')
>   putStrLn $ "(Mat m') . (Mat m) = " ++ show (Mat m' . Mat m)
>   putStrLn $ "(Mat m) . (Id 2.0) = " ++ show (Mat m . Id 2.0)
>
