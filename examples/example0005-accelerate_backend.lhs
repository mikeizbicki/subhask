
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE RebindableSyntax #-}
> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> import qualified Prelude as P
> import SubHask
> import SubHask.Algebra.Vector (ACCVector, UVector, mkAccVector, mkAccVectorFromList)
> import SubHask.Algebra.Matrix (ACCMatrix, Matrix, unsafeToModuleM, mkAccMatrixFromList, mkAccMatrixFromMatrix)
> import SubHask.Algebra.Accelerate (ValidBackend(..))
> import SubHask.Algebra.AccelerateBackend (Backend(..))
> import System.IO
>
> v  :: ACCVector Interpreter "a" Double
> v  = mkAccVectorFromList [0..5]
>
> v' :: ACCVector Interpreter "a" Double
> v' = mkAccVectorFromList [0..5]
>
>
> mmat  :: Matrix (UVector "v" Double) Double "a" "b"
> mmat  = unsafeToModuleM 2 [0..5]
>
> m :: ACCMatrix Interpreter (ACCVector Interpreter "v" Double ) "a" "b" Double
> m = mkAccMatrixFromMatrix mmat
>
> mm :: ACCMatrix Interpreter (ACCVector Interpreter "v" Double ) "a" "b" Double
> mm = mkAccMatrixFromList 2 [0,1,2,3,4,5,6,7,8,9,10]
>
> main :: IO ()
> main = do
>   putStrLn $ "v = " ++ show (runAccVector v)
>   putStrLn $ "v' = " ++ show (runAccVector v')
>   putStrLn $ "v + v = " ++ show (runAccVector (v + v))
>   putStrLn $ "v + v - v = " ++ show (runAccVector (v + v - v'))
>   putStrLn $ "v * v / v = " ++ show (runAccVector (v .*. v ./. v'))
>   putStrLn $ "v' *  2 = " ++ show (runAccVector (v' .* 2))
>   putStrLn $ "m +  m = " ++ show (runAccMatrix (m + m))
>   putStrLn $ "m +  mm = " ++ show (runAccMatrix (m + mm))
