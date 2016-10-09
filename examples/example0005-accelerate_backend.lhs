
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE RebindableSyntax #-}
> {-# LANGUAGE OverloadedLists #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> import qualified Prelude as P
> import SubHask
> import SubHask.Algebra.Accelerate.Vector (ACCVector, ValidBackend(..), mkAccVectorFromList)
> import SubHask.Algebra.Vector (UVector)
> --import SubHask.Algebra.Matrix (Matrix, unsafeToModuleM)
> --import SubHask.Algebra.Accelerate.Matrix (ACCMatrix, mmult)
> import SubHask.Algebra.Accelerate.AccelerateBackend (Backend(..))
> import System.IO
>
> v  :: ACCVector Interpreter 6 Double
> v  = mkAccVectorFromList [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
>
> v' :: ACCVector Interpreter 6 Double
> v' = mkAccVectorFromList [0..5]
>
>
> --mmat  :: Matrix (UVector "v" Double) Double "a" "b"
> --mmat  = unsafeToModuleM 2 [0..5]
>
> --m :: ACCMatrix Interpreter (ACCVector Interpreter "v" Double ) "a" "b" Double
> --m = mkAccMatrixFromMatrix mmat
>
> --mm :: ACCMatrix Interpreter (ACCVector Interpreter "v" Double ) "b" "a" Double
> --mm = mkAccMatrixFromList 5 [0,1,2,3,4,5,6,7,8,9]
>
> main :: IO ()
> main = do
>   putStrLn $ "v = " ++ show (runAccVector v)
>   putStrLn $ "v' = " ++ show (runAccVector v')
>   putStrLn $ "v + v = " ++ show (runAccVector (v + v))
>   putStrLn $ "v + v - v = " ++ show (runAccVector (v + v - v'))
>   putStrLn $ "v * v / v = " ++ show (runAccVector (v .*. v ./. v'))
>   putStrLn $ "v' *  2 = " ++ show (runAccVector (v' .* 2))
>   putStrLn $ "v' *  2 = " ++ show (runAccVector (v' .* 2))
>   --putStrLn $ "m *  2 = " ++ show (runAccMatrix (m .* 2))
>   --putStrLn $ "m + 2 = " ++ show (runAccMatrix ((m + 2) - 1 ))
>   --putStrLn $ "m /  2 = " ++ show (runAccMatrix (m / 2))
>   --putStrLn $ "m mmult  mm = " ++ show (runAccMatrix (mmult m mm))
