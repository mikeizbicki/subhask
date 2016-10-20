
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
> --import SubHask.Algebra.Matrix (Matrix, unsafeToModuleM)
> --import SubHask.Algebra.Accelerate.Matrix (ACCMatrix, mmult
> import SubHask.Algebra.Array
> import SubHask.Algebra.Vector
> import qualified Data.Array.Accelerate as A
> import SubHask.Algebra.Accelerate.AccelerateBackend (Backend(..))
> import System.IO
>
> v  :: ACCVector Interpreter 4 Float
> v  = mkAccVectorFromList [0.0, 1.0, 2.0, 3.0]
>
> v' :: ACCVector Interpreter 4 Float
> v' = mkAccVectorFromList [0..3]
> sngtln = (A.constant 2.0)
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
>   putStrLn $ "v = " + show v
>   putStrLn $ "v' = " + show v'
>   putStrLn $ "v + v = " + (show $ runAccVector (v + v))
>   putStrLn $ "v + v - v = " + (show $ runAccVector(v + v - v'))
>   putStrLn $ "v * v = " + (show $ runAccVector (v .*. v ))
>   putStrLn $ "v / v' = " + (show $ runAccVector (v ./. v'))
>   putStrLn $ "v * v / v' = " + (show $ runAccVector (v .*. v ./. v'))
>   putStrLn $ "v' .*  2 = " + (show $ runAccVector (v' .* sngtln))
>   putStrLn $ "v' ./  2 = " + (show $ runAccVector (v' ./ sngtln))
>   putStrLn $ "v >< v' = " + (show $ runAccVector (v >< v))
>   putStrLn $ "v**2 = " + (show $ runAccVector (v**v'))
>   --putStrLn $ "m *  2 = " ++ show (runAccMatrix (m .* 2))
>   --putStrLn $ "m + 2 = " ++ show (runAccMatrix ((m + 2) - 1 ))
>   --putStrLn $ "m /  2 = " ++ show (runAccMatrix (m / 2))
>   --putStrLn $ "m mmult  mm = " ++ show (runAccMatrix (mmult m mm))
