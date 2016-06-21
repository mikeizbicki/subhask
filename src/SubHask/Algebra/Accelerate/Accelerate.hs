module SubHask.Algebra.Accelerate.AccelerateBackend
(
  ValidBackend(..)
  , mkAccVector
  , mkAccVectorFromList
  , mkAccMatrixFromList
  , mkAccMatrixFromMatrix
  , acc2SVector
)
where

import SubHask.Algebra.Accelerate.AccelerateBackend (Backend(..))

-- import qualified Data.Array.Accelerate.LLVM as LLVM
--FIXME:  Replace all intermediary lists with correct use of acclerate-io
mkAccVectorFromList :: A.Elt a => [a] -> ACCVector bknd (n::Symbol) a
mkAccVectorFromList l = let
    len = P.length l
  in ACCVector (A.use (A.fromList (A.Z A.:.len) l))

mkAccVector :: (A.Elt a, ValidSVector (n::Symbol) a) => SVector (n::Symbol) a -> ACCVector (bknd::Backend) (n::Symbol) a
mkAccVector v @(SVector_Dynamic fp off n) = let
  arr = A.fromList (A.Z A.:. n) $ unsafeInlineIO $ go (n-1) []
  go (-1) xs = return $ xs
  go i    xs = withForeignPtr fp $ \p -> do
      x <- peekElemOff p (off+i)
      go (i-1) (x:xs)
  in ACCVector (A.use arr)

--needs to reside in the vector module but also needs acces to ValidBackend
acc2SVector :: ValidACCVector (b::Backend) n a => ACCVector (b::Backend) n a  -> SVector n a
acc2SVector (ACCVector v) = unsafeToModule $ A.toList (ACCVector (runAccVector v)) :: SVector n a




class ValidBackend (b::Backend) where
    runAccVector :: (ValidACCVector (b::Backend) n a, A.IsScalar a) => ACCVector (b::Backend) n a -> [a]
    runAccMatrix :: (ValidACCMatrix (b::Backend) v m n r, A.IsScalar a, A.Elt a) => ACCMatrix (b::Backend) v n m a -> [a]

instance ValidBackend Interpreter where
    runAccVector (ACCVector a) =  A.toList (I.run a)
    runAccMatrix (ACCMatrix a) =  A.toList (I.run a)

instance ValidBackend CUDA where
    runAccVector (ACCVector a) = A.toList (CUDA.run a)
    runAccMatrix (ACCMatrix a) = A.toList (CUDA.run a)

-- instance ValidBackend LLVM where
--     runAccVector (ACCVector a) = A.toList (LLVM.run a)
--     runAccMatrix (ACCMatrix a) = A.toList (LLVM.run a)

-- instance ValidBackend Repa where
--     runAccVector (ACCVector a) = A.toList (Repa.run a)
--     runAccMatrix (ACCMatrix a) = A.toList (Repa.run a)
