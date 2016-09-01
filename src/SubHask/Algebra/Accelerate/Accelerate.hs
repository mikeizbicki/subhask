module SubHask.Algebra.Accelerate.Accelerate
(
  ValidBackend(..)
  -- , runAccVector
  , mkAccVectorFromList
  -- , mkAccMatrixFromList
  -- , mkAccMatrixFromMatrix
  --, acc2SVector
)
where

import SubHask.Algebra.Accelerate.AccelerateBackend (Backend(..))
import SubHask.Algebra.Accelerate.Vector
-- import SubHask.Algebra.Accelerate.Matrix
import qualified Data.Array.Accelerate as A
-- import qualified Data.Array.Accelerate.LLVM.Array.Data as LLVM
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Interpreter as I
import SubHask.Internal.Prelude
import qualified Prelude as P

--FIXME:  Replace all intermediary lists with correct use of acclerate-io
mkAccVectorFromList :: A.Elt a => [a] -> ACCVector bknd (n::Nat) a
mkAccVectorFromList l = let
    len = P.length l
  in ACCVector (A.use (A.fromList (A.Z A.:.len) l))

-- mkAccVector :: (A.Elt a, ValidSVector (n::Symbol) a) => SVector (n::Symbol) a -> ACCVector (bknd::Backend) (n::Symbol) a
-- mkAccVector v @(SVector_Dynamic fp off n) = let
--   arr = A.fromList (A.Z A.:. n) $ unsafeInlineIO $ go (n-1) []
--   go (-1) xs = return $ xs
--   go i    xs = withForeignPtr fp $ \p -> do
--       x <- peekElemOff p (off+i)
--       go (i-1) (x:xs)
--   in ACCVector (A.use arr)

-- acc2SVector :: ValidACCVector (b::Backend) n a => ACCVector (b::Backend) n a  -> SVector n a
-- acc2SVector (ACCVector v) = unsafeToModule $ (runAccVector v) :: SVector n a


class ValidBackend (b::Backend) where
    runAccVector :: (ValidACCVector (b::Backend) n a) => ACCVector (b::Backend) n a -> [a]
    -- runAccMatrix :: (ValidACCMatrix (b::Backend) v r, A.IsScalar a, A.Elt a) => ACCMatrix (b::Backend) v n m a -> [a]

instance ValidBackend 'Interpreter where
    runAccVector (ACCVector a) =  A.toList (I.run a)
    -- runAccMatrix (ACCMatrix a) =  A.toList (I.run a)

instance ValidBackend 'CUDA where
    runAccVector (ACCVector a) = A.toList (CUDA.run a)
    -- runAccMatrix (ACCMatrix a) = A.toList (CUDA.run a)

-- instance ValidBackend LLVM where
--     runAccVector (ACCVector a) = A.toList (LLVM.runArray a)
--     runAccMatrix (ACCMatrix a) = A.toList (LLVM.runArray a)
