module SubHask.Algebra.Accelerate
(
  ValidBackend(..)
  -- , acc2SVector
)
where
  import SubHask.Algebra.AccelerateBackend (Backend(..))
  import SubHask.Algebra.Vector
  import SubHask.Algebra.Matrix
  import SubHask.Category
  import SubHask.Compatibility.Base
  import SubHask.SubType
  import qualified Data.Array.Accelerate as A
  import qualified Data.Array.Accelerate.Interpreter as I
  import qualified Data.Array.Accelerate.CUDA as CUDA
  -- import qualified Data.Array.Accelerate.LLVM as LLVM

  --needs to reside in the vector module but also needs acces to ValidBackend
  -- acc2SVector :: ValidACCVector (b::Backend) n a => ACCVector (b::Backend) n a  -> SVector n a
  -- acc2SVector (ACCVector v) = unsafeToModule $ A.toList (ACCVector (runAccVector v)) :: SVector n a


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
