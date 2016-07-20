module SubHask.Algebra.Accelerate.AccelerateBackend
(
  Backend(..),
)
where


data Backend
    = Interpreter
    | CUDA
    -- | LLVM
    -- LLVM has an SoC project slated, so check back in 60 days for non-parial functionality
