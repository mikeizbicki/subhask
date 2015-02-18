module SubHask.Algebra.Trans.Kernel
    where

import qualified Prelude as P
import SubHask
import SubHask.TemplateHaskell.Deriving

kernel2distance :: (Floating (Scalar v), VectorSpace v) => (v -> v -> Scalar v) -> v -> v -> Scalar v
kernel2distance kernel v1 v2 = sqrt $ kernel v1 v1 - kernel v1 v2 - kernel v2 v1 + kernel v2 v2

-------------------------------------------------------------------------------
-- generic

-- FIXME: use a dependently typed kernel like this for everything
-- create a couple of standard static ones
data WithKernel (kernel::k) v where
    WithKernel :: (v -> v -> Scalar v) -> v -> WithKernel k v

type instance Scalar (WithKernel k v) = Scalar v
type instance Logic (WithKernel k v) = Logic v

instance Eq v => Eq_ (WithKernel k v) where
    (WithKernel _ v1)==(WithKernel _ v2) = v1==v2

instance
    ( Ord (Scalar v)
    , Floating (Scalar v)
    , VectorSpace v
    , Eq v
    ) => MetricSpace (WithKernel k v)
        where
    distance (WithKernel k v1) (WithKernel _ v2) = kernel2distance k v1 v2

sameKernel :: WithKernel k1 v -> WithKernel k2 v -> WithKernel k2 v
sameKernel (WithKernel k1 v1) (WithKernel k2 _) = WithKernel k2 v1

-------------------------------------------------------------------------------
-- polynomial

newtype Polynomial (n::Nat) v = Polynomial v

deriveHierarchy ''Polynomial
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    ]

instance (KnownNat n, InnerProductSpace v) => MetricSpace (Polynomial n v) where
    distance = kernel2distance polykernel
        where
            polykernel (Polynomial v1) (Polynomial v2) = (1+v1<>v2)**n
            n = fromIntegral $ natVal (Proxy::Proxy n)

-------------------------------------------------------------------------------
-- ExponentialKernel

newtype ExponentialKernel (n::Nat) v = ExponentialKernel v

deriveHierarchy ''ExponentialKernel
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    ]

instance (KnownNat n, InnerProductSpace v) => MetricSpace (ExponentialKernel n v) where
    distance = kernel2distance rbf
        where
            rbf (ExponentialKernel v1) (ExponentialKernel v2) = exp $ -(size $ v1 - v2) / sigma2
            sigma2=10

-------------------------------------------------------------------------------
-- RBF

newtype RBF (n::Nat) v = RBF v

deriveHierarchy ''RBF
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    ]

instance (KnownNat n, InnerProductSpace v) => MetricSpace (RBF n v) where
    distance = kernel2distance rbf
        where
            rbf (RBF v1) (RBF v2) = exp $ -(size $ v1 - v2)**2 / sigma2
            sigma2=1/100

-------------------------------------------------------------------------------
-- Sigmoid

newtype Sigmoid (n::Nat) v = Sigmoid v

deriveHierarchy ''Sigmoid
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    ]

instance (InnerProductSpace v) => MetricSpace (Sigmoid n v) where
    distance = kernel2distance sigmoid
        where
            sigmoid (Sigmoid v1) (Sigmoid v2) = tanh $ alpha * v1<>v2 + beta
            alpha=1/10000
            beta=0
