module SubHask.Algebra.Trans.Kernel
    where

import qualified Prelude as P
import SubHask

kernel2distance :: (Floating (Scalar v), VectorSpace v) => (v -> v -> Scalar v) -> v -> v -> Scalar v
kernel2distance kernel v1 v2 = sqrt $ kernel v1 v1 - kernel v1 v2 - kernel v2 v1 + kernel v2 v2

-------------------------------------------------------------------------------
-- polynomial

newtype Polynomial (n::Nat) v = Polynomial v
    deriving
        (Read,Show,NFData,Arbitrary
        ,Eq,POrd,Ord,InfSemilattice,MinBound,SupSemilattice,MaxBound,Lattice
        ,Semigroup,Cancellative,Monoid,Abelian,Group
        )

type instance Scalar (Polynomial n v) = Scalar v
deriving instance (Module v) => Module (Polynomial n v)
deriving instance (VectorSpace v) => VectorSpace (Polynomial n v)

instance (KnownNat n, InnerProductSpace v) => MetricSpace (Polynomial n v) where
    distance = kernel2distance polykernel
        where
            polykernel (Polynomial v1) (Polynomial v2) = (1+v1<>v2)**n
            n = fromIntegral $ natVal (Proxy::Proxy n)

-------------------------------------------------------------------------------
-- ExponentialKernel

newtype ExponentialKernel (n::Nat) v = ExponentialKernel v
    deriving
        (Read,Show,NFData,Arbitrary
        ,Eq,POrd,Ord,InfSemilattice,MinBound,SupSemilattice,MaxBound,Lattice
        ,Semigroup,Cancellative,Monoid,Abelian,Group
        )

type instance Scalar (ExponentialKernel n v) = Scalar v
deriving instance (Module v) => Module (ExponentialKernel n v)
deriving instance (VectorSpace v) => VectorSpace (ExponentialKernel n v)

instance (KnownNat n, InnerProductSpace v) => MetricSpace (ExponentialKernel n v) where
    distance = kernel2distance rbf
        where
            rbf (ExponentialKernel v1) (ExponentialKernel v2) = exp $ -(abs $ v1 - v2) / sigma2
            sigma2=10

-------------------------------------------------------------------------------
-- RBF

newtype RBF (n::Nat) v = RBF v
    deriving
        (Read,Show,NFData,Arbitrary
        ,Eq,POrd,Ord,InfSemilattice,MinBound,SupSemilattice,MaxBound,Lattice
        ,Semigroup,Cancellative,Monoid,Abelian,Group
        )

type instance Scalar (RBF n v) = Scalar v
deriving instance (Module v) => Module (RBF n v)
deriving instance (VectorSpace v) => VectorSpace (RBF n v)

instance (KnownNat n, InnerProductSpace v) => MetricSpace (RBF n v) where
    distance = kernel2distance rbf
        where
            rbf (RBF v1) (RBF v2) = exp $ -(abs $ v1 - v2)**2 / sigma2
            sigma2=1/100

-------------------------------------------------------------------------------
-- Sigmoid

newtype Sigmoid (n::Nat) v = Sigmoid v
    deriving
        (Read,Show,NFData,Arbitrary
        ,Eq,POrd,Ord,InfSemilattice,MinBound,SupSemilattice,MaxBound,Lattice
        ,Semigroup,Cancellative,Monoid,Abelian,Group
        )

type instance Scalar (Sigmoid n v) = Scalar v
deriving instance (Module v) => Module (Sigmoid n v)
deriving instance (VectorSpace v) => VectorSpace (Sigmoid n v)

instance (InnerProductSpace v) => MetricSpace (Sigmoid n v) where
    distance = kernel2distance sigmoid
        where
            sigmoid (Sigmoid v1) (Sigmoid v2) = tanh $ alpha * v1<>v2 + beta
            alpha=1/10000
            beta=0
