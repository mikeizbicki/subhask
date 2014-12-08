-- | This module defines the algebra over various types of balls in metric spaces
module SubHask.Algebra.Metric
    where

import Control.Monad

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude

-------------------

-- |
--
-- FIXME: think carefully about whether these should all be open or closed
--
-- FIXME: think carefully about how the logic of the interval should relate to the logic of the metric space
data Box v = Box
    { smallest :: !v
    , largest :: !v
    }
    deriving (Read,Show)

type instance Scalar (Box v) = Scalar v
type instance Logic (Box v) = Logic v

instance (Eq v, Eq (Scalar v)) => Eq_ (Box v) where
    b1==b2 = smallest b1 == smallest b2 && largest b1 == largest b2

instance (Lattice v, MetricSpace v) => POrd_ (Box v) where
    inf b1 b2 = Box
        { smallest = sup (smallest b1) (smallest b2)
        , largest = inf (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => Lattice_ (Box v) where
    sup b1 b2 = Box
        { smallest = inf (smallest b1) (smallest b2)
        , largest = sup (largest b1) (largest b2)
        }

-------------------

-- | The open ball of a metric space assuming no special structure
--
-- FIXME: not implemented
data OpenBall v = OpenBall
    { radius :: !(Scalar v)
    , center :: !v
    }

type instance Scalar (OpenBall v) = Scalar v
type instance Logic (OpenBall v) = Logic v

instance (Eq v, Eq (Scalar v)) => Eq_ (OpenBall v) where
    b1 == b2 = radius b1 == radius b2 && center b1 == center b2

instance (Logic v~Bool, MetricSpace v) => Semigroup (OpenBall v) where
    b1+b2 = b1' { radius = distance (center b1') (center b2') + radius b2' }
        where
            (b1',b2') = if radius b1 > radius b2
                then (b1,b2)
                else (b2,b1)

-------------------

-- | The open ball of a metric space made more efficient relying on extra structure
--
-- FIXME: not implemented
newtype VectorBall v = VectorBall (OpenBall v)

type instance Scalar (VectorBall v) = Scalar v
