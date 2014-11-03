-- | This module defines the algebra over various types of balls in metric spaces
module SubHask.Algebra.Ball
    where

import Control.Monad

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude

-------------------

data Box v = Box
    { smallest :: !v
    , largest :: !v
    }
    deriving (Read,Show)

type instance Scalar (Box v) = Scalar v

instance (Eq v, Eq (Scalar v)) => Eq (Box v) where
    b1==b2 = smallest b1 == smallest b2 && largest b1 == largest b2

instance (POrd v, MetricSpace v) => POrd (Box v) where
    pcompare b1 b2 = if b1 == b2
        then PEQ
        else if smallest b1 > smallest b2 && largest b1 < largest b2
            then PLT
            else if smallest b1 < smallest b2 && largest b1 > largest b2
                then PGT
                else PNA

instance (Lattice v, MetricSpace v) => Lattice (Box v)

instance (Lattice v, MetricSpace v) => InfSemilattice (Box v) where
    inf b1 b2 = Box
        { smallest = sup (smallest b1) (smallest b2)
        , largest = inf (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => SupSemilattice (Box v) where
    sup b1 b2 = Box
        { smallest = inf (smallest b1) (smallest b2)
        , largest = sup (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => Semigroup (Box v) where
    b1+b2 = Box
        { smallest = inf (smallest b1) (smallest b2)
        , largest = sup (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => Abelian (Box v) where

-- instance (Lattice v, POrd v, MetricSpace v) => Container (Box v) where
--     type Elem (Box v) = v
--     elem x b = x > smallest b && x < largest b

-------------------

data OpenBall v = OpenBall
    { radius :: !(Scalar v)
    , center :: !v
    }

type instance Scalar (OpenBall v) = Scalar v

instance (Eq v, Eq (Scalar v)) => Eq (OpenBall v) where
    b1 == b2 = radius b1 == radius b2 && center b1 == center b2

instance MetricSpace v => POrd (OpenBall v) where
    pcompare b1 b2 = if dist == 0 && radius b1 == radius b2
        then PEQ
        else if dist <= radius b1 - radius b2
            then PGT
            else if dist <= radius b2 - radius b1
                then PLT
                else PNA
        where
            dist = distance (center b1) (center b2)

-- instance MetricSpace v => Lattice (OpenBall v) where
--     inf = error "FIXME: inf OpenBall"
--     sup = error "FIXME: sup OpenBall"

instance MetricSpace v => Semigroup (OpenBall v) where
    b1+b2 = b1' { radius = distance (center b1') (center b2') + radius b2' }
        where
            (b1',b2') = if radius b1 > radius b2
                then (b1,b2)
                else (b2,b1)

instance MetricSpace v => Abelian (OpenBall v)

-- instance MetricSpace v => Container (OpenBall v) where
--     type Elem (OpenBall v) = v
--     elem x b = not $ isFartherThan x (center b) (radius b)

-------------------

newtype VectorBall v = VectorBall (OpenBall v)

type instance Scalar (VectorBall v) = Scalar v

-- instance InnerProductSpace v => Lattice (VectorBall v) where
--     (VectorBall b1) || (VectorBall b2) = VectorBall $ OpenBall
--         { radius = (distance (center b1) (center b2) + radius b1 + radius b2) / 2
--         , center = ( (1-radius b2+radius b1)*.center b1
--                    + (1-radius b1+radius b2)*.center b2
--                    ) ./ 2
--         }
--
--     -- FIXME: this is incorrect!
--     (VectorBall b1) && (VectorBall b2) = VectorBall $ OpenBall
--         { radius = max 0 $ radius b1 + radius b2 - 2*distance (center b1) (center b2)
--         , center = (radius b1 *. center b1 + radius b2 *. center b2) ./ (radius b1 + radius b2)
--         }


-- class InnerProductSpace v => RepresentableVector v where
--     getBasis :: Int -> v
--
--     (!) :: v -> Int -> Scalar v
--     (!) v i = getBasis i <> v

