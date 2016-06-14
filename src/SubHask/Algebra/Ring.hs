module SubHask.Algebra.Ring
    where

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

--------------------------------------------------------------------------------

-- | Every free module can be converted into a ring with this type.
-- Intuitively, this lets us use all our code designed for univariate operations on vectors.
newtype Componentwise v = Componentwise { unComponentwise :: v }

type instance Scalar (Componentwise v) = Scalar v
type instance Logic (Componentwise v) = Logic v
type instance Elem (Componentwise v) = Scalar v
type instance SetElem (Componentwise v) v' = Componentwise v'

instance IsMutable (Componentwise v)

instance Eq_ v => Eq_ (Componentwise v) where
    (Componentwise v1)==(Componentwise v2) = v1==v2

instance Semigroup v => Semigroup (Componentwise v) where
    (Componentwise v1)+(Componentwise v2) = Componentwise $ v1+v2

instance Monoid v => Monoid (Componentwise v) where
    zero = Componentwise zero

instance Abelian v => Abelian (Componentwise v)

instance Cancellative v => Cancellative (Componentwise v) where
    (Componentwise v1)-(Componentwise v2) = Componentwise $ v1-v2

instance Group v => Group (Componentwise v) where
    negate (Componentwise v) = Componentwise $ negate v

instance FreeModule v => Rg (Componentwise v) where
    (Componentwise v1)*(Componentwise v2) = Componentwise $ v1.*.v2

instance FiniteModule v => Rig (Componentwise v) where
    one = Componentwise $ ones

instance FiniteModule v => Ring (Componentwise v)

instance (FiniteModule v, VectorSpace v) => Field (Componentwise v) where
    (Componentwise v1)/(Componentwise v2) = Componentwise $ v1./.v2
