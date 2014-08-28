module SubHask.Category.Algebra.Vector
    (
    )
    where

import Foreign.Storable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Category

-------------------------------------------------------------------------------

u = VG.fromList [1..3] :: VS.Vector Float
v = VG.fromList [1..2] :: VS.Vector Float

instance (Storable r, Monoid r) => Semigroup (VS.Vector r) where
    {-# INLINE (+) #-}
    v1 + v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "VS.Vector + two different non-zero lengths"
                else VG.generate (VG.length v1) go 
        where
            go i = v1 `VG.unsafeIndex` i + v2 `VG.unsafeIndex` i

instance (Storable r, Monoid r) => Monoid (VS.Vector r) where
    {-# INLINE zero #-}
    zero = VG.empty

instance (Storable r, Abelian r) => Abelian (VS.Vector r)

instance (Storable r, Group r) => Group (VS.Vector r) where
    {-# INLINE negate #-}
    negate v = VG.map negate v

type instance Scalar (VS.Vector r) = Scalar r

instance (Storable r, Module r, IsScalar (Scalar r)) => Module (VS.Vector r) where
    {-# INLINE (.*) #-}
    r .* v = VG.map (r.*) v

instance (Storable r, VectorSpace r, IsScalar (Scalar r)) => VectorSpace (VS.Vector r) where
    {-# INLINE (/.) #-}
    v /. r = VG.map (/.r) v

instance (IsScalar r, VectorSpace r, Field r, VS.Storable r) => InnerProductSpace (VS.Vector r) where
    v1 <> v2 = if VG.length v1 == 0
        then zero
        else if VG.length v2 == 0
            then zero
            else if VG.length v1 /= VG.length v2
                then error "inner product on storable vectors of different sizes"
                else VG.foldl' (+) zero $ VG.zipWith (*) v1 v2


