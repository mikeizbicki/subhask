{-# OPTIONS_GHC -XNoRebindableSyntax #-}

module SubHask.Compatibility.HMatrix
--     ( Matrix
--     , mkMatrix
--     , fromHMatrix
--     , trans
--     , toSingleton
--     , toVector
--     , vXm
--     , mXv
--     ( VS.Vector
--     , (+>)
--     , mkMatrix
--
--     , GL
--     , unsafeProveGL
--     , proveGL
--
--     , SO
--     , O
--     , Sp
--     , module SubHask.Algebra.Vector
--     )
    where

import qualified Prelude as P

import Control.DeepSeq
import Data.Typeable
import Foreign.Storable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import qualified Data.Packed.Matrix as HM
import qualified Numeric.LinearAlgebra as HM
import qualified Numeric.LinearAlgebra.HMatrix as HM (Numeric)
import qualified Numeric.LinearAlgebra.Devel as HM (GMatrix)

import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Compatibility.Vector
import SubHask.Category

import Debug.Trace

-------------------------------------------------------------------------------

data Matrix r
    = Zero
    | One !r
    | Matrix !(HM.Matrix r)
    deriving (Show,Typeable)

type instance Scalar (Matrix r) = r
type instance Logic (Matrix r) = Bool

type instance Matrix a >< b = MatrixOuterProduct (Matrix a) b
type family MatrixOuterProduct a b where
    MatrixOuterProduct (Matrix a) (Matrix b) = Matrix (a><b)
    MatrixOuterProduct (Matrix a) b = Matrix (a><b)

type instance Vector a >< b = VectorOuterProduct (Vector a) b
type family VectorOuterProduct a b where
    VectorOuterProduct (Vector a) (Vector b) = Matrix (a><b)
    VectorOuterProduct (Vector a) b = Vector (a><b)

instance (Storable r, NFData r) => NFData (Matrix r) where
    rnf Zero = ()
    rnf (One r) = ()
    rnf (Matrix m) = rnf m

---------

instance (P.Eq r, HM.Container HM.Matrix r) => Eq_ (Matrix r) where
    Zero == Zero = True
    Zero == _    = False
    _    == Zero = False
    (One r1) == (One r2) = r1 P.== r2
    (One r1) == _        = False
    _        == (One r2) = False
    (Matrix m1) == (Matrix m2) = m1 P.== m2

instance (P.Eq r, HM.Container HM.Matrix r) => Semigroup (Matrix r) where
    Zero + x = x
    x + Zero = x
    (One r1) + (One r2) = One $ r1 P.+ r2
    (One r1) + (Matrix m2) = Matrix $ (HM.scale r1 $ HM.ident (HM.rows m2)) `HM.add` m2
    (Matrix m1) + (One r2) = Matrix $ m1 `HM.add` (HM.scale r2 $ HM.ident (HM.rows m1))
    (Matrix m1) + (Matrix m2) = Matrix $ m1 `HM.add` m2

instance (P.Eq r, HM.Container HM.Matrix r) => Monoid (Matrix r) where
    zero = Zero

instance (P.Eq r, HM.Container HM.Matrix r) => Cancellative (Matrix r) where
    m1-m2 = m1 + negate m2

instance (P.Eq r, HM.Container HM.Matrix r) => Group (Matrix r) where
    negate Zero = Zero
    negate (One r) = One (-r)
    negate (Matrix m) = Matrix $ HM.scale (-1) m

instance (P.Eq r, HM.Container HM.Matrix r) => Abelian (Matrix r) where

instance (P.Eq r, HM.Container HM.Matrix r, HM.Product r) => Rg (Matrix r) where
    Zero * _ = Zero
    _ * Zero = Zero
    (One r) * (Matrix m) = Matrix $ HM.scale r m
    (Matrix m) * (One r) = Matrix $ HM.scale r m
    (Matrix m1) * (Matrix m2) = Matrix $ m1 HM.<> m2

instance (P.Eq r, HM.Container HM.Matrix r, HM.Product r) => Rig (Matrix r) where
    one = One 1

instance (P.Eq r, HM.Container HM.Matrix r, HM.Product r) => Ring (Matrix r) where
    fromInteger i = One $ P.fromInteger i

instance (P.Eq r, HM.Container HM.Matrix r, HM.Field r) => Field (Matrix r) where
    reciprocal Zero = error "recip zero"
    reciprocal (One r) = One $ 1 P./ r
    reciprocal (Matrix m) = Matrix $ HM.inv m

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , P.Eq r
    , IsScalar r
    , Ring r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => Module (Matrix r)
        where
    (Matrix m) .* r = Matrix $ HM.scale r m
    (One r2)   .* r = One $ r*r2
    Zero       .* r = Zero

    (Matrix m1) .*. (Matrix m2) = Matrix $ HM.mul m1 m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , P.Eq r
    , IsScalar r
    , Field r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => VectorSpace (Matrix r)
        where
    (Matrix m1) ./. (Matrix m2) = Matrix $ HM.divide m1 m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , HM.Numeric r
    , P.Eq r
    , IsScalar r
    , Field r
    , Floating r
    , Normed r
    , HM.Field r
    , VectorSpace r
    , ClassicalLogic r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => Banach (Matrix r)

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , HM.Numeric r
    , P.Eq r
    , HM.Field r
    , IsScalar r
    , Floating r
    , Normed r
    , Logic r~Bool
    , VectorSpace r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => Hilbert (Matrix r)
        where
    Zero <> _ = 0
    _ <> Zero = 0
    (One _) <> (One _) = error "inner product of ones undefined without length"
    (Matrix m) <> (One r) = HM.sumElements $ HM.scale r m
    (One r) <> (Matrix m) = HM.sumElements $ HM.scale r m
    (Matrix m1) <> (Matrix m2) = HM.flatten m1 `HM.dot` HM.flatten m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , HM.Numeric r
    , P.Eq r
    , IsScalar r
    , Floating r
    , Normed r
    , HM.Field r
    , Logic r~Bool
    , VectorSpace r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => Metric (Matrix r)
        where
    distance m1 m2 = innerProductDistance m2 m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , HM.Numeric r
    , P.Eq r
    , IsScalar r
    , Floating r
    , Normed r
    , HM.Field r
    , Logic r~Bool
    , VectorSpace r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => Normed (Matrix r)
        where
    size = innerProductNorm

---------

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , P.Eq r
    , IsScalar r
    , Field r
    , VectorSpace r
    , HM.Field r
    , VectorOuterProduct (Vector r) r ~ Vector r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => TensorAlgebra (Vector r)
        where
    v1 >< v2 = Matrix $ HM.outer v1 v2

    vXm v Zero = VS.map (const 0) v
    vXm v (One r) = VS.map (P.*r) v
    vXm v (Matrix m) = v `HM.vXm` m

    mXv Zero v = VS.map (const 0) v
    mXv (One r) v = VS.map (P.*r) v
    mXv (Matrix m) v = m `HM.mXv` v


instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , P.Eq r
    , IsScalar r
    , Field r
    , VectorSpace r
    , HM.Field r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    ) => TensorAlgebra (Matrix r)
        where
    Zero >< _ = Zero
    _ >< Zero = Zero
    (Matrix m1) >< (Matrix m2) = Matrix $ HM.kronecker m1 m2

---------

trans :: Matrix r -> Matrix r
trans (Matrix m) = Matrix $ HM.trans m
trans m = m

toSingleton :: (HM.Container HM.Matrix r, Field r) => Matrix r -> r
toSingleton (Matrix m) = if HM.rows m == 1 && HM.cols m == 1
    then HM.sumElements m
    else error "toSingleton on non singleton matrix"
toSingleton Zero = zero
toSingleton (One r) = r

toVector :: HM.Element r => Matrix r -> Vector r
toVector Zero = VG.empty
toVector (One r) = VG.singleton r
toVector (Matrix m) = HM.flatten m
-- toVector (Matrix m) = if HM.rows m == 1 || HM.cols m == 1
--     then HM.flatten m
--     else error "toVector called on non-vector matrix"

-------------------------------------------------------------------------------

fromHMatrix :: HM.Matrix r -> Matrix r
fromHMatrix = Matrix

mkMatrix :: Storable r => Int -> Int -> [r] -> Matrix r
mkMatrix r c xs = Matrix $ (r HM.>< c) xs

-- x = mkMatrix 3 3 [1,2,3,2,2,3,1,1,1] :: Matrix Double
-- y = mkMatrix 3 3 [2..10] :: Matrix Double
-- z = mkMatrix 3 2 [2..10] :: Matrix Double
-- t = mkMatrix 2 2 [2..10] :: Matrix Double


-------------------------------------------------------------------------------
-- moved from Vector for dependency issues

instance (Storable r, Module r, IsScalar (Scalar r), VectorOuterProduct (Vector r) (Scalar r) ~ Vector r) => Module (VS.Vector r) where

    {-# INLINE (.*) #-}
    v .* r = VG.map (r*.) v

    {-# INLINE (.*.) #-}
    u .*. v = if VG.length u == VG.length v
        then VG.zipWith (.*.) u v
        else error "(.*.): u and v have different lengths"

instance (Storable r, VectorSpace r, IsScalar (Scalar r), VectorOuterProduct (Vector r) (Scalar r) ~ Vector r) => VectorSpace (VS.Vector r) where
    {-# INLINE (./) #-}
    v ./ r = VG.map (./r) v

    {-# INLINE (./.) #-}
    u ./. v = if VG.length u == VG.length v
        then VG.zipWith (./.) u v
        else error "(./.): u and v have different lengths"

instance
    ( IsScalar r
    , Normed r
    , Logic r~Bool
    , VectorSpace r
    , Floating r
    , HM.Field r
    , VS.Storable r
    , VectorOuterProduct (Vector r) r ~ Vector r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    , P.Eq r
    ) => Normed (VS.Vector r)
        where
    size = innerProductNorm

instance
    ( IsScalar r
    , Normed r
    , Logic r~Bool
    , VectorSpace r
    , Floating r
    , HM.Field r
    , VS.Storable r
    , VectorOuterProduct (Vector r) r ~ Vector r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    , P.Eq r
    ) => Banach (VS.Vector r)

instance
    ( IsScalar r
    , Normed r
    , Logic r~Bool
    , VectorSpace r
    , Floating r
    , HM.Field r
    , VS.Storable r
    , VectorOuterProduct (Vector r) r ~ Vector r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    , P.Eq r
    ) => Metric (VS.Vector r)
        where
    distance = innerProductDistance

instance
    ( IsScalar r
    , Normed r
    , Logic r~Bool
    , VectorSpace r
    , Floating r
    , VS.Storable r
    , HM.Container Vector r
    , HM.Product r
    , HM.Field r
    , VectorOuterProduct (Vector r) r ~ Vector r
    , MatrixOuterProduct (Matrix r) r ~ Matrix r
    , P.Eq r
    ) => Hilbert (VS.Vector r)
        where
    v1 <> v2 = if VG.length v1 == 0
        then zero
        else if VG.length v2 == 0
            then zero
            else if VG.length v1 /= VG.length v2
                then error "inner product on storable vectors of different sizes"
                else VG.foldl' (+) zero $ VG.zipWith (*) v1 v2


type instance Index (VS.Vector r) = Int
type instance Elem (VS.Vector r) = r
type instance SetElem (VS.Vector r) b = VS.Vector b

instance (IsScalar r, VS.Storable r) => IxContainer (VS.Vector r) where
    lookup i s = s VG.!? i
    indices s = [0..VG.length s-1]
    values = VG.toList

instance (IsScalar r, Module r, VS.Storable r, VectorOuterProduct (Vector r) r ~ Vector r) => FiniteModule (VS.Vector r) where
    unsafeToModule = VG.fromList
