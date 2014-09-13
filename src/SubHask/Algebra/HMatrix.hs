{-# OPTIONS_GHC -XNoRebindableSyntax #-}

module SubHask.Algebra.HMatrix
    ( Matrix
    , mkMatrix
    , fromHMatrix
    , trans
    , toSingleton
    , toVector
    , vXm
    , mXv
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
    , module SubHask.Algebra.Vector
    )
    where

import qualified Prelude as P

import Control.DeepSeq
import Data.Typeable
import Foreign.Storable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import qualified Numeric.LinearAlgebra as HM
import qualified Data.Packed.Matrix as HM

import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Algebra.Vector
import SubHask.Category

import Debug.Trace

-------------------------------------------------------------------------------

data Matrix r 
    = Zero
    | One !r
    | Matrix !(HM.Matrix r)
    deriving (Show,Typeable)

type instance Scalar (Matrix r) = r

instance (Storable r, NFData r) => NFData (Matrix r) where
    rnf Zero = ()
    rnf (One r) = ()
    rnf (Matrix m) = rnf m

---------

instance HM.Container HM.Matrix r => Semigroup (Matrix r) where
    Zero + x = x
    x + Zero = x
    (One r1) + (One r2) = One $ r1 P.+ r2
    (One r1) + (Matrix m2) = Matrix $ (HM.scale r1 $ HM.ident (HM.rows m2)) `HM.add` m2
    (Matrix m1) + (One r2) = Matrix $ m1 `HM.add` (HM.scale r2 $ HM.ident (HM.rows m1))
    (Matrix m1) + (Matrix m2) = Matrix $ m1 `HM.add` m2

instance HM.Container HM.Matrix r => Monoid (Matrix r) where
    zero = Zero

instance HM.Container HM.Matrix r => Group (Matrix r) where
    negate Zero = Zero
    negate (One r) = One (-r)
    negate (Matrix m) = Matrix $ HM.scale (-1) m

instance HM.Container HM.Matrix r => Abelian (Matrix r) where

instance (HM.Container HM.Matrix r, HM.Product r) => Rng (Matrix r) where   
    Zero * _ = Zero
    _ * Zero = Zero
    (One r) * (Matrix m) = Matrix $ HM.scale r m
    (Matrix m) * (One r) = Matrix $ HM.scale r m
    (Matrix m1) * (Matrix m2) = Matrix $ m1 HM.<> m2

instance (HM.Container HM.Matrix r, HM.Product r) => Ring (Matrix r) where   
    one = One 1

instance (HM.Container HM.Matrix r, HM.Field r) => Field (Matrix r) where
    reciprocal Zero = error "recip zero"
    reciprocal (One r) = One $ 1 P./ r
    reciprocal (Matrix m) = Matrix $ HM.inv m

instance 
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Ring r
    ) => Module (Matrix r) 
        where   
    r *. (Matrix m) = Matrix $ HM.scale r m
    r *. (One r2) = One $ r*r2
    r *. Zero = Zero

    (Matrix m1) .*. (Matrix m2) = Matrix $ HM.mul m1 m2

instance 
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Field r
    ) => VectorSpace (Matrix r) 
        where
    (Matrix m1) ./. (Matrix m2) = Matrix $ HM.divide m1 m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Floating r
    , Normed r
    ) => InnerProductSpace (Matrix r) 
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
    , IsScalar r
    , Floating r
    , Normed r
    ) => MetricSpace (Matrix r) 
        where
    distance m1 m2 = innerProductDistance m2 m2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Floating r
    , Normed r
    ) => Normed (Matrix r) 
        where
    abs = innerProductNorm 

---------

instance 
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Field r
    , VectorSpace r
    ) => OuterProductSpace (Vector r)   
        where
    type Outer (Vector r) = Matrix r
    v1 >< v2 = Matrix $ HM.outer v1 v2

instance
    ( HM.Container HM.Matrix r
    , HM.Product r
    , IsScalar r
    , Field r
    , VectorSpace r
    ) => OuterProductSpace (Matrix r)
        where
    type Outer (Matrix r) = Matrix r
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
toVector (Matrix m) = if HM.rows m == 1 || HM.cols m == 1
    then HM.flatten m
    else error "toVector called on non-vector matrix"

vXm :: (HM.Container HM.Matrix r, HM.Product r) => VS.Vector r -> Matrix r -> VS.Vector r
vXm v Zero = VS.map (const 0) v
vXm v (One r) = VS.map (P.*r) v
vXm v (Matrix m) = v `HM.vXm` m

mXv :: (HM.Container HM.Matrix r, HM.Product r) => Matrix r -> VS.Vector r -> VS.Vector r
mXv Zero v = VS.map (const 0) v
mXv (One r) v = VS.map (P.*r) v
mXv (Matrix m) v = m `HM.mXv` v

-------------------------------------------------------------------------------

fromHMatrix :: HM.Matrix r -> Matrix r
fromHMatrix = Matrix

mkMatrix :: Storable r => Int -> Int -> [r] -> Matrix r
mkMatrix r c xs = Matrix $ (r HM.>< c) xs

x = mkMatrix 3 3 [1,2,3,2,2,3,1,1,1] :: Matrix Double 
y = mkMatrix 3 3 [2..10] :: Matrix Double 
z = mkMatrix 3 2 [2..10] :: Matrix Double 
t = mkMatrix 2 2 [2..10] :: Matrix Double 

{-
-------------------

data family a +> b

newtype instance (VS.Vector r) +> (VS.Vector r) 
    = Linear1 (AddUnit' (HM.Matrix r))

newtype instance (VS.Vector r) +> (a +> b) 
    = Linear2 (V.Vector (a +> b))

newtype instance (a +> b) +> c
    = Linear3 (a +> (b +> c))

---------

class IdLinear a where
    idLinear :: a +> a

instance IdLinear (VS.Vector r) where
    idLinear = Linear1 Unit'

---------

class DotLinear a b c where
    dotLinear :: b +> c -> a +> b -> a +> c

instance HM.Product r => DotLinear (VS.Vector r) (VS.Vector r) (VS.Vector r) where
    dotLinear (Linear1 Unit') x = x
    dotLinear x (Linear1 Unit') = x
    dotLinear (Linear1 (AddUnit' m1)) (Linear1 (AddUnit' m2)) = Linear1 $ AddUnit' $ m1 HM.<> m2

instance DotLinear (VS.Vector r) (a +> b) (VS.Vector r) where
    dotLinear (Linear3 _) (Linear2 _) = undefined

-------------------

instance Category (+>) where
    type ValidCategory (+>) a b = 
        ( IdLinear a
        )

    {-# INLINE id #-}
    id = idLinear

--     {-# INLINE (.) #-}
--     f.g = dotLinear f g
--     (Linear f).(Linear g) = Linear $ f.g

-- instance SubCategory Linear (->) where
--     {-# INLINE embed #-}
--     embed (Linear f) = f
-- 
-- instance Monoidal Linear where
--     type ValidMonoidal' Linear a b = MkLinearTensor a b
--     type Tensor Linear = (><)
--     type Unit Linear = ()
-- 
--     {-# INLINE tensor #-}
--     tensor = mkLinearTensor

-- instance Dagger (Linear r) where
--     {-# INLINE dagger #-}
--     dagger (Matrix Unit') = Matrix Unit'
--     dagger (Matrix (AddUnit' m)) = Matrix $ AddUnit' $ HM.trans m

-- {-# INLINE trans #-}
-- trans :: 
--     ( IsScalar r
--     , HM.Container HM.Vector r
--     , HM.Product r
--     ) => (VS.Vector r `Linear` VS.Vector r) -> (VS.Vector r `Linear` VS.Vector r)
-- trans = dagger

-------------------

-- class MkLinearTensor a b where
--     mkLinearTensor :: Linear a (Linear b (a><b))
-- 
-- type Matrix r = VS.Vector r >< VS.Vector r
-- 
-- data family (a><b) 
-- newtype instance (VS.Vector r >< VS.Vector r) = Matrix (AddUnit' (HM.Matrix r))
-}
