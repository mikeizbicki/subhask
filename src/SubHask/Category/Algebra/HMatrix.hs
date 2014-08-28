{-# OPTIONS_GHC -XNoRebindableSyntax #-}

module SubHask.Category.Algebra.HMatrix
--     ( VS.Vector
--     , (+>)
--     , mkMatrix
--     , trans
-- 
--     , GL
--     , unsafeProveGL
--     , proveGL
-- 
--     , SO
--     , O
--     , Sp
--     )
    where

import qualified Prelude as P

import Foreign.Storable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import qualified Numeric.LinearAlgebra as HM
import qualified Data.Packed.Matrix as HM

import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Algebra.Vector

import Debug.Trace

-------------------------------------------------------------------------------

-- mkMatrix :: Storable r => Int -> Int -> [r] -> (VS.Vector r `Linear` VS.Vector r)
-- mkMatrix r c xs = Matrix $ AddUnit' $ (r HM.>< c) xs
-- 
-- x = mkMatrix 3 3 [1..10] :: VS.Vector Double `Linear` VS.Vector Double
-- y = mkMatrix 3 3 [2..10] :: VS.Vector Double `Linear` VS.Vector Double
-- z = mkMatrix 3 2 [2..10] :: VS.Vector Double `Linear` VS.Vector Double
-- t = mkMatrix 2 2 [2..10] :: VS.Vector Double `Linear` VS.Vector Double
-- t' = unsafeProveGL t

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

instance HM.Container HM.Matrix r => Semigroup (HM.Matrix r) where
    m1 + m2 = m1 `HM.add` m2
--     m1 + m2 = HM.liftMatrix (VG.zipWith (+) $ HM.flatten m1) m2

