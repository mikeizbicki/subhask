{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module SubHask.Compatibility.Vector.Lebesgue
--     ( L2 (..)
--     , L1 (..)
--     , Linf (..)
--     )
    where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Csv
import Data.Primitive.MutVar
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import System.IO.Unsafe

import Test.QuickCheck

import qualified Prelude as P
import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return, liftM)

import Data.Primitive
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Unsafe.Coerce


-------------------------------------------------------------------------------
-- L1

newtype L1 v a = L1 { unL1 :: v a }
    deriving (Read,Show,POrd_,Lattice_,Ord_,Arbitrary,FromRecord,NFData, P.Ord)

type instance Logic (L1 v a) = Logic (v a)

deriving instance Eq_ (v a) => Eq_ (L1 v a)
deriving instance F.Foldable v => F.Foldable (L1 v)
-- deriving instance Functor v => Functor (L1 v)

instance VG.Vector v a => VG.Vector (L1 v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (L1M v) = liftM L1 $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (L1 v) = liftM L1M $ VG.basicUnsafeThaw v
    basicLength (L1 v) = VG.basicLength v
    basicUnsafeSlice s t (L1 v) = L1 $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (L1 v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (L1M vm) (L1 v) = VG.basicUnsafeCopy vm v
    elemseq (L1 v) a b = VG.elemseq v a b

newtype L1M v s a = L1M { unL1M :: v s a }

instance VGM.MVector v a => VGM.MVector (L1M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (L1M v) = VGM.basicLength v
    basicUnsafeSlice s t (L1M v) = L1M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (L1M v1) (L1M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM L1M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM L1M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (L1M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (L1M v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (L1 v) = L1M (VG.Mutable v)

---------------------------------------

type instance Scalar (L1 v r) = r


instance
    ( VG.Vector v r
    , Eq (v r)
    , Floating r
    , Normed r
    , Ord r
    , IsScalar r
    ) => MetricSpace (L1 v r)
        where
-- instance (VG.Unbox r, RealFrac r,Floating r) => MetricSpace (L1 VG.Vector r) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(L1 v1) !(L1 v2) = go 0 (VG.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)
--             go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
--                               *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

    isFartherThan !(L1 v1) !(L1 v2) !dist = go 0 (VG.length v1-1)
        where
            go tot (-1) = False
            go tot i = if tot'>dist
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

-------------------------------------------------------------------------------
-- L2

newtype L2 v a = L2 { unL2 :: v a }
    deriving (Read,Show,POrd_,Lattice_,Ord_,Arbitrary,FromRecord,NFData, P.Ord)

type instance Logic (L2 v a) = Logic (v a)

deriving instance Eq_ (v a) => Eq_ (L2 v a)
deriving instance F.Foldable v => F.Foldable (L2 v)
-- deriving instance Functor v => Functor (L2 v)

instance VG.Vector v a => VG.Vector (L2 v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (L2M v) = liftM L2 $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (L2 v) = liftM L2M $ VG.basicUnsafeThaw v
    basicLength (L2 v) = VG.basicLength v
    basicUnsafeSlice s t (L2 v) = L2 $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (L2 v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (L2M vm) (L2 v) = VG.basicUnsafeCopy vm v
    elemseq (L2 v) a b = VG.elemseq v a b

newtype L2M v s a = L2M { unL2M :: v s a }

instance VGM.MVector v a => VGM.MVector (L2M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (L2M v) = VGM.basicLength v
    basicUnsafeSlice s t (L2M v) = L2M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (L2M v1) (L2M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM L2M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM L2M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (L2M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (L2M v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (L2M v1) (L2M v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (L2M v1) (L2M v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (L2M v) i = L2M `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (L2 v) = L2M (VG.Mutable v)

---------------------------------------

type instance Scalar (L2 v r) = r

instance
    ( VG.Vector v r
    , Eq (v r)
    , Floating r
    , Normed r
    , Ord r
    , IsScalar r
    ) => MetricSpace (L2 v r)
        where

--     {-# INLINE[1] distance #-}
--     distance v1 v2 = distance_l2_hask v1 v2
--
--     {-# INLINE[1] isFartherThanWithDistanceCanError #-}
--     isFartherThanWithDistanceCanError v1 v2 = isFartherThan_l2_hask v1 v2

    {-# INLINE[1] distance #-}
    distance (L2 v1) (L2 v2) = {-# SCC l2_distance_hask #-} sqrt $ go 0 0
        where
            go !tot !i =  if i>VG.length v1-4
                then goEach tot i
                else go tot' (i+4)
                where
                    tot' = tot
                        +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))

            goEach !tot !i = if i>= VG.length v1
                then tot
                else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

    {-# INLINE[1] isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError (L2 v1) (L2 v2) !dist = {-# SCC l2_isFartherThan_hask #-}
        sqrt $ go 0 0
        where
            dist2=dist*dist

            go !tot !i = if i>VG.length v1-4
                then goEach tot i
                else if tot'>dist2
                    then errorVal
                    else go tot' (i+4)
                where
                    tot' = tot
                        +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))

            goEach !tot !i = if i>= VG.length v1
                then tot
                else if tot'>dist2
                    then errorVal
                    else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)


--------------------------------------------------------------------------------

-- {-# RULES
--
-- "subhask/distance_l2_m128_storable"        distance = distance_l2_m128_storable
-- "subhask/distance_l2_m128_unboxed"         distance = distance_l2_m128_unboxed
-- "subhask/distance_l2_m128d_storable"       distance = distance_l2_m128d_storable
-- "subhask/isFartherThan_l2_m128_storable"   isFartherThanWithDistanceCanError=isFartherThan_l2_m128_storable
-- "subhask/isFartherThan_l2_m128_unboxed"    isFartherThanWithDistanceCanError=isFartherThan_l2_m128_unboxed
-- "subhask/isFartherThan_l2_m128d_storable"  isFartherThanWithDistanceCanError=isFartherThan_l2_m128d_storable
--
--   #-}

-- | FIXME: super dangerous :)
data UnsafeUnboxed = UnsafeUnboxed !Int !Int !ByteArray

unsafeUV2Ptr :: Storable r => UnboxedVector r -> (Ptr r, Int)
unsafeUV2Ptr v = unboxed2Ptr $ unsafeCoerce v
    where
        unboxed2Ptr (UnsafeUnboxed i n arr) = (advancePtr (unsafeCoerce $ byteArrayContents arr) i,n)

foreign import ccall unsafe "distance_l2_m128" distance_l2_m128_
    :: Ptr Float -> Ptr Float -> Int -> IO Float

distance_l2_m128_storable :: L2 Vector Float -> L2 Vector Float -> Float
distance_l2_m128_storable (L2 v1) (L2 v2) = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        distance_l2_m128_ p1 p2 n1
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

distance_l2_m128_unboxed :: L2 UnboxedVector Float -> L2 UnboxedVector Float -> Float
distance_l2_m128_unboxed (L2 v1) (L2 v2) = {-# SCC l2_distance_m128_unboxed #-}unsafeDupablePerformIO $
    distance_l2_m128_ p1 p2 n1
    where
        (p1,n1) = unsafeUV2Ptr v1
        (p2,n2) = unsafeUV2Ptr v2

foreign import ccall unsafe "isFartherThan_l2_m128" isFartherThan_l2_m128_
    :: Ptr Float -> Ptr Float -> Int -> Float -> IO Float

isFartherThan_l2_m128_storable :: L2 Vector Float -> L2 Vector Float -> Float -> Float
isFartherThan_l2_m128_storable (L2 v1) (L2 v2) dist = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        isFartherThan_l2_m128_ p1 p2 n1 dist
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

isFartherThan_l2_m128_unboxed :: L2 UnboxedVector Float -> L2 UnboxedVector Float -> Float -> Float
isFartherThan_l2_m128_unboxed (L2 v1) (L2 v2) dist = {-# SCC l2_isFartherThan_m128_unboxed  #-}unsafeDupablePerformIO $
    isFartherThan_l2_m128_ p1 p2 n1 dist
    where
        (p1,n1) = unsafeUV2Ptr v1
        (p2,n2) = unsafeUV2Ptr v2

foreign import ccall unsafe "distance_l2_float" distance_l2_float_
    :: Ptr Float -> Ptr Float -> Int -> IO Float

distance_l2_float_storable :: L2 Vector Float -> L2 Vector Float -> Float
distance_l2_float_storable (L2 v1) (L2 v2) = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        distance_l2_float_ p1 p2 n1
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

distance_l2_float_unboxed :: L2 UnboxedVector Float -> L2 UnboxedVector Float -> Float
distance_l2_float_unboxed (L2 v1) (L2 v2) = unsafeDupablePerformIO $
    distance_l2_float_ p1 p2 n1
    where
        (p1,n1) = unsafeUV2Ptr v1
        (p2,n2) = unsafeUV2Ptr v2

foreign import ccall unsafe "isFartherThan_l2_float" isFartherThan_l2_float_
    :: Ptr Float -> Ptr Float -> Int -> Float -> IO Float

isFartherThan_l2_float_storable :: L2 Vector Float -> L2 Vector Float -> Float -> Float
isFartherThan_l2_float_storable (L2 v1) (L2 v2) dist = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        isFartherThan_l2_float_ p1 p2 n1 dist
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

isFartherThan_l2_float_unboxed :: L2 UnboxedVector Float -> L2 UnboxedVector Float -> Float -> Float
isFartherThan_l2_float_unboxed (L2 v1) (L2 v2) dist = unsafeDupablePerformIO $
    isFartherThan_l2_float_ p1 p2 n1 dist
    where
        (p1,n1) = unsafeUV2Ptr v1
        (p2,n2) = unsafeUV2Ptr v2

foreign import ccall unsafe "distance_l2_m128d" distance_l2_m128d_
    :: Ptr Double -> Ptr Double -> Int -> IO Double

distance_l2_m128d_storable :: L2 Vector Double -> L2 Vector Double -> Double
distance_l2_m128d_storable (L2 v1) (L2 v2) = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        distance_l2_m128d_ p1 p2 n1
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

foreign import ccall unsafe "isFartherThan_l2_m128d" isFartherThan_l2_m128d_
    :: Ptr Double -> Ptr Double -> Int -> Double -> IO Double

isFartherThan_l2_m128d_storable :: L2 Vector Double -> L2 Vector Double -> Double -> Double
isFartherThan_l2_m128d_storable (L2 v1) (L2 v2) dist = unsafeDupablePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        isFartherThan_l2_m128d_ p1 p2 n1 dist
    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1
        (fp2,n2) = VS.unsafeToForeignPtr0 v2

-------------------------------------------------------------------------------
-- Linf

{-
newtype Linf v a = Linf { unLinf :: v a }
    deriving (Read,Show,Arbitrary,FromRecord,NFData, P.Ord)

type instance Logic (L2 v a) = Logic (v a)

deriving instance Eq_ (v a) => Eq_ (Linf v a)
deriving instance POrd_ (v a) => POrd_ (Linf v a)
deriving instance Lattice_ (v a) => Lattice_ (Linf v a)
deriving instance Ord_ (v a) => Ord_ (Linf v a)
deriving instance F.Foldable v => F.Foldable (Linf v)
deriving instance Functor v => Functor (Linf v)

instance VG.Vector v a => VG.Vector (Linf v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (LinfM v) = liftM Linf $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Linf v) = liftM LinfM $ VG.basicUnsafeThaw v
    basicLength (Linf v) = VG.basicLength v
    basicUnsafeSlice s t (Linf v) = Linf $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Linf v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (LinfM vm) (Linf v) = VG.basicUnsafeCopy vm v
    elemseq (Linf v) a b = VG.elemseq v a b

newtype LinfM v s a = LinfM { unLinfM :: v s a }

instance VGM.MVector v a => VGM.MVector (LinfM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (LinfM v) = VGM.basicLength v
    basicUnsafeSlice s t (LinfM v) = LinfM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (LinfM v1) (LinfM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM LinfM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM LinfM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (LinfM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (LinfM v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (Linf v) = LinfM (VG.Mutable v)

---------------------------------------

type instance Scalar (Linf v r) = r

instance
    ( VG.Vector v r
    , Eq (v r)
    , Floating r
    , Normed r
    , Ord r
    , IsScalar r
    ) => MetricSpace (Linf v r)
        where

-- instance (VG.Unbox r, RealFrac r,Floating r) => MetricSpace (Linf VG.Vector r) where
    {-# INLINABLE distance #-}
    {-# INLINABLE isFartherThan #-}

    distance !(Linf v1) !(Linf v2) = go 0 (VG.length v1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(max (v1 `VG.unsafeIndex` i) (v2 `VG.unsafeIndex` i))) (i-1)
--             go tot i = go (tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
--                               *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)) (i-1)

    isFartherThan !(Linf v1) !(Linf v2) !dist = go 0 (VG.length v1-1)
        where
            go tot (-1) = False
            go tot i = if tot'>dist
                then True
                else go tot' (i-1)
                where
                    tot' = tot+(max (v1 `VG.unsafeIndex` i) (v2 `VG.unsafeIndex` i))

---------------------------------------
-}
