{-# LANGUAGE ForeignFunctionInterface #-}

-- | Importing this module will activate RULES that use the FFI for vector ops.
module SubHask.Algebra.Vector.FFI
    ( distance_l2_m128
    , distance_l2_m128_SVector_Dynamic
    , distance_l2_m128_UVector_Dynamic

    , distanceUB_l2_m128
    , distanceUB_l2_m128_SVector_Dynamic
    , distanceUB_l2_m128_UVector_Dynamic
    )
    where

import qualified Prelude as P
import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils

import System.IO.Unsafe
import Unsafe.Coerce

import SubHask.Algebra
import SubHask.Algebra.Vector
import SubHask.Category
import SubHask.Internal.Prelude

{-# RULES

"subhask/distance_l2_m128_UVector_Dynamic"     distance   = distance_l2_m128_UVector_Dynamic
"subhask/distance_l2_m128_SVector_Dynamic"     distance   = distance_l2_m128_SVector_Dynamic

"subhask/distanceUB_l2_m128_UVector_Dynamic"   distanceUB = distanceUB_l2_m128_UVector_Dynamic
"subhask/distanceUB_l2_m128_SVector_Dynamic"   distanceUB = distanceUB_l2_m128_SVector_Dynamic

  #-}

{-# INLINE sizeOfFloat #-}
sizeOfFloat :: Int
sizeOfFloat = sizeOf (undefined::Float)

foreign import ccall unsafe "distance_l2_m128" distance_l2_m128
    :: Ptr Float -> Ptr Float -> Int -> IO Float

foreign import ccall unsafe "distanceUB_l2_m128" distanceUB_l2_m128
    :: Ptr Float -> Ptr Float -> Int -> Float -> IO Float

{-# INLINE distance_l2_m128_UVector_Dynamic #-}
distance_l2_m128_UVector_Dynamic :: UVector (s::Symbol) Float -> UVector (s::Symbol) Float -> Float
distance_l2_m128_UVector_Dynamic (UVector_Dynamic arr1 off1 n) (UVector_Dynamic arr2 off2 _)
    = unsafeInlineIO $ distance_l2_m128 p1 p2 n
    where
        p1 = plusPtr (unsafeCoerce $ byteArrayContents arr1) (off1*sizeOfFloat)
        p2 = plusPtr (unsafeCoerce $ byteArrayContents arr2) (off2*sizeOfFloat)

{-# INLINE distanceUB_l2_m128_UVector_Dynamic #-}
distanceUB_l2_m128_UVector_Dynamic :: UVector (s::Symbol) Float -> UVector (s::Symbol) Float -> Float -> Float
distanceUB_l2_m128_UVector_Dynamic (UVector_Dynamic arr1 off1 n) (UVector_Dynamic arr2 off2 _) ub
    = unsafeInlineIO $ distanceUB_l2_m128 p1 p2 n ub
    where
        p1 = plusPtr (unsafeCoerce $ byteArrayContents arr1) (off1*sizeOfFloat)
        p2 = plusPtr (unsafeCoerce $ byteArrayContents arr2) (off2*sizeOfFloat)

{-# INLINE distance_l2_m128_SVector_Dynamic #-}
distance_l2_m128_SVector_Dynamic :: SVector (s::Symbol) Float -> SVector (s::Symbol) Float -> Float
distance_l2_m128_SVector_Dynamic (SVector_Dynamic fp1 off1 n) (SVector_Dynamic fp2 off2 _)
    = unsafeInlineIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            distance_l2_m128 (plusPtr p1 $ off1*sizeOfFloat) (plusPtr p2 $ off2*sizeOfFloat) n

{-# INLINE distanceUB_l2_m128_SVector_Dynamic #-}
distanceUB_l2_m128_SVector_Dynamic :: SVector (s::Symbol) Float -> SVector (s::Symbol) Float -> Float -> Float
distanceUB_l2_m128_SVector_Dynamic (SVector_Dynamic fp1 off1 n) (SVector_Dynamic fp2 off2 _) ub
    = unsafeInlineIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            distanceUB_l2_m128 (plusPtr p1 $ off1*sizeOfFloat) (plusPtr p2 $ off2*sizeOfFloat) n ub
