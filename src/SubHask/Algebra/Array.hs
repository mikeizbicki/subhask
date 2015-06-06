{-# LANGUAGE CPP #-}
module SubHask.Algebra.Array
    ( BArray (..)
    , UArray
    , Unboxable
    )
    where

import Control.Monad
import Control.Monad.Primitive
import Unsafe.Coerce
import Data.Primitive as Prim
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Prelude as P
import SubHask.Algebra
import SubHask.Algebra.Parallel
import SubHask.Algebra.Vector
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.Compatibility.Base

-------------------------------------------------------------------------------
-- boxed arrays

newtype BArray e = BArray (V.Vector e)

type instance Index (BArray e) = Int
type instance Logic (BArray e) = Logic e
type instance Scalar (BArray e) = Int
type instance Elem (BArray e) = e
type instance SetElem (BArray e) e' = BArray e'

----------------------------------------
-- mutability

mkMutable [t| forall e. BArray e |]

----------------------------------------
-- misc instances

instance Arbitrary e => Arbitrary (BArray e) where
    arbitrary = fmap fromList arbitrary

instance NFData e => NFData (BArray e) where
    rnf (BArray v) = rnf v

instance Show e => Show (BArray e) where
    show (BArray v) = "BArray " ++ show (VG.toList v)

----------------------------------------
-- algebra

instance Semigroup (BArray e) where
    (BArray v1)+(BArray v2) = fromList $ VG.toList v1 ++ VG.toList v2

instance Monoid (BArray e) where
    zero = BArray VG.empty

instance Normed (BArray e) where
    size (BArray v) = VG.length v

----------------------------------------
-- comparison

instance (ValidLogic e, Eq_ e) => Eq_ (BArray e) where
    a1==a2 = toList a1==toList a2

instance (ClassicalLogic e, POrd_ e) => POrd_ (BArray e) where
    inf a1 a2 = fromList $ inf (toList a1) (toList a2)

instance (ClassicalLogic e, POrd_ e) => MinBound_ (BArray e) where
    minBound = zero

----------------------------------------
-- container

instance Constructible (BArray e) where
    fromList1 x xs = BArray $ VG.fromList (x:xs)

instance (ValidLogic e, Eq_ e) => Container (BArray e) where
    elem e arr = elem e $ toList arr

instance Foldable (BArray e) where

    {-# INLINE toList #-}
    toList (BArray v) = VG.toList v

    {-# INLINE uncons #-}
    uncons (BArray v) = if VG.null v
        then Nothing
        else Just (VG.head v, BArray $ VG.tail v)

    {-# INLINE unsnoc #-}
    unsnoc (BArray v) = if VG.null v
        then Nothing
        else Just (BArray $ VG.init v, VG.last v)

    {-# INLINE foldMap #-}
    foldMap f   (BArray v) = VG.foldl' (\a e -> a + f e) zero v

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    {-# INLINE foldr1' #-}
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldl1' #-}
    foldr   f x (BArray v) = VG.foldr   f x v
    foldr'  f x (BArray v) = {-# SCC foldr'_BArray #-} VG.foldr'  f x v
    foldr1  f   (BArray v) = VG.foldr1  f   v
    foldr1' f   (BArray v) = VG.foldr1' f   v
    foldl   f x (BArray v) = VG.foldl   f x v
    foldl'  f x (BArray v) = VG.foldl'  f x v
    foldl1  f   (BArray v) = VG.foldl1  f   v
    foldl1' f   (BArray v) = VG.foldl1' f   v

instance ValidLogic e => Sliceable (BArray e) where
    slice i n (BArray v) = BArray $ VG.slice i n v

instance ValidLogic e => IxContainer (BArray e) where
    lookup i (BArray v) = v VG.!? i
    (!) (BArray v) = VG.unsafeIndex v
    indices (BArray v) = [0..VG.length v-1]
    values (BArray v) = VG.toList v
    imap f (BArray v) = BArray $ VG.imap f v

instance ValidLogic e => Partitionable (BArray e) where
    partition n arr = go 0
        where
            go i = if i>=length arr
                then []
                else (slice i len arr):(go $ i+lenmax)
                where
                    len = if i+lenmax >= length arr
                        then (length arr)-i
                        else lenmax

            lenmax = length arr `quot` n

-------------------------------------------------------------------------------
-- unboxed arrays

newtype UArray e = UArray (VU.Vector e)

type instance Index (UArray e) = Int
type instance Logic (UArray e) = Logic e
type instance Scalar (UArray e) = Int
type instance Elem (UArray e) = e
type instance SetElem (UArray e) e' = UArray e'

----------------------------------------
-- mutability

mkMutable [t| forall e. UArray e |]

----------------------------------------
-- misc instances

instance (Unboxable e, Arbitrary e) => Arbitrary (UArray e) where
    arbitrary = fmap fromList arbitrary

instance (Unbox e, NFData e) => NFData (UArray e) where
    rnf (UArray v) = rnf v

instance (Unbox e, Show e) => Show (UArray e) where
    show (UArray v) = "UArray " ++ show (VG.toList v)

----------------------------------------
-- algebra

instance Unboxable e => Semigroup (UArray e) where
    (UArray v1)+(UArray v2) = fromList $ VG.toList v1 ++ VG.toList v2

instance Unbox e => Normed (UArray e) where
    size (UArray v) = VG.length v

----------------------------------------
-- comparison

instance (Unboxable e, Eq_ e) => Eq_ (UArray e) where
    a1==a2 = toList a1==toList a2

instance (Unboxable e, POrd_ e) => POrd_ (UArray e) where
    inf a1 a2 = fromList $ inf (toList a1) (toList a2)

instance (Unboxable e, POrd_ e) => MinBound_ (UArray e) where
    minBound = zero

----------------------------------------
-- container

type Unboxable e = (Monoid (UArray e), Constructible (UArray e), ClassicalLogic e, Eq_ e, Unbox e)

#define mkConstructible(e) \
instance Constructible (UArray e) where\
    { fromList1 x xs = UArray $ VG.fromList (x:xs) } ; \
instance Monoid (UArray e) where \
    zero = UArray $ P.mempty

mkConstructible(Int)
mkConstructible(Char)
mkConstructible(Bool)

{-
instance (Unboxable x, Unboxable y) => Constructible (UArray (Labeled' x y)) where
    fromList1 x xs = UArray $ UMV_Labeled' $ VG.fromList (x:xs)

instance (Unboxable x, Unboxable y) => Monoid (UArray (Labeled' x y)) where
    zero = UMV_Labeled' zero zero
-}

instance
    ( ClassicalLogic r
    , Eq_ r
    , Unbox r
    , Prim r
    , FreeModule r
    , IsScalar r
    ) => Constructible (UArray (UVector (s::Symbol) r))
        where

    {-# INLINABLE fromList1 #-}
    fromList1 x xs = fromList1N (length $ x:xs) x xs

    {-# INLINABLE fromList1N #-}
    fromList1N n x xs = unsafeInlineIO $ do
        marr <- newPinnedByteArray (n*size*rbytes)
        let mv = UArray_MUVector marr 0 n size

        let go [] (-1) = return ()
            go (x:xs) i = do
                VGM.unsafeWrite mv i x
                go xs (i-1)

        go (P.reverse $ x:xs) (n-1)
        v <- VG.basicUnsafeFreeze mv
        return $ UArray v
        where
            rbytes=Prim.sizeOf (undefined::r)
            size=dim x

instance
    ( ClassicalLogic r
    , Eq_ r
    , Unbox r
    , Prim r
    , FreeModule r
    , IsScalar r
    ) => Monoid (UArray (UVector (s::Symbol) r)) where
    zero = unsafeInlineIO $ do
        marr <- newPinnedByteArray 0
        arr <- unsafeFreezeByteArray marr
        return $ UArray $ UArray_UVector arr 0 0 0

instance
    ( ClassicalLogic r
    , Eq_ r
    , Unbox r
    , Prim r
    , FreeModule r
    , IsScalar r
    , Prim y
    , Unbox y
    ) => Constructible (UArray (Labeled' (UVector (s::Symbol) r) y))
        where

    {-# INLINABLE fromList1 #-}
    fromList1 x xs = fromList1N (length $ x:xs) x xs

    {-# INLINABLE fromList1N #-}
    fromList1N n x xs = unsafeInlineIO $ do
        marr <- newPinnedByteArray (n*(xsize+ysize)*rbytes)
        let mv = UArray_Labeled'_MUVector marr 0 n xsize

        let go [] (-1) = return ()
            go (x:xs) i = do
                VGM.unsafeWrite mv i x
                go xs (i-1)

        go (P.reverse $ x:xs) (n-1)
        v <- VG.basicUnsafeFreeze mv
        return $ UArray v
        where
            rbytes=Prim.sizeOf (undefined::r)

            xsize=dim $ xLabeled' x
            ysize=Prim.sizeOf (undefined::y) `quot` rbytes

instance
    ( ClassicalLogic r
    , Eq_ r
    , Unbox r
    , Prim r
    , FreeModule r
    , IsScalar r
    , Prim y
    , Unbox y
    ) => Monoid (UArray (Labeled' (UVector (s::Symbol) r) y)) where
    zero = unsafeInlineIO $ do
        marr <- newPinnedByteArray 0
        arr <- unsafeFreezeByteArray marr
        return $ UArray $ UArray_Labeled'_UVector arr 0 0 0

instance Unboxable e => Container (UArray e) where
    elem e (UArray v) = elem e $ VG.toList v

instance Unboxable e => Foldable (UArray e) where

    {-# INLINE toList #-}
    toList (UArray v) = VG.toList v

    {-# INLINE uncons #-}
    uncons (UArray v) = if VG.null v
        then Nothing
        else Just (VG.head v, UArray $ VG.tail v)

    {-# INLINE unsnoc #-}
    unsnoc (UArray v) = if VG.null v
        then Nothing
        else Just (UArray $ VG.init v, VG.last v)

    {-# INLINE foldMap #-}
    foldMap f   (UArray v) = VG.foldl' (\a e -> a + f e) zero v

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    {-# INLINE foldr1' #-}
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldl1' #-}
    foldr   f x (UArray v) = VG.foldr   f x v
    foldr'  f x (UArray v) = {-# SCC foldr'_UArray #-} VG.foldr'  f x v
    foldr1  f   (UArray v) = VG.foldr1  f   v
    foldr1' f   (UArray v) = VG.foldr1' f   v
    foldl   f x (UArray v) = VG.foldl   f x v
    foldl'  f x (UArray v) = VG.foldl'  f x v
    foldl1  f   (UArray v) = VG.foldl1  f   v
    foldl1' f   (UArray v) = VG.foldl1' f   v

instance Unboxable e => Sliceable (UArray e) where
    slice i n (UArray v) = UArray $ VG.slice i n v

instance Unboxable e => IxContainer (UArray e) where
    lookup i (UArray v) = v VG.!? i
    (!) (UArray v) = VG.unsafeIndex v
    indices (UArray v) = [0..VG.length v-1]
    values (UArray v) = VG.toList v
--     imap = VG.imap

instance Unboxable e => Partitionable (UArray e) where
    partition n arr = go 0
        where
            go i = if i>=length arr
                then []
                else (slice i len arr):(go $ i+lenmax)
                where
                    len = if i+lenmax >= length arr
                        then (length arr)-i
                        else lenmax

            lenmax = length arr `quot` n


-------------------------------------------------------------------------------
-- unsafe globals

{-
{-# NOINLINE ptsizeIO #-}
ptsizeIO = unsafeDupablePerformIO $ newIORef (5::Int)

{-# NOINLINE ptalignIO #-}
ptalignIO = unsafeDupablePerformIO $ newIORef (5::Int)

{-# NOINLINE ptsize #-}
ptsize = unsafeDupablePerformIO $ readIORef ptsizeIO

{-# NOINLINE ptalign #-}
ptalign = unsafeDupablePerformIO $ readIORef ptalignIO

-- {-# NOINLINE setptsize #-}
setptsize :: Int -> IO ()
setptsize len = do
    writeIORef ptsizeIO len
    writeIORef ptalignIO (1::Int)
-}

-------------------------------------------------------------------------------
-- UVector

instance
    ( IsScalar elem
    , ClassicalLogic elem
    , Unbox elem
    , Prim elem
    ) => Unbox (UVector (n::Symbol) elem)

---------------------------------------

data instance VU.Vector (UVector (n::Symbol) elem) = UArray_UVector
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- ^ offset
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( IsScalar elem
    , Unbox elem
    , Prim elem
    ) => VG.Vector VU.Vector (UVector (n::Symbol) elem)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_UVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i len' (UArray_UVector arr off n size) = UArray_UVector arr (off+i*size) len' size

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (UArray_MUVector marr off n size) = do
        arr <- unsafeFreezeByteArray marr
        return $ UArray_UVector arr off n size

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (UArray_UVector arr off n size)= do
        marr <- unsafeThawByteArray arr
        return $ UArray_MUVector marr off n size

    {-# INLINABLE basicUnsafeIndexM #-}
    basicUnsafeIndexM (UArray_UVector arr off n size) i =
        return $ UVector_Dynamic arr (off+i*size) size

--     {-# INLINABLE basicUnsafeCopy #-}
--     basicUnsafeCopy mv v = VG.basicUnsafeCopy (vecM mv) (vec v)

---------------------------------------

data instance VUM.MVector s (UVector (n::Symbol) elem) = UArray_MUVector
    {-#UNPACK#-}!(MutableByteArray s)
    {-#UNPACK#-}!Int -- ^ offset in number of elem
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( ClassicalLogic elem
    , IsScalar elem
    , Unbox elem
    , Prim elem
    ) => VGM.MVector VUM.MVector (UVector (n::Symbol) elem)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_MUVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' (UArray_MUVector marr off n size) = UArray_MUVector marr (off+i*size) lenM' size

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (UArray_MUVector marr1 off1 n1 size) (UArray_MUVector marr2 off2 n2 _)
        = sameMutableByteArray marr1 marr2

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew lenM' = error "basicUnsafeNew not supported on UArray_MUVector"
--     basicUnsafeNew lenM' = do
--         let elemsize=ptsize
--         marr <- newPinnedByteArray (lenM'*elemsize*Prim.sizeOf (undefined::elem))
--         return $ UArray_MUVector marr 0 lenM' elemsize

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead mv@(UArray_MUVector marr off n size) i = do
        let b=Prim.sizeOf (undefined::elem)
        marr' <- newPinnedByteArray (size*b)
        copyMutableByteArray marr' 0 marr ((off+i*size)*b) (size*b)
        arr <- unsafeFreezeByteArray marr'
        return $ UVector_Dynamic arr 0 size

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite mv@(UArray_MUVector marr1 off1 _ size) loc v@(UVector_Dynamic arr2 off2 _) =
        copyByteArray marr1 ((off1+size*loc)*b) arr2 (off2*b) (size*b)
        where
            b=Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeCopy #-}
    basicUnsafeCopy (UArray_MUVector marr1 off1 n1 size1) (UArray_MUVector marr2 off2 n2 size2) =
        copyMutableByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = size1*Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeMove #-}
    basicUnsafeMove (UArray_MUVector marr1 off1 n1 size1) (UArray_MUVector marr2 off2 n2 size2) =
        moveByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = size1*Prim.sizeOf (undefined::elem)

----------------------------------------
-- Labeled'

instance
    ( Unbox y
    , Prim y
    , ClassicalLogic a
    , IsScalar a
    , Unbox a
    , Prim a
    ) => Unbox (Labeled' (UVector (s::Symbol) a) y)

---------------------------------------

-- newtype instance VUM.MVector s (Labeled' x y) = UMV_Labeled' (VUM.MVector s (x,y))

data instance VUM.MVector s (Labeled' (UVector (n::Symbol) elem) y) = UArray_Labeled'_MUVector
    {-#UNPACK#-}!(MutableByteArray s)
    {-#UNPACK#-}!Int -- ^ offset in number of elem
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( ClassicalLogic elem
    , IsScalar elem
    , Unbox elem
    , Prim elem
    , Prim y
    ) => VGM.MVector VUM.MVector (Labeled' (UVector (n::Symbol) elem) y)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_Labeled'_MUVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i lenM' (UArray_Labeled'_MUVector marr off n size)
        = UArray_Labeled'_MUVector marr (off+i*(size+ysize)) lenM' size
        where
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (UArray_Labeled'_MUVector marr1 off1 n1 size) (UArray_Labeled'_MUVector marr2 off2 n2 _)
        = sameMutableByteArray marr1 marr2

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew = error "basicUnsafeNew not supported on UArray_Labeled'_MUVector"
--     basicUnsafeNew lenM' = do
--         let elemsize=ptsize
--         marr <- newPinnedByteArray (lenM'*(elemsize+ysize)*Prim.sizeOf (undefined::elem))
--         return $ UArray_Labeled'_MUVector marr 0 lenM' elemsize
--         where
--             ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead mv@(UArray_Labeled'_MUVector marr off n size) i = do
        marr' <- newPinnedByteArray (size*b)
        copyMutableByteArray marr' 0 marr ((off+i*(size+ysize))*b) (size*b)
        arr <- unsafeFreezeByteArray marr'
        let x=UVector_Dynamic arr 0 size
        y <- readByteArray marr $ (off+i*(size+ysize)+size) `quot` ysize
        return $ Labeled' x y
        where
            b=Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite
        (UArray_Labeled'_MUVector marr1 off1 _ size)
        i
        (Labeled' (UVector_Dynamic arr2 off2 _) y)
        = do
            copyByteArray marr1 ((off1+i*(size+ysize))*b) arr2 (off2*b) (size*b)
            writeByteArray marr1 ((off1+i*(size+ysize)+size) `quot` ysize) y
        where
            b=Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeCopy #-}
    basicUnsafeCopy
        (UArray_Labeled'_MUVector marr1 off1 n1 size1)
        (UArray_Labeled'_MUVector marr2 off2 n2 size2)
        = copyMutableByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = (size1+ysize)*Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeMove #-}
    basicUnsafeMove
        (UArray_Labeled'_MUVector marr1 off1 n1 size1)
        (UArray_Labeled'_MUVector marr2 off2 n2 size2)
        = moveByteArray marr1 (off1*b) marr2 (off2*b) (n2*b)
        where
            b = (size1+ysize)*Prim.sizeOf (undefined::elem)
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

----------------------------------------

data instance VU.Vector (Labeled' (UVector (n::Symbol) elem) y) = UArray_Labeled'_UVector
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- ^ offset
    {-#UNPACK#-}!Int -- ^ length of container
    {-#UNPACK#-}!Int -- ^ length of element vectors

instance
    ( IsScalar elem
    , Unbox elem
    , Prim elem
    , Prim y
    ) => VG.Vector VU.Vector (Labeled' (UVector (n::Symbol) elem) y)
        where

    {-# INLINABLE basicLength #-}
    basicLength (UArray_Labeled'_UVector _ _ n _) = n

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice i len' (UArray_Labeled'_UVector arr off n size)
        = UArray_Labeled'_UVector arr (off+i*(size+ysize)) len' size
        where
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (UArray_Labeled'_MUVector marr off n size) = do
        arr <- unsafeFreezeByteArray marr
        return $ UArray_Labeled'_UVector arr off n size

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (UArray_Labeled'_UVector arr off n size)= do
        marr <- unsafeThawByteArray arr
        return $ UArray_Labeled'_MUVector marr off n size

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (UArray_Labeled'_UVector arr off n size) i =
        return $ Labeled' x y
        where
            off' = off+i*(size+ysize)
            x = UVector_Dynamic arr off' size
            y = indexByteArray arr $ (off'+size) `quot` ysize
            ysize=Prim.sizeOf (undefined::y) `quot` Prim.sizeOf (undefined::elem)
--             y = indexByteArray arr $ (off'+size) `shiftR` 1
--             ysize=2

-------------------------------------------------------------------------------
-- Labeled'

{-
instance (VUM.Unbox x, VUM.Unbox y) => VUM.Unbox (Labeled' x y)

newtype instance VUM.MVector s (Labeled' x y) = UMV_Labeled' (VUM.MVector s (x,y))

instance
    ( VUM.Unbox x
    , VUM.Unbox y
    ) => VGM.MVector VUM.MVector (Labeled' x y)
        where

    {-# INLINABLE basicLength #-}
    {-# INLINABLE basicUnsafeSlice #-}
    {-# INLINABLE basicOverlaps #-}
    {-# INLINABLE basicUnsafeNew #-}
    {-# INLINABLE basicUnsafeRead #-}
    {-# INLINABLE basicUnsafeWrite #-}
    {-# INLINABLE basicUnsafeCopy #-}
    {-# INLINABLE basicUnsafeMove #-}
    {-# INLINABLE basicSet #-}
    basicLength (UMV_Labeled' v) = VGM.basicLength v
    basicUnsafeSlice i len (UMV_Labeled' v) = UMV_Labeled' $ VGM.basicUnsafeSlice i len v
    basicOverlaps (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew len = liftM UMV_Labeled' $ VGM.basicUnsafeNew len
    basicUnsafeRead (UMV_Labeled' v) i = do
        (!x,!y) <- VGM.basicUnsafeRead v i
        return $ Labeled' x y
    basicUnsafeWrite (UMV_Labeled' v) i (Labeled' x y) = VGM.basicUnsafeWrite v i (x,y)
    basicUnsafeCopy (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (UMV_Labeled' v1) (UMV_Labeled' v2) = VGM.basicUnsafeMove v1 v2
    basicSet (UMV_Labeled' v1) (Labeled' x y) = VGM.basicSet v1 (x,y)

newtype instance VU.Vector (Labeled' x y) = UV_Labeled' (VU.Vector (x,y))

instance
    ( VUM.Unbox x
    , VUM.Unbox y
    ) => VG.Vector VU.Vector (Labeled' x y)
        where

    {-# INLINABLE basicUnsafeFreeze #-}
    {-# INLINABLE basicUnsafeThaw #-}
    {-# INLINABLE basicLength #-}
    {-# INLINABLE basicUnsafeSlice #-}
--     {-# INLINABLE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (UMV_Labeled' v) = liftM UV_Labeled' $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (UV_Labeled' v) = liftM UMV_Labeled' $ VG.basicUnsafeThaw v
    basicLength (UV_Labeled' v) = VG.basicLength v
    basicUnsafeSlice i len (UV_Labeled' v) = UV_Labeled' $ VG.basicUnsafeSlice i len v
    basicUnsafeIndexM (UV_Labeled' v) i = do
        (!x,!y) <- VG.basicUnsafeIndexM v i
        return $ Labeled' x y
        -}
