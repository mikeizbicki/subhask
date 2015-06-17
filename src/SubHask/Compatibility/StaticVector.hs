-- |
--
-- FIXME:
--
-- * Connect with HMatrix
--
-- * Add Hilbert instances
--
-- * Add Reisz instances
--
module SubHask.Compatibility.StaticVector
--     ( Vector
--     , StaticVector
--     , DynVector
--     , UnsafeDynVector
--     , SArray
--     )
    where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Control.Monad.Primitive
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import qualified Prelude as P
import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Category
import SubHask.Compatibility.Base
import SubHask.Compatibility.Vector ()

import qualified Prelude as P
import Control.Monad
import Debug.Trace

import Data.Csv

--------------------------------------------------------------------------------

-- |
--
-- FIXME:
-- We can't create "Vector"s of unknown size using type families because the @n@ parameter might unify later.
-- Maybe we can fix this with a GHC plugin?
type family Vector (n::Nat) r where
    Vector 200 r = StaticVector 200 r
    Vector n r   = DynVector n r

-- | does the foreign pointer equal null?
isNull :: ForeignPtr a -> Bool
isNull fp = unsafeInlineIO $ withForeignPtr fp $ \p -> (return $ p P.== nullPtr)

-- | allocates a ForeignPtr that is filled with n "zero"s
zerofp :: forall n r. (Storable r, Monoid r) => Int -> IO (ForeignPtr r)
zerofp n = do
    fp <- mallocForeignPtrBytes b
    withForeignPtr fp $ \p -> go p (n-1)
    return fp
    where
        b = n*sizeOf (undefined::r)

        go _ (-1) = return ()
        go p i = do
            pokeElemOff p i zero
            go p (i-1)

--------------------------------------------------------------------------------

instance Storable a => VUM.Unbox (DynVector n a)

data instance (VU.Vector (DynVector n a)) = UVector
    {-#UNPACK#-}!(ForeignPtr a)
    {-#UNPACK#-}!Int -- offset in (sizeOf a)
    {-#UNPACK#-}!Int -- length in number of DynVectors
    {-#UNPACK#-}!Int -- size of a DynVector in (sizeOf a)

-- instance (Storable a, Show a, Monoid a) => Show (VU.Vector (DynVector n a)) where
--     show v = show $ VG.toList v

instance NFData (VU.Vector (DynVector n a)) where
    rnf a = seq a ()

instance Storable a => VG.Vector VU.Vector (DynVector n a) where
    {-# INLINABLE basicLength #-}
    basicLength (UVector _ _ len _) = len

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice off' len' (UVector fp off len s) = UVector fp (off+off'*s) len' s

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (UVectorM fp off len s) = return $ UVector fp off len s

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (UVector fp off len s) = return $ UVectorM fp off len s

    {-# INLINABLE basicUnsafeIndexM #-}
    basicUnsafeIndexM (UVector fp off len s) i = return $ DynVector (castForeignPtr fp) (off+i*s) s
        where
            bytes=sizeOf (undefined::a)

{-
instance Storable a => Semigroup (VU.Vector (DynVector n a)) where
    {-# INLINABLE (+) #-}
    (UVector fp1 off1 len1 s)+(UVector fp2 off2 len2 _) = trace "+" $ unsafeInlineIO $ do
        fp3 <- mallocForeignPtrBytes $ bytes*s*(len1+len2)

        withForeignPtr fp1 $ \p1 -> withForeignPtr fp3 $ \p3 ->
            copyBytes p3 (plusPtr p1 off1) (len1*s*bytes)

        withForeignPtr fp2 $ \p2 -> withForeignPtr fp3 $ \p3 ->
--             copyBytes p3 (plusPtr p2 off2) (len2*s*bytes)
            trace ("len1*s*bytes="++show (len1*s*bytes)) $
            moveBytes (plusPtr p3 $ len1*s*bytes) (plusPtr p2 off2) (len2*s*bytes)
--             moveBytes (plusPtr p3 $ 12) (plusPtr p2 off2) (len2*s*bytes)

        let ret=UVector (castForeignPtr fp3::ForeignPtr (DynVector 3 Float)) 0 (len1+len2) s
--         trace ("ret VG.! 0 = "++show (ret VG.! 0)) $ return ()
--         trace ("ret VG.! 1 = "++show (ret VG.! 1)) $ return ()

        return $ UVector fp3 0 (len1+len2) s

        where
            bytes=sizeOf (undefined::a)

instance Storable a => Monoid (VU.Vector (DynVector n a)) where
--     zero = error "Monoid (UVector (DynVector n a)) : zero not implemented"
-}
{-
instance Storable a => Normed (UVector (DynVector n a)) where
    size = VG.basicLength

instance Storable a => Constructible (UVector (DynVector n a)) where
    fromList1N len x@(DynVector _ _ n) xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrBytes $ n*len*sizeOf (undefined::a)
        go (UVectorM fp 0 len n) 0 (x:xs)
        return $ UVector fp 0 len n
        where
            go _    _ []     = return ()
            go marr i (y:ys) = do
                VGM.unsafeWrite marr i y
                go marr (i+1) ys

instance (ValidLogic a, Storable a) => Container (UVector (DynVector n a))

instance Storable a => Foldable (UVector (DynVector n a)) where
    {-# INLINE toList #-}
    toList = VG.toList

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    {-# INLINE foldr1' #-}
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldl1' #-}
    foldr   = VG.foldr
    foldr'  = {-# SCC foldr'_UVector #-} VG.foldr'
    foldr1  = VG.foldr1
    foldr1' = VG.foldr1'
    foldl   = VG.foldl
    foldl'  = VG.foldl'
    foldl1  = VG.foldl1
    foldl1' = VG.foldl1'
-}

-------------------

-- newtype instance Mutable m (VU.Vector (DynVector n a))
--     = Mutable_UVector (PrimRef m (VU.Vector (DynVector n a)))
--
-- instance IsMutable (VU.Vector (DynVector n a))

-------------------

data instance VUM.MVector s (DynVector n a) = UVectorM
    {-#UNPACK#-}!(ForeignPtr a)
    {-#UNPACK#-}!Int -- offset in bytes
    {-#UNPACK#-}!Int -- length in number of elements
    {-#UNPACK#-}!Int -- size of elem in bytes

{-# NOINLINE ptsizePrim #-}
ptsizePrim = unsafeInlineIO $ newPrimRef (16::Int)

{-# NOINLINE ptsize #-}
ptsize = unsafeInlineIO $ readPrimRef ptsizePrim

{-# NOINLINE setptsize #-}
setptsize :: Int -> IO ()
setptsize len = do
    writePrimRef ptsizePrim len

instance Storable a => VGM.MVector VUM.MVector (DynVector n a) where
    {-# INLINABLE basicLength #-}
    basicLength (UVectorM _ _ len _) = len

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice off' len' (UVectorM fp off len s) = UVectorM fp (off+off'*s) len' s

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (UVectorM fp1 _ _ _) (UVectorM fp2 _ _ _) = fp1 P.== fp2
    -- FIXME: this is an overapproximation

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew len = unsafePrimToPrim $ do
--         putStrLn $ "WARNING: MVector (DynVector n a) using ptsize="++show ptsize)
        fp <- mallocForeignPtrArray (len*ptsize)
        return $ UVectorM fp 0 len ptsize
--         error "UVectorM (DynVector n a) ; basicUnsafeNew not supported"

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead (UVectorM fp off len s) i = return $ DynVector (castForeignPtr fp) (off+i*s*bytes) s
        where
            bytes=sizeOf (undefined::a)

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite a@(UVectorM fp1 off1 len s) i v@(DynVector fp2 off2 _) = unsafePrimToPrim $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            copyBytes (plusPtr p1 $ off1+i*s*bytes) (plusPtr p2 off2) (s*bytes)
        where
            bytes=sizeOf (undefined::a)

--------------------------------------------------------------------------------

data SArray a = SArray
    {-#UNPACK#-}!(ForeignPtr a)
    {-#UNPACK#-}!Int -- offset in (sizeOf :: undefined a)
    {-#UNPACK#-}!Int -- length in number of elements
    {-#UNPACK#-}!Int -- size of elem in bytes

type instance Scalar (SArray a) = Int
type instance Logic (SArray a) = Logic a
type instance Elem (SArray a) = a

instance (Storable a, Show a, Monoid a) => Show (SArray (DynVector n a)) where
    show v = show $ toList v

instance NFData (SArray a) where
    rnf a = seq a ()

-- instance (Storable a, Arbitrary a) => Arbitrary (SArray a)

instance Storable a => VG.Vector SArray (DynVector n a) where
    {-# INLINABLE basicLength #-}
    basicLength (SArray _ _ len _) = len

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice off' len' (SArray fp off len s) = SArray fp (off+off') len' s

    {-# INLINABLE basicUnsafeFreeze #-}
    basicUnsafeFreeze (SArrayM fp off len s) = return $ SArray fp off len s

    {-# INLINABLE basicUnsafeThaw #-}
    basicUnsafeThaw (SArray fp off len s) = return $ SArrayM fp off len s

    {-# INLINABLE basicUnsafeIndexM #-}
    basicUnsafeIndexM (SArray fp off len s) i = return $ DynVector (castForeignPtr fp) (off+i*s) s
        where
            bytes=sizeOf (undefined::a)

instance Storable a => Semigroup (SArray (DynVector n a)) where
    {-# INLINABLE (+) #-}
    (SArray fp1 off1 len1 s)+(SArray fp2 off2 len2 _) = trace "+" $ unsafeInlineIO $ do
        fp3 <- mallocForeignPtrBytes $ bytes*s*(len1+len2)

        withForeignPtr fp1 $ \p1 -> withForeignPtr fp3 $ \p3 ->
            copyBytes p3 (plusPtr p1 off1) (len1*s*bytes)

        withForeignPtr fp2 $ \p2 -> withForeignPtr fp3 $ \p3 ->
--             copyBytes p3 (plusPtr p2 off2) (len2*s*bytes)
            trace ("len1*s*bytes="++show (len1*s*bytes)) $
            moveBytes (plusPtr p3 $ len1*s*bytes) (plusPtr p2 off2) (len2*s*bytes)
--             moveBytes (plusPtr p3 $ 12) (plusPtr p2 off2) (len2*s*bytes)

        let ret=SArray (castForeignPtr fp3::ForeignPtr (DynVector 3 Float)) 0 (len1+len2) s
        trace ("ret VG.! 0 = "++show (ret VG.! 0)) $ return ()
        trace ("ret VG.! 1 = "++show (ret VG.! 1)) $ return ()

        return $ SArray fp3 0 (len1+len2) s

        where
            bytes=sizeOf (undefined::a)

instance Storable a => Monoid (SArray (DynVector n a)) where
    zero = error "Monoid (SArray (DynVector n a)) : zero not implemented"

instance Storable a => Normed (SArray (DynVector n a)) where
    size = VG.basicLength

instance Storable a => Constructible (SArray (DynVector n a)) where
    fromList1N len x@(DynVector _ _ n) xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrBytes $ n*len*sizeOf (undefined::a)
        go (SArrayM fp 0 len n) 0 (x:xs)
        return $ SArray fp 0 len n
        where
            go _    _ []     = return ()
            go marr i (y:ys) = do
                VGM.unsafeWrite marr i y
                go marr (i+1) ys

instance (ValidLogic a, Storable a) => Container (SArray (DynVector n a))

instance Storable a => Foldable (SArray (DynVector n a)) where
    {-# INLINE toList #-}
    toList = VG.toList

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    {-# INLINE foldr1' #-}
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldl1' #-}
    foldr   = VG.foldr
    foldr'  = {-# SCC foldr'_SArray #-} VG.foldr'
    foldr1  = VG.foldr1
    foldr1' = VG.foldr1'
    foldl   = VG.foldl
    foldl'  = VG.foldl'
    foldl1  = VG.foldl1
    foldl1' = VG.foldl1'


-------------------

newtype instance Mutable m (SArray a) = Mutable_SArray (PrimRef m (SArray a))

instance IsMutable (SArray a)

-------------------

data SArrayM s a = SArrayM
    {-#UNPACK#-}!(ForeignPtr a)
    {-#UNPACK#-}!Int -- offset in bytes
    {-#UNPACK#-}!Int -- length in number of elements
    {-#UNPACK#-}!Int -- size of elem in bytes

type instance VG.Mutable SArray = SArrayM

instance (Storable a) => VGM.MVector SArrayM (DynVector n a) where
    {-# INLINABLE basicLength #-}
    basicLength (SArrayM _ _ len _) = len

    {-# INLINABLE basicUnsafeSlice #-}
    basicUnsafeSlice off' len' (SArrayM fp off len s) = SArrayM fp (off+off') len' s

    {-# INLINABLE basicOverlaps #-}
    basicOverlaps (SArrayM fp1 _ _ _) (SArrayM fp2 _ _ _) = fp1 P.== fp2
    -- FIXME: this is an overapproximation

    {-# INLINABLE basicUnsafeNew #-}
    basicUnsafeNew len = error "SArrayM (DynVector n a) ; basicUnsafeNew not supported"

    {-# INLINABLE basicUnsafeRead #-}
    basicUnsafeRead (SArrayM fp off len s) i = return $ DynVector (castForeignPtr fp) (off+i*s*bytes) s
        where
            bytes=sizeOf (undefined::a)

    {-# INLINABLE basicUnsafeWrite #-}
    basicUnsafeWrite a@(SArrayM fp1 off1 len s) i v@(DynVector fp2 off2 _) = unsafePrimToPrim $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
--             trace ("s*bytes="++show (s*bytes)) $
--             trace ("a="++show (SArray (castForeignPtr fp1 :: ForeignPtr (DynVector 3 Float)) off1 3 s)) $
--             trace ("v="++show (DynVector (castForeignPtr fp2 :: ForeignPtr Float) off2 s)) $
            copyBytes (plusPtr p1 $ off1+i*s*bytes) (plusPtr p2 off2) (s*bytes)
        where
            bytes=sizeOf (undefined::a)


--------------------------------------------------------------------------------

data DynVector n r = DynVector
    {-#UNPACK#-}!(ForeignPtr r)
    {-#UNPACK#-}!Int -- ^ offset
    {-#UNPACK#-}!Int -- ^ length

type instance Scalar (DynVector n r) = Scalar r
type instance Logic (DynVector n r) = Logic r
type instance Index (DynVector n r) = Int
type instance Elem (DynVector n r) = Scalar r

type instance DynVector n r >< a = DynVector n (r><a)

instance (Show r, Monoid r, Storable r) => Show (DynVector n  r) where
    show (DynVector fp off n) = if isNull fp
        then "zero"
        else show $ unsafeInlineIO $ go (n-1) []
        where
            go (-1) xs = return $ xs
            go i    xs = withForeignPtr fp $ \p -> do
                x <- peekElemOff p (off+i)
                go (i-1) (x:xs)

instance (NFData r, Storable r) => NFData (DynVector n r) where
    rnf (DynVector fp off n) = seq fp ()

instance (FromField r, Storable r, IsScalar r, FreeModule r) => FromRecord (DynVector n r) where
    parseRecord r = do
        rs :: [r] <- parseRecord r
        return $ unsafeToModule rs

--------------------

newtype instance Mutable m (DynVector n r) = Mutable_DynVector (PrimRef m (DynVector n r))

instance (Storable r) => IsMutable (DynVector n r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_DynVector ref) = readPrimRef ref
    unsafeThaw v = do
        ref <- newPrimRef v
        return $ Mutable_DynVector ref

    copy (Mutable_DynVector ref) = do
        (DynVector fp1 off1 n) <- readPrimRef ref
        let b = n*sizeOf (undefined::r)
        fp2 <- if isNull fp1
            then return fp1
            else unsafePrimToPrim $ do
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 (plusPtr p1 off1) b
                return fp2
        ref2 <- newPrimRef (DynVector fp2 0 n)
        return $ Mutable_DynVector ref2

    write (Mutable_DynVector ref) (DynVector fp2 off2 n2) = do
        (DynVector fp1 off1 n1) <- readPrimRef ref
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | isNull fp1 && isNull fp2 -> return ()

            -- only fp1 null: allocate memory then copy fp2 over
            | isNull fp1 && not isNull fp2 -> do
                fp1' <- mallocForeignPtrBytes b
                unsafePrimToPrim $ writePrimRef ref (DynVector fp1' 0 n2)
                withForeignPtr fp1' $ \p1 -> withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b

            -- only fp2 null: make fp1 null
            | not isNull fp1 && isNull fp2 -> unsafePrimToPrim $ writePrimRef ref (DynVector fp2 0 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise ->
                withForeignPtr fp1 $ \p1 ->
                withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b
            where b = n2*sizeOf (undefined::r)

----------------------------------------
-- algebra

{-# INLINE binopDyn #-}
binopDyn :: forall a b n m.
    ( Storable a
    , Monoid a
    ) => (a -> a -> a) -> DynVector n a -> DynVector n a -> DynVector n a
binopDyn f v1@(DynVector fp1 off1 n1) v2@(DynVector fp2 off2 n2) = if
    | isNull fp1 && isNull fp2 -> v1
    | isNull fp1 -> monopDyn (f zero) v2
    | isNull fp2 -> monopDyn (\a -> f a zero) v1
    | otherwise -> unsafeInlineIO $ do
        let b = n1*sizeOf (undefined::a)
        fp3 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
            withForeignPtr fp3 $ \p3 ->
            go (plusPtr p1 off1) (plusPtr p2 off2) p3 (n1-1)
        return $ DynVector fp3 0 n1

    where
        go _ _ _ (-1) = return ()
        go p1 p2 p3 i = do
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p3 i (f v1 v2)
            go p1 p2 p3 (i-1)

{-# INLINE monopDyn #-}
monopDyn :: forall a b n m.
    ( Storable a
    ) => (a -> a) -> DynVector n a -> DynVector n a
monopDyn f v@(DynVector fp1 off1 n) = if isNull fp1
    then v
    else unsafeInlineIO $ do
        let b = n*sizeOf (undefined::a)
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go (plusPtr p1 off1) p2 (n-1)
        return $ DynVector fp2 0 n

    where
        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p2 i (f v1)
            go p1 p2 (i-1)

{-# INLINE binopDynM #-}
binopDynM :: forall a b n m.
    ( PrimBase m
    , Storable a
    , Storable b
    , Monoid a
    , Monoid b
    ) => (a -> b -> a) -> Mutable m (DynVector n a) -> DynVector n b -> m ()
binopDynM f (Mutable_DynVector ref) (DynVector fp2 off2 n2) = do
    (DynVector fp1 off1 n1) <- readPrimRef ref

    let runop fp1 fp2 n = unsafePrimToPrim $
            withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go (plusPtr p1 off1) (plusPtr p2 off2) (n-1)

    unsafePrimToPrim $ if
        -- both vectors are zero: do nothing
        | isNull fp1 && isNull fp2 -> return ()

        -- only left vector is zero: allocate space and overwrite old vector
        -- FIXME: this algorithm requires two passes over the left vector
        | isNull fp1 -> do
            fp1' <- zerofp n2
            unsafePrimToPrim $ writePrimRef ref (DynVector fp1' 0 n2)
            runop fp1' fp2 n2

        -- only right vector is zero: use a temporary zero vector to run like normal
        -- FIXME: this algorithm requires an unneeded memory allocation and memory pass
        | isNull fp2 -> do
            fp2' <- zerofp n1
            runop fp1 fp2' n1

        -- both vectors nonzero: run like normal
        | otherwise -> runop fp1 fp2 n1

    where
        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p1 i (f v1 v2)
            go p1 p2 (i-1)

{-# INLINE monopDynM #-}
monopDynM :: forall a b n m.
    ( PrimMonad m
    , Storable a
    ) => (a -> a) -> Mutable m (DynVector n a) -> m ()
monopDynM f (Mutable_DynVector ref) = do
    (DynVector fp1 off1 n) <- readPrimRef ref
    if isNull fp1
        then return ()
        else unsafePrimToPrim $
            withForeignPtr fp1 $ \p1 ->
                go (plusPtr p1 off1) (n-1)

    where
        go _ (-1) = return ()
        go p1 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p1 i (f v1)
            go p1 (i-1)

-------------------

instance (Monoid r, Storable r) => Semigroup (DynVector n r) where
    {-# INLINE (+)  #-} ; (+)  = binopDyn  (+)
    {-# INLINE (+=) #-} ; (+=) = binopDynM (+)

instance (Monoid r, Cancellative r, Storable r) => Cancellative (DynVector n r) where
    {-# INLINE (-)  #-} ; (-)  = binopDyn  (-)
    {-# INLINE (-=) #-} ; (-=) = binopDynM (-)

instance (Monoid r, Storable r) => Monoid (DynVector n r) where
    {-# INLINE zero #-}
    zero = DynVector (unsafeInlineIO $ newForeignPtr_ nullPtr) 0 0

instance (Group r, Storable r) => Group (DynVector n r) where
    {-# INLINE negate #-}
    negate v = unsafeInlineIO $ do
        mv <- thaw v
        monopDynM negate mv
        unsafeFreeze mv

instance (Monoid r, Abelian r, Storable r) => Abelian (DynVector n r)

instance (Module r, Storable r) => Module (DynVector n r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopDyn  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopDynM (.*r) v

instance (FreeModule r, Storable r) => FreeModule (DynVector n r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopDyn  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopDynM (.*.)

instance (VectorSpace r, Storable r) => VectorSpace (DynVector n r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopDyn  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopDynM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopDyn  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopDynM (./.)

----------------------------------------
-- container

instance (Monoid r, ValidLogic r, Storable r, IsScalar r) => IxContainer (DynVector n r) where

    {-# INLINE (!) #-}
    (!) (DynVector fp off n) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p (off+i)


instance (FreeModule r, ValidLogic r, Storable r, IsScalar r) => FiniteModule (DynVector n r) where

    {-# INLINE dim #-}
    dim (DynVector _ _ n) = n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrArray n
        withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
        return $ DynVector fp 0 n

        where
            n = length xs

            go p []  (-1) = return ()
            go p (x:xs) i = do
                pokeElemOff p i x
                go p xs (i-1)

----------------------------------------
-- comparison

instance (Eq r, Monoid r, Storable r) => Eq_ (DynVector n r) where
    {-# INLINE (==) #-}
    (DynVector fp1 off1 n1)==(DynVector fp2 off2 n2) = unsafeInlineIO $ if
        | isNull fp1 && isNull fp2 -> return true
        | isNull fp1 -> withForeignPtr fp2 $ \p -> checkZero (plusPtr p off2) (n2-1)
        | isNull fp2 -> withForeignPtr fp1 $ \p -> checkZero (plusPtr p off1) (n1-1)
        | otherwise ->
            withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                outer (plusPtr p1 off1) (plusPtr p2 off2) (n1-1)
        where
            checkZero :: Ptr r -> Int -> IO Bool
            checkZero p (-1) = return true
            checkZero p i = do
                x <- peekElemOff p i
                if isZero x
                    then checkZero p (-1)
                    else return false

            outer :: Ptr r -> Ptr r -> Int -> IO Bool
            outer p1 p2 = go
                where
                    go (-1) = return true
                    go i = do
                        v1 <- peekElemOff p1 i
                        v2 <- peekElemOff p2 i
                        next <- go (i-1)
                        return $ v1==v2 && next

{-


{-# INLINE innerp #-}
-- innerp :: DynVector 200 Float -> DynVector 200 Float -> Float
innerp v1 v2 = go 0 (n-1)

    where
        n = 200
--         n = nat2int (Proxy::Proxy n)

        go !tot !i =  if i<4
            then goEach tot i
            else
                go (tot+(v1!(i  ) * v2!(i  ))
                       +(v1!(i-1) * v2!(i-1))
                       +(v1!(i-2) * v2!(i-2))
                       +(v1!(i-3) * v2!(i-3))
                   ) (i-4)

        goEach !tot !i = if i<0
            then tot
            else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

----------------------------------------
-- distances
-}
instance
    ( Storable r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (DynVector n r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(DynVector fp1 _ n) v2@(DynVector fp2 _ _) = {-# SCC distance_DynVector #-} if
        | isNull fp1 -> size v2
        | isNull fp2 -> size v1
        | otherwise -> sqrt $ go 0 (n-1)
        where
            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                            +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                            +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                            +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                        ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

    {-# INLINE[2] distanceUB #-}
    distanceUB v1@(DynVector fp1 _ n) v2@(DynVector fp2 _ _) ub = {-# SCC distanceUB_DynVector #-}if
        | isNull fp1 -> size v2
        | isNull fp2 -> size v1
        | otherwise -> sqrt $ go 0 (n-1)
        where
            ub2=ub*ub

            go !tot !i = if tot>ub2
                then tot
                else if i<4
                    then goEach tot i
                    else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                                +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                                +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                                +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                            ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

instance (VectorSpace r, Storable r, IsScalar r, ExpField r) => Normed (DynVector n r) where
    {-# INLINE size #-}
    size v@(DynVector fp _ n) = if isNull fp
        then 0
        else  sqrt $ go 0 (n-1)
        where
            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+v!(i  ).*.v!(i  )
                            +v!(i-1).*.v!(i-1)
                            +v!(i-2).*.v!(i-2)
                            +v!(i-3).*.v!(i-3)
                        ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+v!i*v!i) (i-1)


--------------------------------------------------------------------------------

data UnsafeDynVector n r = UnsafeDynVector {-#UNPACK#-}!(ForeignPtr r) {-#UNPACK#-}!Int

type instance Scalar (UnsafeDynVector n r) = Scalar r
type instance Logic (UnsafeDynVector n r) = Logic r
type instance Index (UnsafeDynVector n r) = Int
type instance Elem (UnsafeDynVector n r) = Scalar r

type instance UnsafeDynVector n r >< a = UnsafeDynVector n (r><a)

instance (Show r, Monoid r, Storable r) => Show (UnsafeDynVector n  r) where
    show (UnsafeDynVector fp n) = if isNull fp
        then "zero"
        else show $ unsafeInlineIO $ go (n-1) []
        where
            go (-1) xs = return $ xs
            go i    xs = withForeignPtr fp $ \p -> do
                x <- peekElemOff p i
                go (i-1) (x:xs)

instance (NFData r, Storable r) => NFData (UnsafeDynVector n r) where
    rnf (UnsafeDynVector fp n) = seq fp ()

instance (FromField r, Storable r, IsScalar r, FreeModule r) => FromRecord (UnsafeDynVector n r) where
    parseRecord r = do
        rs :: [r] <- parseRecord r
        return $ unsafeToModule rs

--------------------

newtype instance Mutable m (UnsafeDynVector n r) = Mutable_UnsafeDynVector (PrimRef m (UnsafeDynVector n r))

instance (Storable r) => IsMutable (UnsafeDynVector n r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_UnsafeDynVector ref) = readPrimRef ref
    unsafeThaw v = do
        ref <- newPrimRef v
        return $ Mutable_UnsafeDynVector ref

    copy (Mutable_UnsafeDynVector ref) = do
        (UnsafeDynVector fp1 n) <- readPrimRef ref
        let b = n*sizeOf (undefined::r)
        fp2 <- if isNull fp1
            then return fp1
            else unsafePrimToPrim $ do
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 p1 b
                return fp2
        ref2 <- newPrimRef (UnsafeDynVector fp2 n)
        return $ Mutable_UnsafeDynVector ref2

    write (Mutable_UnsafeDynVector ref) (UnsafeDynVector fp2 n2) = do
        (UnsafeDynVector fp1 n1) <- readPrimRef ref
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | isNull fp1 && isNull fp2 -> return ()

            -- only fp1 null: allocate memory then copy fp2 over
            | isNull fp1 && not isNull fp2 -> do
                fp1' <- mallocForeignPtrBytes b
                unsafePrimToPrim $ writePrimRef ref (UnsafeDynVector fp1' n2)
                withForeignPtr fp1' $ \p1 -> withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b

            -- only fp2 null: make fp1 null
            | not isNull fp1 && isNull fp2 -> unsafePrimToPrim $ writePrimRef ref (UnsafeDynVector fp2 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise ->
                withForeignPtr fp1 $ \p1 ->
                withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b
            where b = n2*sizeOf (undefined::r)

----------------------------------------
-- algebra

{-# INLINE binopUnsafeDyn #-}
binopUnsafeDyn :: forall a b n m.
    ( Storable a
    , Monoid a
    ) => (a -> a -> a) -> UnsafeDynVector n a -> UnsafeDynVector n a -> UnsafeDynVector n a
binopUnsafeDyn f v1@(UnsafeDynVector fp1 n1) v2@(UnsafeDynVector fp2 n2) = if
    | isNull fp1 && isNull fp2 -> UnsafeDynVector fp1 n1
    | isNull fp1 -> monopUnsafeDyn (f zero) v2
    | isNull fp2 -> monopUnsafeDyn (\a -> f a zero) v1
    | otherwise -> unsafeInlineIO $ do
        let b = n1*sizeOf (undefined::a)
        fp3 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
            withForeignPtr fp3 $ \p3 ->
            go p1 p2 p3 (n1-1)
        return $ UnsafeDynVector fp3 n1

    where
        go _ _ _ (-1) = return ()
        go p1 p2 p3 i = do
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p3 i (f v1 v2)
            go p1 p2 p3 (i-1)

{-# INLINE monopUnsafeDyn #-}
monopUnsafeDyn :: forall a b n m.
    ( Storable a
    ) => (a -> a) -> UnsafeDynVector n a -> UnsafeDynVector n a
monopUnsafeDyn f v@(UnsafeDynVector fp1 n) = if isNull fp1
    then v
    else unsafeInlineIO $ do
        let b = n*sizeOf (undefined::a)
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go p1 p2 (n-1)
        return $ UnsafeDynVector fp2 n

    where
        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p2 i (f v1)
            go p1 p2 (i-1)

{-# INLINE binopUnsafeDynM #-}
binopUnsafeDynM :: forall a b n m.
    ( PrimBase m
    , Storable a
    , Storable b
    , Monoid a
    , Monoid b
    ) => (a -> b -> a) -> Mutable m (UnsafeDynVector n a) -> UnsafeDynVector n b -> m ()
binopUnsafeDynM f (Mutable_UnsafeDynVector ref) (UnsafeDynVector fp2 n2) = do
    (UnsafeDynVector fp1 n1) <- readPrimRef ref
    unsafePrimToPrim $ if
        -- both vectors are zero: do nothing
        | isNull fp1 && isNull fp2 -> return ()

        -- only left vector is zero: allocate space and overwrite old vector
        -- FIXME: this algorithm requires two passes over the left vector
        | isNull fp1 -> do
            fp1' <- zerofp n2
            unsafePrimToPrim $ writePrimRef ref (UnsafeDynVector fp1' n2)
            runop fp1' fp2 n2

        -- only right vector is zero: use a temporary zero vector to run like normal
        -- FIXME: this algorithm requires an unneeded memory allocation and memory pass
        | isNull fp2 -> do
            fp2' <- zerofp n1
            runop fp1 fp2' n1

        -- both vectors nonzero: run like normal
        | otherwise -> runop fp1 fp2 n1

    where
        runop fp1 fp2 n = unsafePrimToPrim $
            withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go p1 p2 (n-1)

        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p1 i (f v1 v2)
            go p1 p2 (i-1)

{-# INLINE monopUnsafeDynM #-}
monopUnsafeDynM :: forall a b n m.
    ( PrimMonad m
    , Storable a
    ) => (a -> a) -> Mutable m (UnsafeDynVector n a) -> m ()
monopUnsafeDynM f (Mutable_UnsafeDynVector ref) = do
    (UnsafeDynVector fp1 n) <- readPrimRef ref
    if isNull fp1
        then return ()
        else unsafePrimToPrim $
            withForeignPtr fp1 $ \p1 ->
                go p1 (n-1)

    where
        go _ (-1) = return ()
        go p1 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p1 i (f v1)
            go p1 (i-1)

-------------------

instance (Monoid r, Storable r) => Semigroup (UnsafeDynVector n r) where
    {-# INLINE (+)  #-} ; (+)  = binopUnsafeDyn  (+)
    {-# INLINE (+=) #-} ; (+=) = binopUnsafeDynM (+)

instance (Monoid r, Cancellative r, Storable r) => Cancellative (UnsafeDynVector n r) where
    {-# INLINE (-)  #-} ; (-)  = binopUnsafeDyn  (-)
    {-# INLINE (-=) #-} ; (-=) = binopUnsafeDynM (-)

instance (Monoid r, Storable r) => Monoid (UnsafeDynVector n r) where
    {-# INLINE zero #-}
    zero = UnsafeDynVector (unsafeInlineIO $ newForeignPtr_ nullPtr) 0

instance (Group r, Storable r) => Group (UnsafeDynVector n r) where
    {-# INLINE negate #-}
    negate v = unsafeInlineIO $ do
        mv <- thaw v
        monopUnsafeDynM negate mv
        unsafeFreeze mv

instance (Monoid r, Abelian r, Storable r) => Abelian (UnsafeDynVector n r)

instance (Module r, Storable r) => Module (UnsafeDynVector n r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopUnsafeDyn  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopUnsafeDynM (.*r) v

instance (FreeModule r, Storable r) => FreeModule (UnsafeDynVector n r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopUnsafeDyn  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopUnsafeDynM (.*.)

instance (VectorSpace r, Storable r) => VectorSpace (UnsafeDynVector n r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopUnsafeDyn  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopUnsafeDynM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopUnsafeDyn  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopUnsafeDynM (./.)

----------------------------------------
-- container

instance (Monoid r, ValidLogic r, Storable r, IsScalar r) => IxContainer (UnsafeDynVector n r) where

    {-# INLINE (!) #-}
    (!) (UnsafeDynVector fp n) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i


instance (FreeModule r, ValidLogic r, Storable r, IsScalar r) => FiniteModule (UnsafeDynVector n r) where

    {-# INLINE dim #-}
    dim (UnsafeDynVector _ n) = n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrArray n
        withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
        return $ UnsafeDynVector fp n

        where
            n = length xs

            go p []  (-1) = return ()
            go p (x:xs) i = do
                pokeElemOff p i x
                go p xs (i-1)

----------------------------------------
-- comparison

instance (Eq r, Monoid r, Storable r) => Eq_ (UnsafeDynVector n r) where
    {-# INLINE (==) #-}
    (UnsafeDynVector fp1 n1)==(UnsafeDynVector fp2 n2) = unsafeInlineIO $ if
        | isNull fp1 && isNull fp2 -> return true
        | isNull fp1 -> withForeignPtr fp2 $ \p -> checkZero p (n2-1)
        | isNull fp2 -> withForeignPtr fp1 $ \p -> checkZero p (n1-1)
        | otherwise ->
            withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                outer p1 p2 (n1-1)
        where
            checkZero p (-1) = return true
            checkZero p i = do
                x <- peekElemOff p i
                if isZero x
                    then checkZero p (-1)
                    else return false

            outer p1 p2 = go
                where
                    go (-1) = return true
                    go i = do
                        v1 <- peekElemOff p1 i
                        v2 <- peekElemOff p2 i
                        next <- go (i-1)
                        return $ v1==v2 && next

{-


{-# INLINE innerp #-}
-- innerp :: UnsafeDynVector 200 Float -> UnsafeDynVector 200 Float -> Float
innerp v1 v2 = go 0 (n-1)

    where
        n = 200
--         n = nat2int (Proxy::Proxy n)

        go !tot !i =  if i<4
            then goEach tot i
            else
                go (tot+(v1!(i  ) * v2!(i  ))
                       +(v1!(i-1) * v2!(i-1))
                       +(v1!(i-2) * v2!(i-2))
                       +(v1!(i-3) * v2!(i-3))
                   ) (i-4)

        goEach !tot !i = if i<0
            then tot
            else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

----------------------------------------
-- distances
-}
instance
    ( Storable r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (UnsafeDynVector n r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(UnsafeDynVector fp1 n) v2@(UnsafeDynVector fp2 _) = {-# SCC distance_UnsafeDynVector #-} if
        | isNull fp1 -> size v2
        | isNull fp2 -> size v1
        | otherwise -> sqrt $ go 0 (n-1)
        where
            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                            +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                            +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                            +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                        ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

    {-# INLINE[2] distanceUB #-}
    distanceUB v1@(UnsafeDynVector fp1 n) v2@(UnsafeDynVector fp2 _) ub = {-# SCC distanceUB_UnsafeDynVector #-}if
        | isNull fp1 -> size v2
        | isNull fp2 -> size v1
        | otherwise -> sqrt $ go 0 (n-1)
        where
            ub2=ub*ub

            go !tot !i = if tot>ub2
                then tot
                else if i<4
                    then goEach tot i
                    else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                                +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                                +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                                +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                            ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

instance (VectorSpace r, Storable r, IsScalar r, ExpField r) => Normed (UnsafeDynVector n r) where
    {-# INLINE size #-}
    size v@(UnsafeDynVector fp n) = if isNull fp
        then 0
        else  sqrt $ go 0 (n-1)
        where
            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+v!(i  ).*.v!(i  )
                            +v!(i-1).*.v!(i-1)
                            +v!(i-2).*.v!(i-2)
                            +v!(i-3).*.v!(i-3)
                        ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+v!i*v!i) (i-1)

--------------------------------------------------------------------------------

newtype StaticVector (n::Nat) r = StaticVector (ForeignPtr r)

type instance Scalar (StaticVector n r) = Scalar r
type instance Logic (StaticVector n r) = Logic r
type instance Index (StaticVector n r) = Int
type instance Elem (StaticVector n r) = Scalar r

type instance StaticVector n r >< a = StaticVector n (r><a)

instance (Show r, Storable r, KnownNat n) => Show (StaticVector n  r) where
    show v = show (vec2list v)
        where
            vec2list (StaticVector fp) = unsafeInlineIO $ go (n-1) []
                where
                    n = nat2int (Proxy::Proxy n)

                    go (-1) xs = return $ xs
                    go i    xs = withForeignPtr fp $ \p -> do
                        x <- peekElemOff p i
                        go (i-1) (x:xs)

instance (NFData r, Storable r) => NFData (StaticVector n r) where
    rnf (StaticVector fp) = seq fp ()

static2dynamic :: forall n r. KnownNat n => StaticVector n r -> DynVector n r
static2dynamic (StaticVector fp) = DynVector fp 0 $ nat2int (Proxy::Proxy n)

--------------------

newtype instance Mutable m (StaticVector n r) = Mutable_StaticVector (ForeignPtr r)

instance (KnownNat n, Storable r) => IsMutable (StaticVector n r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_StaticVector fp) = return $ StaticVector fp
    unsafeThaw (StaticVector fp) = return $ Mutable_StaticVector fp

    copy (Mutable_StaticVector fp1) = unsafePrimToPrim $ do
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 p1 b
        return (Mutable_StaticVector fp2)

        where
            n = nat2int (Proxy::Proxy n)
            b = n*sizeOf (undefined::r)

    write (Mutable_StaticVector fp1) (StaticVector fp2) = unsafePrimToPrim $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            copyBytes p1 p2 b

        where
            n = nat2int (Proxy::Proxy n)
            b = n*sizeOf (undefined::r)

----------------------------------------
-- algebra

{-# INLINE binopStatic #-}
binopStatic :: forall a b n m.
    ( Storable a
    , KnownNat n
    ) => (a -> a -> a) -> StaticVector n a -> StaticVector n a -> StaticVector n a
binopStatic f v1@(StaticVector fp1) v2@(StaticVector fp2) = unsafeInlineIO $ do
    fp3 <- mallocForeignPtrBytes b
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        withForeignPtr fp3 $ \p3 ->
        go p1 p2 p3 (n-1)
    return $ StaticVector fp3

    where
        n = nat2int (Proxy::Proxy n)
        b = n*sizeOf (undefined::a)

        go _ _ _ (-1) = return ()
        go p1 p2 p3 i = do
            x0 <- peekElemOff p1 i
--             x1 <- peekElemOff p1 (i-1)
--             x2 <- peekElemOff p1 (i-2)
--             x3 <- peekElemOff p1 (i-3)

            y0 <- peekElemOff p2 i
--             y1 <- peekElemOff p2 (i-1)
--             y2 <- peekElemOff p2 (i-2)
--             y3 <- peekElemOff p2 (i-3)

            pokeElemOff p3 i     (f x0 y0)
--             pokeElemOff p3 (i-1) (f x1 y1)
--             pokeElemOff p3 (i-2) (f x2 y2)
--             pokeElemOff p3 (i-3) (f x3 y3)

            go p1 p2 p3 (i-1)
--             go p1 p2 p3 (i-4)

{-# INLINE monopStatic #-}
monopStatic :: forall a b n m.
    ( Storable a
    , KnownNat n
    ) => (a -> a) -> StaticVector n a -> StaticVector n a
monopStatic f v@(StaticVector fp1) = unsafeInlineIO $ do
    fp2 <- mallocForeignPtrBytes b
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            go p1 p2 (n-1)
    return $ StaticVector fp2

    where
        n = nat2int (Proxy::Proxy n)
        b = n*sizeOf (undefined::a)

        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p2 i (f v1)
            go p1 p2 (i-1)

{-# INLINE binopStaticM #-}
binopStaticM :: forall a b n m.
    ( PrimMonad m
    , Storable a
    , Storable b
    , KnownNat n
    ) => (a -> b -> a) -> Mutable m (StaticVector n a) -> StaticVector n b -> m ()
binopStaticM f (Mutable_StaticVector fp1) (StaticVector fp2) = unsafePrimToPrim $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 ->
        go p1 p2 (n-1)

    where
        n = nat2int (Proxy::Proxy n)

        go _ _ (-1) = return ()
        go p1 p2 i = do
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p1 i (f v1 v2)
            go p1 p2 (i-1)

{-# INLINE monopStaticM #-}
monopStaticM :: forall a b n m.
    ( PrimMonad m
    , Storable a
    , KnownNat n
    ) => (a -> a) -> Mutable m (StaticVector n a) -> m ()
monopStaticM f (Mutable_StaticVector fp1)  = unsafePrimToPrim $
    withForeignPtr fp1 $ \p1 ->
        go p1 (n-1)

    where
        n = nat2int (Proxy::Proxy n)

        go _ (-1) = return ()
        go p1 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p1 i (f v1)
            go p1 (i-1)

-------------------

instance (KnownNat n, Semigroup r, Storable r) => Semigroup (StaticVector n r) where
    {-# INLINE (+)  #-} ; (+)  = binopStatic  (+)
    {-# INLINE (+=) #-} ; (+=) = binopStaticM (+)

instance (KnownNat n, Cancellative r, Storable r) => Cancellative (StaticVector n r) where
    {-# INLINE (-)  #-} ; (-)  = binopStatic  (-)
    {-# INLINE (-=) #-} ; (-=) = binopStaticM (-)

instance (KnownNat n, Monoid r, Storable r) => Monoid (StaticVector n r) where
    {-# INLINE zero #-}
    zero = unsafeInlineIO $ do
        mv <- fmap (\fp -> Mutable_StaticVector fp) $ mallocForeignPtrArray n
        monopStaticM (const zero) mv
        unsafeFreeze mv
        where
            n = nat2int (Proxy::Proxy n)

instance (KnownNat n, Group r, Storable r) => Group (StaticVector n r) where
    {-# INLINE negate #-}
    negate v = unsafeInlineIO $ do
        mv <- thaw v
        monopStaticM negate mv
        unsafeFreeze mv

instance (KnownNat n, Abelian r, Storable r) => Abelian (StaticVector n r)

instance (KnownNat n, Module r, Storable r) => Module (StaticVector n r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopStatic  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopStaticM (.*r) v

instance (KnownNat n, FreeModule r, Storable r) => FreeModule (StaticVector n r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopStatic  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopStaticM (.*.)

instance (KnownNat n, VectorSpace r, Storable r) => VectorSpace (StaticVector n r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopStatic  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopStaticM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopStatic  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopStaticM (./.)

----------------------------------------
-- "container"

instance (KnownNat n, Monoid r, ValidLogic r, Storable r, IsScalar r) => IxContainer (StaticVector n r) where

    {-# INLINE (!) #-}
    (!) (StaticVector fp) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i


instance (KnownNat n, FreeModule r, ValidLogic r, Storable r, IsScalar r) => FiniteModule (StaticVector n r) where

    {-# INLINE dim #-}
    dim v = n
        where
            n = nat2int (Proxy::Proxy n)

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = if n /= length xs
        then error "unsafeToModule size mismatch"
        else unsafeInlineIO $ do
            fp <- mallocForeignPtrArray n
            withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
            return $ StaticVector fp

        where
            n = nat2int (Proxy::Proxy n)

            go p []  (-1) = return ()
            go p (x:xs) i = do
                pokeElemOff p i x
                go p xs (i-1)


----------------------------------------
-- comparison

instance (KnownNat n, Eq_ r, ValidLogic r, Storable r) => Eq_ (StaticVector n r) where
    {-# INLINE (==) #-}
    (StaticVector fp1)==(StaticVector fp2) = unsafeInlineIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            outer p1 p2 (n-1)
        where
            n = nat2int (Proxy::Proxy n)

            outer p1 p2 = go
                where
                    go (-1) = return true
                    go i = do
                        v1 <- peekElemOff p1 i
                        v2 <- peekElemOff p2 i
                        next <- go (i-1)
                        return $ v1==v2 && next




{-# INLINE innerp #-}
-- innerp :: StaticVector 200 Float -> StaticVector 200 Float -> Float
innerp v1 v2 = go 0 (n-1)

    where
        n = 200
--         n = nat2int (Proxy::Proxy n)

        go !tot !i =  if i<4
            then goEach tot i
            else
                go (tot+(v1!(i  ) * v2!(i  ))
                       +(v1!(i-1) * v2!(i-1))
                       +(v1!(i-2) * v2!(i-2))
                       +(v1!(i-3) * v2!(i-3))
                   ) (i-4)

        goEach !tot !i = if i<0
            then tot
            else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

----------------------------------------
-- distances

instance
    ( KnownNat n
    , Storable r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (StaticVector n r)
        where

    -- For some reason, using the dynamic vector is a little faster than a straight implementation
    {-# INLINE[2] distance #-}
    distance v1 v2 = distance (static2dynamic v1) (static2dynamic v2)
--     distance v1 v2 = sqrt $ go 0 (n-1)
--         where
--             n = nat2int (Proxy::Proxy n)
--
--             go !tot !i =  if i<4
--                 then goEach tot i
--                 else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
--                             +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
--                             +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
--                             +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
--                         ) (i-4)
--
--             goEach !tot !i = if i<0
--                 then tot
--                 else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

    {-# INLINE[2] distanceUB #-}
    distanceUB v1 v2 ub = {-# SCC distanceUB_StaticVector #-} sqrt $ go 0 (n-1)
        where
            n = nat2int (Proxy::Proxy n)
            ub2 = ub*ub

            go !tot !i = if tot>ub2
                then tot
                else if i<4
                    then goEach tot i
                    else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                                +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                                +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                                +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                            ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

instance
    ( KnownNat n
    , VectorSpace r
    , Storable r
    , IsScalar r
    , ExpField r
    ) => Normed (StaticVector n r)
        where
    {-# INLINE size #-}
    size v = sqrt $ go 0 (n-1)
        where
            n = nat2int (Proxy::Proxy n)

            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+v!(i  ) .*. v!(i  )
                            +v!(i-1) .*. v!(i-1)
                            +v!(i-2) .*. v!(i-2)
                            +v!(i-3) .*. v!(i-3)
                        ) (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot+v!i*v!i) (i-1)

--------------------------------------------------------------------------------
-- rewrite rules for faster static parameters
--
-- FIXME: Find a better home for this.
--
-- FIXME: Expand to many more naturals.

{-# INLINE[2] nat2int #-}
nat2int :: KnownNat n => Proxy n -> Int
nat2int = fromIntegral . natVal

{-# INLINE[1] nat200 #-}
nat200 :: Proxy 200 -> Int
nat200 _ = 200

{-# RULES

-- "subhask/nat2int_200" nat2int = nat200

  #-}

