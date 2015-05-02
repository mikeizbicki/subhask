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
    ( Vector
    , StaticVector
    , DynVector
    )
    where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Control.Monad.Primitive

import qualified Prelude as P
import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Category
import SubHask.Compatibility.Base

import qualified Prelude as P
import Control.Monad
import Debug.Trace

--------------------------------------------------------------------------------

-- |
--
-- FIXME:
-- We can't create "Vector"s of unknown size using type families because the @n@ parameter might unify later.
-- Maybe we can fix this with a GHC plugin?
type family Vector (n::Nat) r where
    Vector 200 r = StaticVector 200 r
    Vector n r   = DynVector n r

--------------------------------------------------------------------------------

data DynVector n r = DynVector {-#UNPACK#-}!(ForeignPtr r) {-#UNPACK#-}!Int

type instance Scalar (DynVector n r) = Scalar r
type instance Logic (DynVector n r) = Logic r
type instance Index (DynVector n r) = Int
type instance Elem (DynVector n r) = Scalar r

type instance DynVector n r >< a = DynVector n (r><a)

isNull :: ForeignPtr a -> Bool
isNull fp = unsafeInlineIO $ withForeignPtr fp $ \p -> (return $ p P.== nullPtr)

instance (Show r, Monoid r, Storable r) => Show (DynVector n  r) where
    show (DynVector fp n) = if isNull fp
        then "zero"
        else show $ unsafeInlineIO $ go (n-1) []
        where
            go (-1) xs = return $ xs
            go i    xs = withForeignPtr fp $ \p -> do
                x <- peekElemOff p i
                go (i-1) (x:xs)

instance (NFData r, Storable r) => NFData (DynVector n r) where
    rnf (DynVector fp n) = seq fp ()

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
        (DynVector fp1 n) <- readPrimRef ref
        let b = n*sizeOf (undefined::r)
        fp2 <- if isNull fp1
            then return fp1
            else unsafePrimToPrim $ do
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 p1 b
                return fp2
        ref2 <- newPrimRef (DynVector fp2 n)
        return $ Mutable_DynVector ref2

    write (Mutable_DynVector ref) (DynVector fp2 n2) = do
        (DynVector fp1 n1) <- readPrimRef ref
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | isNull fp1 && isNull fp2 -> return ()

            -- only fp1 null: allocate memory then copy fp2 over
            | isNull fp1 && not isNull fp2 -> do
                fp1' <- mallocForeignPtrBytes b
                unsafePrimToPrim $ writePrimRef ref (DynVector fp1' n2)
                withForeignPtr fp1' $ \p1 -> withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b

            -- only fp2 null: make fp1 null
            | not isNull fp1 && isNull fp2 -> unsafePrimToPrim $ writePrimRef ref (DynVector fp2 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise ->
                withForeignPtr fp1 $ \p1 ->
                withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b
            where b = n2*sizeOf (undefined::r)

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

----------------------------------------
-- algebra

{-# INLINE binopDyn #-}
binopDyn :: forall a b n m.
    ( Storable a
    , Monoid a
    ) => (a -> a -> a) -> DynVector n a -> DynVector n a -> DynVector n a
binopDyn f v1@(DynVector fp1 n1) v2@(DynVector fp2 n2) = if
    | isNull fp1 && isNull fp2 -> DynVector fp1 n1
    | isNull fp1 -> monopDyn (f zero) v2
    | isNull fp2 -> monopDyn (\a -> f a zero) v1
    | otherwise -> unsafeInlineIO $ do
        let b = n1*sizeOf (undefined::a)
        fp3 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
            withForeignPtr fp3 $ \p3 ->
            go p1 p2 p3 (n1-1)
        return $ DynVector fp3 n1

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
monopDyn f v@(DynVector fp1 n) = if isNull fp1
    then v
    else unsafeInlineIO $ do
        let b = n*sizeOf (undefined::a)
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go p1 p2 (n-1)
        return $ DynVector fp2 n

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
binopDynM f (Mutable_DynVector ref) (DynVector fp2 n2) = do
    (DynVector fp1 n1) <- readPrimRef ref
    unsafePrimToPrim $ if
        -- both vectors are zero: do nothing
        | isNull fp1 && isNull fp2 -> return ()

        -- only left vector is zero: allocate space and overwrite old vector
        -- FIXME: this algorithm requires two passes over the left vector
        | isNull fp1 -> do
            fp1' <- zerofp n2
            unsafePrimToPrim $ writePrimRef ref (DynVector fp1' n2)
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

{-# INLINE monopDynM #-}
monopDynM :: forall a b n m.
    ( PrimMonad m
    , Storable a
    ) => (a -> a) -> Mutable m (DynVector n a) -> m ()
monopDynM f (Mutable_DynVector ref) = do
    (DynVector fp1 n) <- readPrimRef ref
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

instance (Monoid r, Storable r) => Semigroup (DynVector n r) where
    {-# INLINE (+)  #-} ; (+)  = binopDyn  (+)
    {-# INLINE (+=) #-} ; (+=) = binopDynM (+)

instance (Monoid r, Cancellative r, Storable r) => Cancellative (DynVector n r) where
    {-# INLINE (-)  #-} ; (-)  = binopDyn  (-)
    {-# INLINE (-=) #-} ; (-=) = binopDynM (-)

instance (Monoid r, Storable r) => Monoid (DynVector n r) where
    {-# INLINE zero #-}
    zero = DynVector (unsafeInlineIO $ newForeignPtr_ nullPtr) 0

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
    (!) (DynVector fp n) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i


instance (Module r, ValidLogic r, Storable r, IsScalar r) => FiniteModule (DynVector n r) where

    {-# INLINE dim #-}
    dim (DynVector _ n) = n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrArray n
        withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
        return $ DynVector fp n

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
    (DynVector fp1 n1)==(DynVector fp2 n2) = unsafeInlineIO $ if
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
    distance v1@(DynVector fp1 n) v2@(DynVector fp2 _) = if
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

instance (VectorSpace r, Storable r, IsScalar r, ExpField r) => Normed (DynVector n r) where
    {-# INLINE size #-}
    size v@(DynVector fp n) = if isNull fp
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
static2dynamic (StaticVector fp) = DynVector fp $ nat2int (Proxy::Proxy n)

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


instance (KnownNat n, Module r, ValidLogic r, Storable r, IsScalar r) => FiniteModule (StaticVector n r) where

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

