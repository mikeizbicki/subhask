{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Dense vectors and linear algebra operations.
--
-- NOTE:
-- This module is a prototype for what a more fully featured linear algebra module might look like.
-- There are a number of efficiency related features that are missing.
-- In particular, matrices will get copied more often than they need to, and only the most naive dense matrix format is currently supported.
-- These limitations are due to using "hmatrix" as a backend (all operations should be at least as fast as in hmatrix).
-- Future iterations will use something like "hblas" to get finer lever control.
--
--
-- FIXME:
-- Shouldn't expose the constructors, but they're needed for the "SubHask.Algebra.Array" types.
--
-- FIXME:
-- We shouldn't need to call out to the FFI in order to get SIMD instructions.

module SubHask.Algebra.Vector
    ( SVector (..)
    , UVector (..)
    , ValidUVector
    , Unbox
    , type (+>)
    , SMatrix
    , unsafeMkSMatrix

    -- * Debug
    , safeNewByteArray
    )
    where

import qualified Prelude as P

import Control.Monad.Primitive
import Control.Monad
import Data.Primitive hiding (sizeOf)
import qualified Data.Primitive as Prim
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Test.QuickCheck.Gen (frequency)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as HM

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.SubType
import SubHask.Algebra.Vector.RMStreams

import Data.Csv (FromRecord,FromField,parseRecord)

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

"subhask/nat2int_200" nat2int = nat200

  #-}

--------------------------------------------------------------------------------

type Unbox = VU.Unbox

--------------------------------------------------------------------------------

-- | The type of dynamic or statically sized unboxed vectors.
data family UVector (n::k) r

type instance Scalar (UVector n r) = Scalar r
type instance Logic (UVector n r) = Logic r
-- type instance UVector n r >< a = UVector n (r><a)

-- type instance UVector m a >< b = Tensor_UVector (UVector m a) b
-- type family Tensor_UVector a b where
--     Tensor_UVector (UVector n r1) (UVector m r2) = UVector n r1 +> UVector m r2
--     Tensor_UVector (UVector n r1) r1 = UVector n r1 -- (r1><r2)
-- type ValidUVector n r = ( (UVector n r><Scalar r)~UVector n r, Prim r)

type ValidUVector n r = (ClassicalLogic r, Prim r)

type instance Index (UVector n r) = Int
type instance Elem (UVector n r) = Scalar r

--------------------------------------------------------------------------------

data instance UVector (n::Symbol) r = UVector_Dynamic
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- offset
    {-#UNPACK#-}!Int -- length

instance (Show r, Prim r) => Show (UVector (n::Symbol) r) where
    show (UVector_Dynamic arr off n) = if isZero n
        then "zero"
        else show $ go (extendDimensions n-1) []
        where
            go (-1) xs = xs
            go i    xs = go (i-1) (x:xs)
                where
                    x = indexByteArray arr (off+i) :: r

instance (Arbitrary r, ValidUVector n r, FreeModule r, ValidScalar r) => Arbitrary (UVector (n::Symbol) r) where
    arbitrary = frequency
        [ (1,return zero)
        , (9,fmap unsafeToModule $ replicateM 27 arbitrary)
        ]

instance (Show r, Prim r) => CoArbitrary (UVector (n::Symbol) r) where
    coarbitrary = coarbitraryShow

instance NFData (UVector (n::Symbol) r) where
    rnf (UVector_Dynamic arr _ _) = seq arr ()

instance (FromField r, ValidUVector n r, ValidScalar r, FreeModule r) => FromRecord (UVector (n::Symbol) r) where
    parseRecord r = do
        rs :: [r] <- parseRecord r
        return $ unsafeToModule rs

---------------------------------------
-- mutable

newtype instance Mutable m (UVector (n::Symbol) r)
    = Mutable_UVector (PrimRef m (UVector (n::Symbol) r))

instance Prim r => IsMutable (UVector (n::Symbol) r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_UVector ref) = readPrimRef ref
    unsafeThaw v = do
        ref <- newPrimRef v
        return $ Mutable_UVector ref

    copy (Mutable_UVector ref) = do
        (UVector_Dynamic arr1 off1 n) <- readPrimRef ref
        let b = (extendDimensions n)*Prim.sizeOf (undefined::r)
        if n==0
            then do
                ref' <- newPrimRef $ UVector_Dynamic arr1 off1 n
                return $ Mutable_UVector ref'
            else unsafePrimToPrim $ do
                marr2 <- safeNewByteArray b 16
                copyByteArray marr2 0 arr1 off1 b
                arr2 <- unsafeFreezeByteArray marr2
                ref2 <- newPrimRef (UVector_Dynamic arr2 0 n)
                return $ Mutable_UVector ref2

    write (Mutable_UVector ref') (UVector_Dynamic arr2 off2 n2) = do
        (UVector_Dynamic arr1 off1 n1) <- readPrimRef ref'
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | n1==0 && n2==0 -> return ()

            -- only arr1 null: allocate memory then copy arr2 over
            | n1==0 -> do
                marr1' <- safeNewByteArray b 16
                copyByteArray marr1' 0 arr2 off2 b
                arr1' <- unsafeFreezeByteArray marr1'
                unsafePrimToPrim $ writePrimRef ref' (UVector_Dynamic arr1' 0 n2)

            -- only arr2 null: make arr1 null
            | n2==0 -> do
                writePrimRef ref' (UVector_Dynamic arr2 0 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise -> do
                marr1 <- unsafeThawByteArray arr1
                copyByteArray marr1 off1 arr2 off2 b

        where b = (extendDimensions n2)*Prim.sizeOf (undefined::r)

----------------------------------------
-- algebra

extendDimensions :: Int -> Int
extendDimensions = roundUpToNearest 4

safeNewByteArray :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
safeNewByteArray b 16 = do
    let n=extendDimensions $ b`quot`4
    marr <- newAlignedPinnedByteArray b 16
    setByteArray marr 0 n (0::Float)
    return marr

{-# INLINE binopDynUV #-}
binopDynUV :: forall a n.
    ( Prim a
    , Monoid a
    ) => (a -> a -> a) -> UVector (n::Symbol) a -> UVector (n::Symbol) a -> UVector (n::Symbol) a
binopDynUV f v1@(UVector_Dynamic arr1 off1 n1) v2@(UVector_Dynamic arr2 off2 n2) = if
    | isZero n1 && isZero n2 -> v1
    | isZero n1 -> monopDynUV (f zero) v2
    | isZero n2 -> monopDynUV (\a -> f a zero) v1
    | otherwise -> unsafeInlineIO $ do
        let b = (extendDimensions n1)*Prim.sizeOf (undefined::a)
        marr3 <- safeNewByteArray b 16
        go marr3 (n1-1)
        arr3 <- unsafeFreezeByteArray marr3
        return $ UVector_Dynamic arr3 0 n1

    where
        go _ (-1) = return ()
        go marr3 i = do
            let v1' = indexByteArray arr1 (off1+i)
                v2' = indexByteArray arr2 (off2+i)
            writeByteArray marr3 i (f v1' v2')
            go marr3 (i-1)

{-# INLINE monopDynUV #-}
monopDynUV :: forall a n.
    ( Prim a
    ) => (a -> a) -> UVector (n::Symbol) a -> UVector (n::Symbol) a
monopDynUV f v@(UVector_Dynamic arr1 off1 n) = if n==0
    then v
    else unsafeInlineIO $ do
        let b = n*Prim.sizeOf (undefined::a)
        marr2 <- safeNewByteArray b 16
        go marr2 (n-1)
        arr2 <- unsafeFreezeByteArray marr2
        return $ UVector_Dynamic arr2 0 n

    where
        go _ (-1) = return ()
        go marr2 i = do
            let v1 = indexByteArray arr1 (off1+i)
            writeByteArray marr2 i (f v1)
            go marr2 (i-1)

instance (Monoid r, Prim r) => Semigroup (UVector (n::Symbol) r) where
    {-# INLINE (+)  #-} ; (+)  = binopDynUV  (+)

instance (Monoid r, Cancellative r, Prim r) => Cancellative (UVector (n::Symbol) r) where
    {-# INLINE (-)  #-} ; (-)  = binopDynUV  (-)

instance (Monoid r, Prim r) => Monoid (UVector (n::Symbol) r) where
    {-# INLINE zero #-}
    zero = unsafeInlineIO $ do
        marr <- safeNewByteArray 0 16
        arr <- unsafeFreezeByteArray marr
        return $ UVector_Dynamic arr 0 0

instance (Group r, Prim r) => Group (UVector (n::Symbol) r) where
    {-# INLINE negate #-}
    negate v = monopDynUV negate v

instance (Monoid r, Abelian r, Prim r) => Abelian (UVector (n::Symbol) r)

instance (Module r, ValidUVector n r) => Module (UVector (n::Symbol) r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopDynUV  (.*r) v

type instance Actor (UVector n r) = Actor r

instance (Action r, Prim r) => Action (UVector (n::Symbol) r) where
  {-# INLINE (.+)   #-}
  (.+) v r = monopDynUV (.+r) v

instance (FreeModule r, ValidUVector n r) => FreeModule (UVector (n::Symbol) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopDynUV  (.*.)

instance (Vector r, ValidUVector n r) => Vector (UVector (n::Symbol) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopDynUV  (./r) v
    {-# INLINE (./.)  #-} ;  (./.)     = binopDynUV  (./.)

----------------------------------------
-- container

instance (Monoid r, Eq r, Prim r, ValidScalar r) => IxContainer (UVector (n::Symbol) r) where

    {-# INLINE[0] (!) #-}
    (!) (UVector_Dynamic arr off _) i = indexByteArray arr (off+i)

    {-# INLINE (!~) #-}
    (!~) i e = \v -> new . newOp (\(marr,n) -> (writeByteArray marr i e :: IO ()) >> return (marr,n :: Int)) . clone $ v
       {-
                unsafeInlineIO $ do
                        let b = n*Prim.sizeOf(undefined::r)
                        marr <- newByteArray b
                        copyByteArray marr 0 arr off b
                        writeByteArray marr i e
                        arr' <- unsafeFreezeByteArray marr
                        return $ UVector_Dynamic arr' 0 n
                        -}

    {-# INLINE (%~) #-}
    (%~) i f = \v -> new . newOp (\(marr,n) -> do
                                                e <- readByteArray marr i
                                                writeByteArray marr i (f e) :: IO ()
                                                return (marr, n :: Int))
                         . clone $ v
            {-(UVector_Dynamic arr off n) =
                unsafeInlineIO $ do
                        let b = n*Prim.sizeOf(undefined::r)
                        marr <- newByteArray b
                        copyByteArray marr 0 arr off b
                        e <- readByteArray marr i
                        writeByteArray marr i (f e)
                        arr' <- unsafeFreezeByteArray marr
                        return $ UVector_Dynamic arr' 0 n-}

    {-# INLINABLE toIxList #-}
    toIxList (UVector_Dynamic arr off n) = P.zip [0..] $ go (n-1) []
        where
            go (-1) xs = xs
            go i xs = go (i-1) (indexByteArray arr (off+i) : xs)

    {-# INLINABLE imap #-}
    imap :: forall s.(ValidElem (UVector n s) s) => (Index (UVector n r) -> Elem (UVector n r) -> s) -> UVector n r -> UVector n s
    imap f (UVector_Dynamic arr off n) =
            unsafeInlineIO $ do
                    let b = n*Prim.sizeOf(undefined::s)
                    marr <- newByteArray b
                    forM_ [0..(n-1)] $ \i -> writeByteArray marr i (f i $ indexByteArray arr i)
                    arr' <- unsafeFreezeByteArray marr
                    return $ UVector_Dynamic arr' 0 n

    type ValidElem (UVector n r) s = (ValidScalar s, Monoid s, Eq s, Prim s)

instance (FreeModule r, ValidUVector n r, Eq r, ValidScalar r) => FiniteModule (UVector (n::Symbol) r) where

    {-# INLINE dim #-}
    dim (UVector_Dynamic _ _ n) = fromInteger $ toInteger n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        marr <- safeNewByteArray (n*Prim.sizeOf (undefined::r)) 16
        go marr (P.reverse xs) (n-1)
        arr <- unsafeFreezeByteArray marr
        return $ UVector_Dynamic arr 0 n

        where
            n = length xs

            go _ []  (-1) = return ()
            go marr (x:xs') i = do
                writeByteArray marr i x
                go marr xs' (i-1)

----------------------------------------
-- Stream-Fusion/Recycling

instance Prim r => Streamable (UVector (sym::Symbol) r) Int r where
        {-# INLINABLE[0] stream #-}
        stream (UVector_Dynamic arr _ n) = Stream next 0 n
                where
                        next i
                          | i < n     = Yield (indexByteArray arr i) (i+1)
                          | otherwise = Done
        {-# INLINABLE[0] unstream #-}
        unstream (Stream next i n) = unsafeInlineIO $ do
                        v <- safeNewByteArray (n*Prim.sizeOf (undefined::r)) 16
                        ent <- fillUV v i 0
                        when (ent >= n) (error $ "tried to stream more than " + show n + " elements into " + show n + "-dim Vector.") -- impossible if types are correct!
                                                                                                                                      -- abort as we have corrupted the memory anyways..
                        a <- unsafeFreezeByteArray v
                        return (UVector_Dynamic a 0 n)
                where
                        fillUV arr i' pos = case next i' of
                                         Yield x i'' -> writeByteArray arr pos x >> fillUV arr i'' (pos+1)
                                         Skip i''    -> fillUV arr i'' (pos+1)
                                         Done        -> return i'

instance Prim r => Streamable (MutableByteArray RealWorld, Int) Int r where
        {-# INLINABLE[0] stream #-}
        stream (marr, n) = Stream next 0 n
                where
                        next i
                          | i < n     = Yield (unsafeInlineIO $ readByteArray marr i) (i+1)
                          | otherwise = Done
        {-# INLINABLE[0] unstream #-}
        unstream (Stream next i n) = unsafeInlineIO $ do
                        marr <- newByteArray (n*Prim.sizeOf(undefined :: r))
                        ent <- fillUV marr i 0
                        when (ent >= n) (error $ "tried to stream more than " + show n + " elements into " + show n + "-dim Vector.") -- impossible if types are correct!
                                                                                                                                      -- abort as we have corrupted the memory anyways..
                        return (marr, n)
                where
                        fillUV arr i' pos = case next i' of
                                         Yield x i'' -> writeByteArray arr pos x >> fillUV arr i'' (pos+1)
                                         Skip i''    -> fillUV arr i'' (pos+1)
                                         Done        -> return i'

instance (Prim r, r ~ Scalar r) => Recycleable IO (MutableByteArray RealWorld, Int) (UVector (sym::Symbol) r) where
        {-# INLINE new #-}
        new = newVec
        {-# INLINE clone #-}
        clone = cloneVec
        -- {-# INLINE[0] new #-}
        -- new :: forall (n :: Symbol) r. New IO (MutableByteArray RealWorld, Int) -> UVector n r
        -- new (New init) = unsafeInlineIO $ do
        --                                 (marr, n) <- init
        --                                 arr <- unsafeFreezeByteArray marr
        --                                 return $ UVector_Dynamic arr 0 n

        -- {-# INLINE[0] clone #-}
        -- clone :: forall r sym.(Prim r, r ~ Scalar r) => UVector (sym::Symbol) r -> New IO (MutableByteArray RealWorld, Int)
        -- clone (UVector_Dynamic a off n) = New $ do
        --                                     let b = n*Prim.sizeOf (undefined :: r)
        --                                     marr <- newByteArray b
        --                                     copyByteArray marr 0 a off b
        --                                     return (marr,n)

{-# INLINE[0] newVec #-}
newVec :: forall (n :: Symbol) r. New IO (MutableByteArray RealWorld, Int) -> UVector n r
newVec (New init) = unsafeInlineIO $ do
                                (marr, n) <- init
                                arr <- unsafeFreezeByteArray marr
                                return $ UVector_Dynamic arr 0 n

{-# INLINE[0] cloneVec #-}
cloneVec :: forall r sym.(Prim r, r ~ Scalar r) => UVector (sym::Symbol) r -> New IO (MutableByteArray RealWorld, Int)
cloneVec (UVector_Dynamic a off n) = New $ do
                                    let b = n*Prim.sizeOf (undefined :: r)
                                    marr <- newByteArray b
                                    copyByteArray marr 0 a off b
                                    return (marr,n)

{-# RULES
"clone/new [UVector]"[~0] forall p. cloneVec (newVec p) = p
  #-}


instance Prim r => Fillable IO (MutableByteArray RealWorld, Int) Int r where
        {-# INLINABLE[0] fill #-}
        fill (Stream next i n) = New $ do
                        arr <- safeNewByteArray (n*Prim.sizeOf (undefined::r)) 16
                        ent <- fillUV arr i 0
                        when (ent >= n) (error $ "tried to stream more than " + show n + " elements into " + show n + "-dim Vector.") -- impossible if types are correct!
                                                                                                                                      -- abort as we have corrupted the memory anyways..
                        return (arr,n)
                where
                        fillUV arr i' pos = case next i' of
                                         Yield x i'' -> writeByteArray arr pos x >> fillUV arr i'' (pos+1)
                                         Skip i''    -> fillUV arr i'' (pos+1)
                                         Done        -> return i'

instance (Prim r, r ~ Scalar r) => RMStreams IO (UVector (sym::Symbol) r) (MutableByteArray RealWorld, Int) Int r

----------------------------------------
-- comparison

isConst :: (Prim r, Eq r) => UVector (n::Symbol) r -> r -> Logic r
isConst (UVector_Dynamic arr1 off1 n1) c = go (off1+n1-1)
    where
        go (-1) = true
        go i = indexByteArray arr1 i==c && go (i-1)

instance (Eq r, Monoid r, Prim r) => Eq (UVector (n::Symbol) r) where
    {-# INLINE (==) #-}
    v1@(UVector_Dynamic arr1 off1 n1)==v2@(UVector_Dynamic arr2 off2 n2) = if
        | isZero n1 && isZero n2 -> true
        | isZero n1 -> isConst v2 zero
        | isZero n2 -> isConst v1 zero
        | otherwise -> go (n1-1)
        where
            go (-1) = true
            go i = v1'==v2' && go (i-1)
                where
                    v1' = indexByteArray arr1 (off1+i) :: r
                    v2' = indexByteArray arr2 (off2+i) :: r

----------------------------------------
-- distances

instance
    ( Prim r
    , ExpField r
    , Normed r
    , Ord r
    , Logic r~Bool
    , ValidScalar r
    , Vector r
    ) => Metric (UVector (n::Symbol) r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(UVector_Dynamic _ _ n1) v2@(UVector_Dynamic _ _ n2)
      = if
        | isZero n1 -> size v2
        | isZero n2 -> size v1
        | otherwise -> sqrt $ go 0 (n1-1)
        where
            go !tot !i =  if i<4
                then goEach tot i
                else go (tot+(v1!(i  ) - v2!(i  )) .*. (v1!(i  ) - v2!(i  ))
                            +(v1!(i-1) - v2!(i-1)) .*. (v1!(i-1) - v2!(i-1))
                            +(v1!(i-2) - v2!(i-2)) .*. (v1!(i-2) - v2!(i-2))
                            +(v1!(i-3) - v2!(i-3)) .*. (v1!(i-3) - v2!(i-3))
                        )
                        (i-4)

            goEach !tot !i = if i<0
                then tot
                else goEach (tot + (v1!i-v2!i).*.(v1!i-v2!i)) (i-1)

    {-# INLINE[2] distanceUB #-}
    distanceUB v1@(UVector_Dynamic _ _ n1) v2@(UVector_Dynamic _ _ n2) ub
      = if
        | isZero n1 -> size v2
        | isZero n2 -> size v1
        | otherwise -> go 0 (n1-1)
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
                            )
                            (i-4)

            goEach !tot !i = if tot>ub2
                then tot
                else if i<0
                        then sqrt $ tot
                        else goEach (tot + (v1!i-v2!i).*.(v1!i-v2!i)) (i-1)

instance (Vector r, Prim r, ValidScalar r, ExpField r) => Normed (UVector (n::Symbol) r) where
    {-# INLINE size #-}
    size v@(UVector_Dynamic _ off n) = if isZero n
        then 0
        else sqrt $ go 0 (off+n-1)
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

instance
    ( Vector r
    , ValidUVector n r
    , ValidScalar r
    , ExpField r
    ) => Banach (UVector (n::Symbol) r)

-- | Construct an "UMatrix"
unsafeMkUMatrix ::
    ( Vector (UVector m r)
    , Vector (UVector n r)
    , ToFromVector (UVector m r)
    , ToFromVector (UVector n r)
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Int -> Int -> [r] -> UMatrix r m n
unsafeMkUMatrix m n rs = Mat_ $ (m HM.>< n) rs

-- | A slightly more convenient type for linear functions between "UVector"s
type UMatrix r m n = UVector m r +> UVector n r

instance
    ( Vector r
    , ValidUVector n r
    , ValidScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Hilbert (UVector (n::Symbol) r)
        where

    type Square (UVector (n::Symbol) r) = UVector n r +> UVector n r

    v1><v2 = unsafeMkUMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

    mXv m v = m $ v
    vXm v m = trans m $ v

    {-# INLINE (<>) #-}
    v1@(UVector_Dynamic _ _ n)<>v2@(UVector_Dynamic _ _ _) = if isZero n
        then 0
        else go 0 (n-1)
        where
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
                else goEach (tot+(v1!i * v2!i)) (i-1)

instance MatrixField r => ToFromVector (UVector (n::Symbol) r) where
    toVector (UVector_Dynamic _ _ _) = undefined
    fromVector _ = UVector_Dynamic fp off n
        where
            (fp,off,n) = undefined -- VS.unsafeToForeignPtr v

instance MatrixField r => Normed (UVector m r +> UVector n r) where
    size (Id_ r) = r
    size (Mat_ m) = HM.det m

--------------------------------------------------------------------------------
-- helper functions for memory management

-- | does the foreign pointer equal null?
isNull :: ForeignPtr a -> Bool
isNull fp = unsafeInlineIO $ withForeignPtr fp $ \p -> (return $ p P.== nullPtr)

-- | allocates a ForeignPtr that is filled with n "zero"s
zerofp :: forall r. (Storable r, Monoid r) => Int -> IO (ForeignPtr r)
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

-- | The type of dynamic or statically sized vectors implemented using the FFI.
data family SVector (n::k) r

type instance Scalar (SVector n r) = Scalar r
type instance Logic (SVector n r) = Logic r

-- type instance SVector m a >< b = Tensor_SVector (SVector m a) b
-- type family Tensor_SVector a b where
--     Tensor_SVector (SVector n r1) (SVector m r2) = SVector n r1 +> SVector m r2
--     Tensor_SVector (SVector n r1) r1 = SVector n r1 -- (r1><r2)
-- type ValidSVector n r = ( (SVector n r><Scalar r)~SVector n r, Storable r)

type ValidSVector n r = (ClassicalLogic r, Storable r)

type instance Index (SVector n r) = Int
type instance Elem (SVector n r) = Scalar r

--------------------------------------------------------------------------------

data instance SVector (n::Symbol) r = SVector_Dynamic
    {-#UNPACK#-}!(ForeignPtr r)
    {-#UNPACK#-}!Int -- offset
    {-#UNPACK#-}!Int -- length

instance (Show r, ValidSVector n r) => Show (SVector (n::Symbol) r) where
    show (SVector_Dynamic fp off n) = if isNull fp
        then "zero"
        else show $ unsafeInlineIO $ go (n-1) []
        where
            go (-1) xs = return $ xs
            go i    xs = withForeignPtr fp $ \p -> do
                x <- peekElemOff p (off+i)
                go (i-1) (x:xs)

instance (Arbitrary r, ValidSVector n r, FreeModule r, ValidScalar r) => Arbitrary (SVector (n::Symbol) r) where
    arbitrary = frequency
        [ (1,return zero)
        , (9,fmap unsafeToModule $ replicateM 27 arbitrary)
        ]

instance NFData (SVector (n::Symbol) r) where
    rnf (SVector_Dynamic fp _ _) = seq fp ()

instance (FromField r, ValidSVector n r, ValidScalar r, FreeModule r) => FromRecord (SVector (n::Symbol) r) where
    parseRecord r = do
        rs :: [r] <- parseRecord r
        return $ unsafeToModule rs

---------------------------------------
-- mutable

newtype instance Mutable m (SVector (n::Symbol) r) = Mutable_SVector (PrimRef m (SVector (n::Symbol) r))

instance (ValidSVector n r) => IsMutable (SVector (n::Symbol) r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_SVector ref) = readPrimRef ref
    unsafeThaw v = do
        ref <- newPrimRef v
        return $ Mutable_SVector ref

    copy (Mutable_SVector ref) = do
        (SVector_Dynamic fp1 off1 n) <- readPrimRef ref
        let b = n*sizeOf (undefined::r)
        fp2 <- if isNull fp1
            then return fp1
            else unsafePrimToPrim $ do
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 (plusPtr p1 off1) b
                return fp2
        ref2 <- newPrimRef (SVector_Dynamic fp2 0 n)
        return $ Mutable_SVector ref2

    write (Mutable_SVector ref) (SVector_Dynamic fp2 _ n2) = do
        (SVector_Dynamic fp1 _ n1) <- readPrimRef ref
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | isNull fp1 && isNull fp2 -> return ()

            -- only fp1 null: allocate memory then copy fp2 over
            | isNull fp1 && not isNull fp2 -> do
                fp1' <- mallocForeignPtrBytes b
                unsafePrimToPrim $ writePrimRef ref (SVector_Dynamic fp1' 0 n2)
                withForeignPtr fp1' $ \p1 -> withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b

            -- only fp2 null: make fp1 null
            | not isNull fp1 && isNull fp2 -> unsafePrimToPrim $ writePrimRef ref (SVector_Dynamic fp2 0 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise ->
                withForeignPtr fp1 $ \p1 ->
                withForeignPtr fp2 $ \p2 ->
                    copyBytes p1 p2 b
            where b = n2*sizeOf (undefined::r)

----------------------------------------
-- algebra

{-# INLINE binopDyn #-}
binopDyn :: forall a n.
    ( Storable a
    , Monoid a
    ) => (a -> a -> a) -> SVector (n::Symbol) a -> SVector (n::Symbol) a -> SVector (n::Symbol) a
binopDyn f v1@(SVector_Dynamic fp1 off1 n1) v2@(SVector_Dynamic fp2 off2 _) = if
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
        return $ SVector_Dynamic fp3 0 n1

    where
        go _ _ _ (-1) = return ()
        go p1 p2 p3 i = do
            v1' <- peekElemOff p1 i
            v2' <- peekElemOff p2 i
            pokeElemOff p3 i (f v1' v2')
            go p1 p2 p3 (i-1)

{-# INLINE monopDyn #-}
monopDyn :: forall a n.
    ( Storable a
    ) => (a -> a) -> SVector (n::Symbol) a -> SVector (n::Symbol) a
monopDyn f v@(SVector_Dynamic fp1 off1 n) = if isNull fp1
    then v
    else unsafeInlineIO $ do
        let b = n*sizeOf (undefined::a)
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                go (plusPtr p1 off1) p2 (n-1)
        return $ SVector_Dynamic fp2 0 n

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
    ) => (a -> b -> a) -> Mutable m (SVector (n::Symbol) a) -> SVector n b -> m ()
binopDynM f (Mutable_SVector ref) (SVector_Dynamic fp2 off2 n2) = do
    (SVector_Dynamic fp1 off1 n1) <- readPrimRef ref

    let runop fp1' fp2' n = unsafePrimToPrim $
            withForeignPtr fp1' $ \p1 ->
            withForeignPtr fp2' $ \p2 ->
                go (plusPtr p1 off1) (plusPtr p2 off2) (n-1)

    unsafePrimToPrim $ if
        -- both vectors are zero: do nothing
        | isNull fp1 && isNull fp2 -> return ()

        -- only left vector is zero: allocate space and overwrite old vector
        -- FIXME: this algorithm requires two passes over the left vector
        | isNull fp1 -> do
            fp1' <- zerofp n2
            unsafePrimToPrim $ writePrimRef ref (SVector_Dynamic fp1' 0 n2)
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
monopDynM :: forall a n m.
    ( PrimMonad m
    , Storable a
    ) => (a -> a) -> Mutable m (SVector (n::Symbol) a) -> m ()
monopDynM f (Mutable_SVector ref) = do
    (SVector_Dynamic fp1 off1 n) <- readPrimRef ref
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

instance (Monoid r, ValidSVector n r) => Semigroup (SVector (n::Symbol) r) where
    {-# INLINE (+)  #-} ; (+)  = binopDyn  (+)
    {-# INLINE (+=) #-} ; (+=) = binopDynM (+)

instance (Monoid r, Cancellative r, ValidSVector n r) => Cancellative (SVector (n::Symbol) r) where
    {-# INLINE (-)  #-} ; (-)  = binopDyn  (-)
    {-# INLINE (-=) #-} ; (-=) = binopDynM (-)

instance (Monoid r, ValidSVector n r) => Monoid (SVector (n::Symbol) r) where
    {-# INLINE zero #-}
    zero = SVector_Dynamic (unsafeInlineIO $ newForeignPtr_ nullPtr) 0 0

instance (Group r, ValidSVector n r) => Group (SVector (n::Symbol) r) where
    {-# INLINE negate #-}
    negate v = unsafeInlineIO $ do
        mv <- thaw v
        monopDynM negate mv
        unsafeFreeze mv

instance (Monoid r, Abelian r, ValidSVector n r) => Abelian (SVector (n::Symbol) r)

instance (Module r, ValidSVector n r, ValidScalar r) => Module (SVector (n::Symbol) r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopDyn  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopDynM (.*r) v

instance (FreeModule r, ValidSVector n r, ValidScalar r) => FreeModule (SVector (n::Symbol) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopDyn  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopDynM (.*.)

instance (Vector r, ValidSVector n r, ValidScalar r) => Vector (SVector (n::Symbol) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopDyn  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopDynM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopDyn  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopDynM (./.)

----------------------------------------
-- container

instance
    ( Monoid r
    , Eq r
    , ValidSVector n r
    , ValidScalar r
    , FreeModule r
    ) => IxContainer (SVector (n::Symbol) r)
        where

    {-# INLINE (!) #-}
    (!) (SVector_Dynamic fp off _) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p (off+i)

    {-# INLINE (!~) #-}
    (!~) i e (SVector_Dynamic fp1 off n) =
            unsafeInlineIO $ do
                let b = n*sizeOf(undefined::r)
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \ptr1 ->
                    withForeignPtr fp2 $ \ptr2 -> do
                        copyBytes ptr2 (plusPtr ptr1 off) b
                        pokeElemOff ptr2 i e
                return $ (SVector_Dynamic fp2 0 n)

    {-# INLINE (%~) #-}
    (%~) i f (SVector_Dynamic fp1 off n) =
            unsafeInlineIO $ do
                let b = n*sizeOf(undefined::r)
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \ptr1 ->
                    withForeignPtr fp2 $ \ptr2 -> do
                        copyBytes ptr2 (plusPtr ptr1 off) b
                        e <- peekElemOff ptr2 i
                        pokeElemOff ptr2 i (f e)
                return $ (SVector_Dynamic fp2 0 n)

    {-# INLINABLE toIxList #-}
    toIxList v = P.zip [0..] $ go (dim v-1) []
        where
            go (-1) xs = xs
            go    i xs = go (i-1) (v!i : xs)

    {-# INLINABLE imap #-}
    imap f v = unsafeToModule $ imap f $ values v

    type ValidElem (SVector n r) e = (ClassicalLogic e, ValidScalar e, FiniteModule e, ValidSVector n e)

instance (FreeModule r, Eq r, ValidSVector n r, ValidScalar r) => FiniteModule (SVector (n::Symbol) r) where

    {-# INLINE dim #-}
    dim (SVector_Dynamic _ _ n) = fromInteger $ toInteger n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrArray n
        withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
        return $ SVector_Dynamic fp 0 n

        where
            n = length xs

            go _ []  (-1) = return ()
            go p (x:xs') i = do
                pokeElemOff p i x
                go p xs' (i-1)

----------------------------------------
-- comparison

instance (Eq r, Monoid r, ClassicalLogic r, ValidSVector n r) => Eq (SVector (n::Symbol) r) where
    {-# INLINE (==) #-}
    (SVector_Dynamic fp1 off1 n1)==(SVector_Dynamic fp2 off2 n2) = unsafeInlineIO $ if
        | isNull fp1 && isNull fp2 -> return true
        | isNull fp1 -> withForeignPtr fp2 $ \p -> checkZero (plusPtr p off2) (n2-1)
        | isNull fp2 -> withForeignPtr fp1 $ \p -> checkZero (plusPtr p off1) (n1-1)
        | otherwise ->
            withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                outer (plusPtr p1 off1) (plusPtr p2 off2) (n1-1)
        where
            checkZero :: Ptr r -> Int -> IO Bool
            checkZero _ (-1) = return true
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

----------------------------------------
-- distances

instance
    ( ValidSVector n r
    , ExpField r
    , Normed r
    , Ord r
    , Logic r~Bool
    , ValidScalar r
    , Vector r
    ) => Metric (SVector (n::Symbol) r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(SVector_Dynamic fp1 _ n) v2@(SVector_Dynamic fp2 _ _) = if
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
    distanceUB v1@(SVector_Dynamic fp1 _ n) v2@(SVector_Dynamic fp2 _ _) ub = if
        | isNull fp1 -> size v2
        | isNull fp2 -> size v1
        | otherwise -> go 0 (n-1)
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

            goEach !tot !i = if tot>ub2
                then tot
                else if i<0
                        then sqrt $ tot
                        else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

instance (Vector r, ValidSVector n r, ValidScalar r, ExpField r) => Normed (SVector (n::Symbol) r) where
    {-# INLINE size #-}
    size v@(SVector_Dynamic fp _ n) = if isNull fp
        then 0
        else sqrt $ go 0 (n-1)
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

instance
    ( Vector r
    , ValidSVector n r
    , ValidScalar r
    , ExpField r
    ) => Banach (SVector (n::Symbol) r)

-- | A slightly more convenient type for linear functions between "SVector"s
type SMatrix r m n = SVector m r +> SVector n r

-- | Construct an "SMatrix"
unsafeMkSMatrix ::
    ( Vector (SVector m r)
    , Vector (SVector n r)
    , ToFromVector (SVector m r)
    , ToFromVector (SVector n r)
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Int -> Int -> [r] -> SMatrix r m n
unsafeMkSMatrix m n rs = Mat_ $ (m HM.>< n) rs

instance
    ( Vector r
    , ValidSVector n r
    , ValidScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Hilbert (SVector (n::Symbol) r)
        where

    type Square (SVector (n::Symbol) r) = SVector n r +> SVector n r

    v1><v2 = unsafeMkSMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

    mXv m v = m $ v
    vXm v m = trans m $ v

    {-# INLINE (<>) #-}
    v1@(SVector_Dynamic fp1 _ _)<>v2@(SVector_Dynamic fp2 _ n) = if isNull fp1 || isNull fp2
        then 0
        else go 0 (n-1)
        where
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
                else goEach (tot+(v1!i * v2!i)) (i-1)


--------------------------------------------------------------------------------

newtype instance SVector (n::Nat) r = SVector_Nat (ForeignPtr r)

instance (Show r, ValidSVector n r, KnownNat n) => Show (SVector n  r) where
    show v = show (vec2list v)
        where
            n = nat2int (Proxy::Proxy n)

            vec2list (SVector_Nat fp) = unsafeInlineIO $ go (n-1) []
                where
                    go (-1) xs = return $ xs
                    go i    xs = withForeignPtr fp $ \p -> do
                        x <- peekElemOff p i
                        go (i-1) (x:xs)

instance
    ( KnownNat n
    , Arbitrary r
    , ValidSVector n r
    , FreeModule r
    , ValidScalar r
    ) => Arbitrary (SVector (n::Nat) r)
        where
    arbitrary = do
        xs <- replicateM n arbitrary
        return $ unsafeToModule xs
        where
            n = nat2int (Proxy::Proxy n)

instance ValidSVector n r => NFData (SVector (n::Nat) r) where
    rnf (SVector_Nat fp) = seq fp ()

static2dynamic :: forall n m r. KnownNat n => SVector (n::Nat) r -> SVector (m::Symbol) r
static2dynamic (SVector_Nat fp) = SVector_Dynamic fp 0 $ nat2int (Proxy::Proxy n)

newtype instance Mutable m (SVector (n::Nat) r) = Mutable_SVector_Nat (ForeignPtr r)

instance (KnownNat n, ValidSVector n r) => IsMutable (SVector (n::Nat) r) where
    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_SVector_Nat fp) = return $ SVector_Nat fp
    unsafeThaw (SVector_Nat fp) = return $ Mutable_SVector_Nat fp

    copy (Mutable_SVector_Nat fp1) = unsafePrimToPrim $ do
        fp2 <- mallocForeignPtrBytes b
        withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> copyBytes p2 p1 b
        return (Mutable_SVector_Nat fp2)

        where
            n = nat2int (Proxy::Proxy n)
            b = n*sizeOf (undefined::r)

    write (Mutable_SVector_Nat fp1) (SVector_Nat fp2) = unsafePrimToPrim $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            copyBytes p1 p2 b

        where
            n = nat2int (Proxy::Proxy n)
            b = n*sizeOf (undefined::r)

----------------------------------------
-- algebra

{-# INLINE binopStatic #-}
binopStatic :: forall a n.
    ( Storable a
    , KnownNat n
    ) => (a -> a -> a) -> SVector n a -> SVector n a -> SVector n a
binopStatic f (SVector_Nat fp1) (SVector_Nat fp2) = unsafeInlineIO $ do
    fp3 <- mallocForeignPtrBytes b
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        withForeignPtr fp3 $ \p3 ->
        go p1 p2 p3 (n-1)
    return $ SVector_Nat fp3

    where
        n = nat2int (Proxy::Proxy n)
        b = n*sizeOf (undefined::a)

        go _ _ _ (-1) = return ()
        go p1 p2 p3 i = do
            x0 <- peekElemOff p1 i
            y0 <- peekElemOff p2 i
            pokeElemOff p3 i     (f x0 y0)
            go p1 p2 p3 (i-1)

{-# INLINE monopStatic #-}
monopStatic :: forall a n.
    ( Storable a
    , KnownNat n
    ) => (a -> a) -> SVector n a -> SVector n a
monopStatic f (SVector_Nat fp1) = unsafeInlineIO $ do
    fp2 <- mallocForeignPtrBytes b
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            go p1 p2 (n-1)
    return $ SVector_Nat fp2

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
    ) => (a -> b -> a) -> Mutable m (SVector n a) -> SVector n b -> m ()
binopStaticM f (Mutable_SVector_Nat fp1) (SVector_Nat fp2) = unsafePrimToPrim $
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
monopStaticM :: forall a n m.
    ( PrimMonad m
    , Storable a
    , KnownNat n
    ) => (a -> a) -> Mutable m (SVector n a) -> m ()
monopStaticM f (Mutable_SVector_Nat fp1)  = unsafePrimToPrim $
    withForeignPtr fp1 $ \p1 ->
        go p1 (n-1)

    where
        n = nat2int (Proxy::Proxy n)

        go _ (-1) = return ()
        go p1 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p1 i (f v1)
            go p1 (i-1)

instance (KnownNat n, Semigroup r, ValidSVector n r) => Semigroup (SVector (n::Nat) r) where
    {-# INLINE (+)  #-} ; (+)  = binopStatic  (+)
    {-# INLINE (+=) #-} ; (+=) = binopStaticM (+)

instance (KnownNat n, Cancellative r, ValidSVector n r) => Cancellative (SVector (n::Nat) r) where
    {-# INLINE (-)  #-} ; (-)  = binopStatic  (-)
    {-# INLINE (-=) #-} ; (-=) = binopStaticM (-)

instance (KnownNat n, Monoid r, ValidSVector n r) => Monoid (SVector (n::Nat) r) where
    {-# INLINE zero #-}
    zero = unsafeInlineIO $ do
        mv <- fmap (\fp -> Mutable_SVector_Nat fp) $ mallocForeignPtrArray n
        monopStaticM (const zero) mv
        unsafeFreeze mv
        where
            n = nat2int (Proxy::Proxy n)

instance (KnownNat n, Group r, ValidSVector n r) => Group (SVector (n::Nat) r) where
    {-# INLINE negate #-}
    negate v = unsafeInlineIO $ do
        mv <- thaw v
        monopStaticM negate mv
        unsafeFreeze mv

instance (KnownNat n, Abelian r, ValidSVector n r) => Abelian (SVector (n::Nat) r)

instance (KnownNat n, Module r, ValidSVector n r, ValidScalar r) => Module (SVector (n::Nat) r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopStatic  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopStaticM (.*r) v

instance (KnownNat n, FreeModule r, ValidSVector n r, ValidScalar r) => FreeModule (SVector (n::Nat) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopStatic  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopStaticM (.*.)

instance (KnownNat n, Vector r, ValidSVector n r, ValidScalar r) => Vector (SVector (n::Nat) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopStatic  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopStaticM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopStatic  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopStaticM (./.)

----------------------------------------
-- "container"

instance
    ( KnownNat n
    , Monoid r
    , Eq r
    , ValidSVector n r
    , ValidScalar r
    , FreeModule r
    ) => IxContainer (SVector (n::Nat) r)
        where

    {-# INLINE (!) #-}
    (!) (SVector_Nat fp) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i

    {-# INLINE (!~) #-}
    (!~) i e (SVector_Nat fp1) =
            unsafeInlineIO $ do
                let b = n*sizeOf(undefined::r)
                    n = nat2int (Proxy::Proxy n)
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \ptr1 ->
                    withForeignPtr fp2 $ \ptr2 -> do
                        copyBytes ptr2 ptr1 b
                        pokeElemOff ptr2 i e
                return $ (SVector_Nat fp2)

    {-# INLINE (%~) #-}
    (%~) i f (SVector_Nat fp1) =
            unsafeInlineIO $ do
                let b = n*sizeOf(undefined::r)
                    n = nat2int (Proxy::Proxy n)
                fp2 <- mallocForeignPtrBytes b
                withForeignPtr fp1 $ \ptr1 ->
                    withForeignPtr fp2 $ \ptr2 -> do
                        copyBytes ptr2 ptr1 b
                        e <- peekElemOff ptr2 i
                        pokeElemOff ptr2 i (f e)
                return $ (SVector_Nat fp2)


    {-# INLINABLE toIxList #-}
    toIxList v = P.zip [0..] $ go (dim v-1) []
        where
            go (-1) xs = xs
            go    i xs = go (i-1) (v!i : xs)

    {-# INLINABLE imap #-}
    imap f v = unsafeToModule $ imap f $ values v

    type ValidElem (SVector n r) e = (ClassicalLogic e, ValidScalar e, FiniteModule e, ValidSVector n e)

instance
    ( KnownNat n
    , FreeModule r
    , Eq r
    , ValidSVector n r
    , ValidScalar r
    ) => FiniteModule (SVector (n::Nat) r)
        where

    {-# INLINE dim #-}
    dim _ = fromInteger $ toInteger $ nat2int (Proxy::Proxy n)

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = if n /= length xs
        then error "unsafeToModule size mismatch"
        else unsafeInlineIO $ do
            fp <- mallocForeignPtrArray n
            withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
            return $ SVector_Nat fp

        where
            n = nat2int (Proxy::Proxy n)

            go _ []  (-1) = return ()
            go p (x:xs') i = do
                pokeElemOff p i x
                go p xs' (i-1)


----------------------------------------
-- comparison

instance (KnownNat n, Eq r, Eq r, ValidSVector n r) => Eq (SVector (n::Nat) r) where
    {-# INLINE (==) #-}
    (SVector_Nat fp1)==(SVector_Nat fp2) = unsafeInlineIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            outer p1 p2 (n-1)
        where
            n = nat2int (Proxy::Proxy n)

            outer :: Ptr r -> Ptr r -> Int -> IO (Logic r)
            outer p1 p2 = go
                where
                    go (-1) = return true
                    go i = do
                        v1 <- peekElemOff p1 i
                        v2 <- peekElemOff p2 i
                        next <- go (i-1)
                        return $ v1==v2 && next

----------------------------------------
-- distances

instance
    ( KnownNat n
    , ValidSVector n r
    , ExpField r
    , Normed r
    , Ord r
    , Logic r~Bool
    , ValidScalar r
    , Vector r
    , ValidSVector "dyn" r
    ) => Metric (SVector (n::Nat) r)
        where

    -- For some reason, using the dynamic vector is a little faster than a straight implementation
    {-# INLINE[2] distance #-}
    distance v1 v2 = distance (static2dynamic v1) (static2dynamic v2 :: SVector "dyn" r)

    {-# INLINE[2] distanceUB #-}
    distanceUB v1 v2 ub = go 0 (n-1)
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

            goEach !tot !i = if tot>ub2
                then tot
                else if i<0
                        then sqrt $ tot
                        else goEach (tot+(v1!i - v2!i) * (v1!i - v2!i)) (i-1)

instance
    ( KnownNat n
    , Vector r
    , ValidSVector n r
    , ValidScalar r
    , ExpField r
    ) => Normed (SVector (n::Nat) r)
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

instance
    ( KnownNat n
    , Vector r
    , ValidSVector n r
    , ValidScalar r
    , ExpField r
    , ValidSVector "dyn" r
    ) => Banach (SVector (n::Nat) r)

instance
    ( KnownNat n
    , Vector r
    , ValidScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , ValidSVector n r
    , ValidSVector "dyn" r
    , P.Num (HM.Vector r)
    ) => Hilbert (SVector (n::Nat) r)
        where

    type Square (SVector (n::Nat) r) = SVector n r +> SVector n r

    v1><v2 = unsafeMkSMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

    mXv m v = m $ v
    vXm v m = trans m $ v

    {-# INLINE (<>) #-}
    v1<>v2 = go 0 (n-1)
        where
            n = nat2int (Proxy::Proxy n)

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
                else goEach (tot+(v1!i * v2!i)) (i-1)

--------------------------------------------------------------------------------

type MatrixField r =
    ( ValidScalar r
    , Vector r
    , Field r
    , HM.Field r
    , HM.Container HM.Vector r
    , HM.Product r
   )

class ToFromVector a where
    toVector   :: a -> VS.Vector (Scalar a)
    fromVector :: VS.Vector (Scalar a) -> a

instance ToFromVector Double where
    toVector x = VS.fromList [x]
    fromVector v = VS.head v

instance MatrixField r => ToFromVector (SVector (n::Symbol) r) where
    toVector (SVector_Dynamic fp off n) = VS.unsafeFromForeignPtr fp off n
    fromVector v = SVector_Dynamic fp off n
        where
            (fp,off,n) = VS.unsafeToForeignPtr v

instance (KnownNat n, MatrixField r) => ToFromVector (SVector (n::Nat) r) where
    toVector (SVector_Nat fp) = VS.unsafeFromForeignPtr fp 0 n
        where
            n = nat2int (Proxy::Proxy n)
    fromVector v = SVector_Nat fp
        where
            (fp,_,_) = VS.unsafeToForeignPtr v

apMat_ ::
    ( Scalar a~Scalar b
    , Scalar b ~ Scalar (Scalar b)
    , MatrixField (Scalar a)
    , ToFromVector a
    , ToFromVector b
    ) => HM.Matrix (Scalar a) -> a -> b
apMat_ m a = fromVector $ HM.flatten $ m HM.<> HM.asColumn (toVector a)

data a +> b where
    Zero ::
        ( Module a
        , Module b
        ) => a +> b

    Id_ ::
        ( Vector b
        ) => !(Scalar b) -> b +> b

    Mat_ ::
        ( MatrixField (Scalar b)
        , Scalar a~Scalar b
        , Scalar b~Scalar (Scalar b)
        , Vector a
        , Vector b
        , ToFromVector a
        , ToFromVector b
        , P.Num (HM.Vector (Scalar a))
        ) => {-#UNPACK#-}!(HM.Matrix (Scalar b)) -> a +> b

type instance Scalar (a +> b) = Scalar b
type instance Logic (a +> b) = Bool

-- type instance (a +> b) >< c = Tensor_Linear (a +> b) c
-- type family Tensor_Linear a b where
--     Tensor_Linear (a +> b) c = a +> b

mkMutable [t| forall a b. a +> b |]

--------------------------------------------------------------------------------
-- instances

deriving instance ( MatrixField (Scalar b), Show (Scalar b) ) => Show (a +> b)

----------------------------------------
-- category

instance Category (+>) where
    type ValidCategory (+>) a = MatrixField a

    id = Id_ 1

    Zero      . Zero      = Zero
    Zero      . (Id_  _ ) = Zero
    Zero      . (Mat_ _ ) = Zero

    (Id_  _ ) . Zero      = Zero
    (Id_  r1) . (Id_  r2) = Id_ (r1*r2)
    (Id_  r ) . (Mat_ m ) = Mat_ $ HM.scale r m

    (Mat_ _) . Zero      = Zero
    (Mat_ m ) . (Id_  r ) = Mat_ $ HM.scale r m
    (Mat_ m1) . (Mat_ m2) = Mat_ $ m1 HM.<> m2

instance Sup (+>) (->) (->)
instance Sup (->) (+>) (->)

instance (+>) <: (->) where
    embedType_ = Embed2 (embedType2 go)
        where
            go :: a +> b -> a -> b
            go Zero     = zero
            go (Id_  r) = (r*.)
            go (Mat_ m) = apMat_ m

instance Dagger (+>) where
    dagger Zero     = Zero
    dagger (Id_  r) = Id_ r
    dagger (Mat_ m) = Mat_ $ HM.tr' m

instance Groupoid (+>) where
    inverse Zero = undefined
    inverse (Id_  r) = Id_  $ reciprocal r
    inverse (Mat_ m) = Mat_ $ HM.inv m


-- FIXME
type instance Elem (a +> b) = b
type instance Index (a +> b) = Index a

instance Eq (a +> b)
instance IxContainer (a +> b)
instance Transposable (a +> a) where
    trans = dagger

----------------------------------------
-- size

-- FIXME: what's the norm of a tensor?
instance MatrixField r => Normed (SVector m r +> SVector n r) where
    size Zero = zero
    size (Id_ r) = r
    size (Mat_ m) = HM.det m

----------------------------------------
-- algebra

instance Semigroup (a +> b) where
    Zero      + a         = a
    a         + Zero      = a
    (Id_  r1) + (Id_  r2) = Id_ (r1+r2)
    (Id_  r ) + (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.+ m
    (Mat_ m ) + (Id_  r ) = Mat_ $ m P.+ HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) + (Mat_ m2) = Mat_ $ m1 P.+ m2

instance (Vector a, Vector b) => Monoid (a +> b) where
    zero = Zero

instance (Vector a, Vector b) => Cancellative (a +> b) where
    a         - Zero      = a
    Zero      - a         = negate a
    (Id_  r1) - (Id_  r2) = Id_ (r1-r2)
    (Id_  r ) - (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.- m
    (Mat_ m ) - (Id_  r ) = Mat_ $ m P.- HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) - (Mat_ m2) = Mat_ $ m1 P.- m2

instance (Vector a, Vector b) => Group (a +> b) where
    negate Zero     = Zero
    negate (Id_  r) = Id_ $ negate r
    negate (Mat_ m) = Mat_ $ HM.scale (-1) m

instance Abelian (a +> b)

-------------------
-- modules

instance (Vector a, Vector b) => Module (a +> b) where
    Zero     .* _  = Zero
    (Id_ r1) .* r2 = Id_ $ r1*r2
    (Mat_ m) .* r2 = Mat_ $ HM.scale r2 m

instance (Vector a, Vector b) => FreeModule (a +> b) where
    Zero      .*. _         = Zero
    _         .*. Zero      = Zero
    (Id_  r1) .*. (Id_  r2) = Id_ $ r1*r2
    (Id_  r ) .*. (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.* m
    (Mat_ m ) .*. (Id_  r ) = Mat_ $ m P.* HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) .*. (Mat_ m2) = Mat_ $ m1 P.* m2

instance (Vector a, Vector b) => Vector (a +> b) where
    Zero      ./. _         = Zero
    (Id_  _) ./. Zero = undefined
    (Mat_  _) ./. Zero = undefined
    (Id_  r1) ./. (Id_  r2) = Id_ $ r1/r2
    (Id_  r ) ./. (Mat_ m ) = Mat_ $ (HM.scale r (HM.ident (HM.rows m))) P./ m
    (Mat_ m ) ./. (Id_  r ) = Mat_ $ m P./ HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) ./. (Mat_ m2) = Mat_ $ m1 P./ m2

-------------------
-- rings
--
-- NOTE: matrices are only a ring when their dimensions are equal

instance Vector a => Rg (a +> a) where
    (*) = (>>>)

instance Vector a => Rig (a +> a) where
    one = Id_ one

instance Vector a => Ring (a +> a) where
    fromInteger i = Id_ $ fromInteger i

instance Vector a => Field (a +> a) where
    fromRational r = Id_ $ fromRational r

    reciprocal Zero = undefined
    reciprocal (Id_ r ) = Id_ $ reciprocal r
    reciprocal (Mat_ m) = Mat_ $ HM.inv m

