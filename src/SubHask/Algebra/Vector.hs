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
    , unsafeToModule

    -- * Debug
    , safeNewByteArray
    )
    where

import qualified Prelude as P

import Control.Monad.Primitive
import Control.Monad
import Data.Primitive hiding (sizeOf)
import Debug.Trace
import qualified Data.Primitive as Prim
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Test.QuickCheck.Gen (frequency)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
-- import qualified Data.Packed.Matrix as HM
import qualified Numeric.LinearAlgebra as HM
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified Numeric.LinearAlgebra.Data as HM

import SubHask.Algebra
import SubHask.Category
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.SubType

import Data.Csv (FromRecord,FromField,parseRecord)

import System.IO.Unsafe
import Unsafe.Coerce

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

type instance UVector m a >< b = Tensor_UVector (UVector m a) b
type family Tensor_UVector a b where
    Tensor_UVector (UVector n r1) (UVector m r2) = UVector n r1 +> UVector m r2
    Tensor_UVector (UVector n r1) r1 = UVector n r1 -- (r1><r2)

type ValidUVector n r = ( (UVector n r><Scalar r)~UVector n r, Prim r)

type instance Index (UVector n r) = Int
type instance Elem (UVector n r) = Scalar r
type instance SetElem (UVector n r) b = UVector n b

--------------------------------------------------------------------------------

data instance UVector (n::Symbol) r = UVector_Dynamic
    {-#UNPACK#-}!ByteArray
    {-#UNPACK#-}!Int -- offset
    {-#UNPACK#-}!Int -- length

instance (Show r, Monoid r, Prim r) => Show (UVector (n::Symbol) r) where
    show (UVector_Dynamic arr off n) = if isZero n
        then "zero"
        else show $ go (extendDimensions n-1) []
        where
            go (-1) xs = xs
            go i    xs = go (i-1) (x:xs)
                where
                    x = indexByteArray arr (off+i) :: r

instance (Arbitrary r, ValidUVector n r, FreeModule r, IsScalar r) => Arbitrary (UVector (n::Symbol) r) where
    arbitrary = frequency
        [ (1,return zero)
        , (9,fmap unsafeToModule $ replicateM 27 arbitrary)
        ]

instance (Show r, Monoid r, Prim r) => CoArbitrary (UVector (n::Symbol) r) where
    coarbitrary = coarbitraryShow

instance (NFData r, Prim r) => NFData (UVector (n::Symbol) r) where
    rnf (UVector_Dynamic arr off n) = seq arr ()

instance (FromField r, ValidUVector n r, IsScalar r, FreeModule r) => FromRecord (UVector (n::Symbol) r) where
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
                ref <- newPrimRef $ UVector_Dynamic arr1 off1 n
                return $ Mutable_UVector ref
            else unsafePrimToPrim $ do
                marr2 <- safeNewByteArray b 16
                copyByteArray marr2 0 arr1 off1 b
                arr2 <- unsafeFreezeByteArray marr2
                ref2 <- newPrimRef (UVector_Dynamic arr2 0 n)
                return $ Mutable_UVector ref2

    write (Mutable_UVector ref) (UVector_Dynamic arr2 off2 n2) = do
        (UVector_Dynamic arr1 off1 n1) <- readPrimRef ref
        unsafePrimToPrim $ if
            -- both ptrs null: do nothing
            | n1==0 && n2==0 -> return ()

            -- only arr1 null: allocate memory then copy arr2 over
            | n1==0 -> do
                marr1' <- safeNewByteArray b 16
                copyByteArray marr1' 0 arr2 off2 b
                arr1' <- unsafeFreezeByteArray marr1'
                unsafePrimToPrim $ writePrimRef ref (UVector_Dynamic arr1' 0 n2)

            -- only arr2 null: make arr1 null
            | n2==0 -> do
                writePrimRef ref (UVector_Dynamic arr2 0 n1)

            -- both ptrs valid: perform a normal copy
            | otherwise -> do
                marr1 <- unsafeThawByteArray arr1
                copyByteArray marr1 off1 arr2 off2 b

        where b = (extendDimensions n2)*Prim.sizeOf (undefined::r)

----------------------------------------
-- algebra

extendDimensions :: Int -> Int
extendDimensions = roundUpToNearest 4 -- i+4-i`rem`4

-- extendDimensions :: Int -> Int
-- extendDimensions x = x+r
--     where
--         m = 4
--         s = x`rem`m
--         r = if s==0 then 0 else m-s

safeNewByteArray :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
safeNewByteArray b 16 = do
    let n=extendDimensions $ b`quot`4
    marr <- newAlignedPinnedByteArray b 16
--     writeByteArray marr (n-0) (0::Float)
--     writeByteArray marr (n-1) (0::Float)
--     writeByteArray marr (n-2) (0::Float)
--     writeByteArray marr (n-3) (0::Float)
    setByteArray marr 0 n (0::Float)

--     trace ("n="++show n) $ return ()
--     a <- forM [0..n-1] $ \i -> do
--         v :: Float <- readByteArray marr i
--         return $ unsafeInlineIO $ P.putStrLn $ "marr!"+show i+" = "+show v
--     deepseq a $ return marr

    return marr

{-# INLINE binopDynUV #-}
binopDynUV :: forall a b n m.
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
            let v1 = indexByteArray arr1 (off1+i)
                v2 = indexByteArray arr2 (off2+i)
            writeByteArray marr3 i (f v1 v2)
            go marr3 (i-1)

{-# INLINE monopDynUV #-}
monopDynUV :: forall a b n m.
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

{-
{-# INLINE binopDynUVM #-}
binopDynUVM :: forall a b n m.
    ( PrimBase m
    , Prim a
    , Prim b
    , Monoid a
    , Monoid b
    ) => (a -> b -> a) -> Mutable m (UVector (n::Symbol) a) -> UVector n b -> m ()
binopDynUVM f (Mutable_UVector ref) (UVector_Dynamic arr2 off2 n2) = do
    (UVector_Dynamic arr1 off1 n1) <- readPrimRef ref

    let runop arr1 arr2 n = unsafePrimToPrim $
            withForeignPtr arr1 $ \p1 ->
            withForeignPtr arr2 $ \p2 ->
                go (plusPtr p1 off1) (plusPtr p2 off2) (n-1)

    unsafePrimToPrim $ if
        -- both vectors are zero: do nothing
        | isNull arr1 && isNull arr2 -> return ()

        -- only left vector is zero: allocate space and overwrite old vector
        -- FIXME: this algorithm requires two passes over the left vector
        | isNull arr1 -> do
            arr1' <- zerofp n2
            unsafePrimToPrim $ writePrimRef ref (UVector_Dynamic arr1' 0 n2)
            runop arr1' arr2 n2

        -- only right vector is zero: use a temporary zero vector to run like normal
        -- FIXME: this algorithm requires an unneeded memory allocation and memory pass
        | isNull arr2 -> do
            arr2' <- zerofp n1
            runop arr1 arr2' n1

        -- both vectors nonzero: run like normal
        | otherwise -> runop arr1 arr2 n1

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
    , Prim a
    ) => (a -> a) -> Mutable m (UVector (n::Symbol) a) -> m ()
monopDynM f (Mutable_UVector ref) = do
    (UVector_Dynamic arr1 off1 n) <- readPrimRef ref
    if isNull arr1
        then return ()
        else unsafePrimToPrim $
            withForeignPtr arr1 $ \p1 ->
                go (plusPtr p1 off1) (n-1)

    where
        go _ (-1) = return ()
        go p1 i = do
            v1 <- peekElemOff p1 i
            pokeElemOff p1 i (f v1)
            go p1 (i-1)

-------------------

-}
instance (Monoid r, Prim r) => Semigroup (UVector (n::Symbol) r) where
    {-# INLINE (+)  #-} ; (+)  = binopDynUV  (+)
--     {-# INLINE (+=) #-} ; (+=) = binopDynUVM (+)

instance (Monoid r, Cancellative r, Prim r) => Cancellative (UVector (n::Symbol) r) where
    {-# INLINE (-)  #-} ; (-)  = binopDynUV  (-)
--     {-# INLINE (-=) #-} ; (-=) = binopDynUVM (-)

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
--     {-# INLINE (.*=)  #-} ;  (.*=) v r = monopDynM (.*r) v

type instance Actor (UVector n r) = Actor r

instance (Action r, Semigroup r, Prim r) => Action (UVector (n::Symbol) r) where
  {-# INLINE (.+)   #-}
  (.+) v r = monopDynUV (.+r) v

instance (FreeModule r, ValidUVector n r) => FreeModule (UVector (n::Symbol) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopDynUV  (.*.)
--     {-# INLINE (.*.=) #-} ;  (.*.=)    = binopDynUVM (.*.)

instance (VectorSpace r, ValidUVector n r) => VectorSpace (UVector (n::Symbol) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopDynUV  (./r) v
--     {-# INLINE (./=)  #-} ;  (./=) v r = monopDynM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopDynUV  (./.)
--     {-# INLINE (./.=) #-} ;  (./.=)    = binopDynUVM (./.)

----------------------------------------
-- container

instance (Monoid r, ValidLogic r, Prim r, IsScalar r) => IxContainer (UVector (n::Symbol) r) where

    {-# INLINE (!) #-}
    (!) (UVector_Dynamic arr off n) i = indexByteArray arr (off+i)

    {-# INLINABLE toIxList #-}
    toIxList (UVector_Dynamic arr off n) = P.zip [0..] $ go (n-1) []
        where
            go (-1) xs = xs
            go i xs = go (i-1) (indexByteArray arr (off+i) : xs)

--     imap f v = unsafeToModule $ imap f $ values v


instance (FreeModule r, ValidUVector n r, ValidLogic r, IsScalar r) => FiniteModule (UVector (n::Symbol) r) where

    {-# INLINE dim #-}
    dim (UVector_Dynamic _ _ n) = n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        marr <- safeNewByteArray (n*Prim.sizeOf (undefined::r)) 16
        go marr (P.reverse xs) (n-1)
        arr <- unsafeFreezeByteArray marr
        return $ UVector_Dynamic arr 0 n

        where
            n = length xs

            go marr []  (-1) = return ()
            go marr (x:xs) i = do
                writeByteArray marr i x
                go marr xs (i-1)

----------------------------------------
-- comparison

isConst :: (Prim r, Eq_ r, ValidLogic r) => UVector (n::Symbol) r -> r -> Logic r
isConst (UVector_Dynamic arr1 off1 n1) c = go (off1+n1-1)
    where
        go (-1) = true
        go i = indexByteArray arr1 i==c && go (i-1)

instance (Eq r, Monoid r, Prim r) => Eq_ (UVector (n::Symbol) r) where
    {-# INLINE (==) #-}
    v1@(UVector_Dynamic arr1 off1 n1)==v2@(UVector_Dynamic arr2 off2 n2) = if
        | isZero n1 && isZero n2 -> true
        | isZero n1 -> isConst v2 zero
        | isZero n2 -> isConst v1 zero
        | otherwise -> go (n1-1)
        where
            go (-1) = true
            go i = v1==v2 && go (i-1)
                where
                    v1 = indexByteArray arr1 (off1+i) :: r
                    v2 = indexByteArray arr2 (off2+i) :: r

{-


{-# INLINE innerp #-}
-- innerp :: UVector 200 Float -> UVector 200 Float -> Float
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
-}

----------------------------------------
-- distances

instance
    ( Prim r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (UVector (n::Symbol) r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(UVector_Dynamic arr1 off1 n1) v2@(UVector_Dynamic arr2 off2 n2)
      = {-# SCC distance_UVector #-} if
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
    distanceUB v1@(UVector_Dynamic arr1 off1 n1) v2@(UVector_Dynamic arr2 off2 n2) ub
      = {-# SCC distanceUB_UVector #-} if
        | isZero n1 -> size v2
        | isZero n2 -> size v1
        | otherwise -> sqrt $ go 0 (n1-1)
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

            goEach !tot !i = if i<0
                then tot
                else goEach (tot + (v1!i-v2!i).*.(v1!i-v2!i)) (i-1)

instance (VectorSpace r, Prim r, IsScalar r, ExpField r) => Normed (UVector (n::Symbol) r) where
    {-# INLINE size #-}
    size v@(UVector_Dynamic arr off n) = if isZero n
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
    ( VectorSpace r
    , ValidUVector n r
    , IsScalar r
    , ExpField r
    , Real r
    ) => Banach (UVector (n::Symbol) r)

instance
    ( VectorSpace r
    , ValidUVector n r
    , IsScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Hilbert (UVector (n::Symbol) r)
        where

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
    toVector (UVector_Dynamic fp off n) = undefined
    fromVector v = UVector_Dynamic fp off n
        where
            (fp,off,n) = undefined -- VS.unsafeToForeignPtr v

instance MatrixField r => Normed (UVector m r +> UVector n r) where
    size (Id_ r) = r
    size (Mat_ m) = HM.det m

-- | A slightly more convenient type for linear functions between "UVector"s
type UMatrix r m n = UVector m r +> UVector n r

-- | Construct an "UMatrix"
unsafeMkUMatrix ::
    ( VectorSpace (UVector m r)
    , VectorSpace (UVector n r)
    , ToFromVector (UVector m r)
    , ToFromVector (UVector n r)
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Int -> Int -> [r] -> UMatrix r m n
unsafeMkUMatrix m n rs = Mat_ $ (m HM.>< n) rs

instance
    ( FiniteModule (UVector n r)
    , VectorSpace (UVector n r)
    , MatrixField r
    , ToFromVector (UVector n r)
    , P.Num (HM.Vector r)
    ) => TensorAlgebra (UVector n r)
        where
    v1><v2 = unsafeMkUMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

    mXv m v = m $ v
    vXm v m = trans m $ v

--------------------------------------------------------------------------------
-- helper functions for memory management

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

-- | The type of dynamic or statically sized vectors implemented using the FFI.
data family SVector (n::k) r

type instance Scalar (SVector n r) = Scalar r
type instance Logic (SVector n r) = Logic r

-- type instance SVector m a >< b = VectorOuterProduct (SVector m a) b
-- type family VectorOuterProduct a b where
-- --     VectorOuterProduct (SVector m a) (SVector n a) = SVector m a
-- --     VectorOuterProduct (SVector m a) (SVector n a) = Matrix a m n
--     VectorOuterProduct (SVector m a) a = SVector m a -- (a><b)

-- type instance SVector n r >< a = SVector n (r><a)

type instance SVector m a >< b = Tensor_SVector (SVector m a) b
type family Tensor_SVector a b where
    Tensor_SVector (SVector n r1) (SVector m r2) = SVector n r1 +> SVector m r2
    Tensor_SVector (SVector n r1) r1 = SVector n r1 -- (r1><r2)

type ValidSVector n r = ( (SVector n r><Scalar r)~SVector n r, Storable r)

type instance Index (SVector n r) = Int
type instance Elem (SVector n r) = Scalar r
type instance SetElem (SVector n r) b = SVector n b

--------------------------------------------------------------------------------

data instance SVector (n::Symbol) r = SVector_Dynamic
    {-#UNPACK#-}!(ForeignPtr r)
    {-#UNPACK#-}!Int -- offset
    {-#UNPACK#-}!Int -- length

instance (Show r, Monoid r, ValidSVector n r) => Show (SVector (n::Symbol) r) where
    show (SVector_Dynamic fp off n) = if isNull fp
        then "zero"
        else show $ unsafeInlineIO $ go (n-1) []
        where
            go (-1) xs = return $ xs
            go i    xs = withForeignPtr fp $ \p -> do
                x <- peekElemOff p (off+i)
                go (i-1) (x:xs)

instance (Arbitrary r, ValidSVector n r, FreeModule r, IsScalar r) => Arbitrary (SVector (n::Symbol) r) where
    arbitrary = frequency
        [ (1,return zero)
        , (9,fmap unsafeToModule $ replicateM 27 arbitrary)
        ]

instance (NFData r, ValidSVector n r) => NFData (SVector (n::Symbol) r) where
    rnf (SVector_Dynamic fp off n) = seq fp ()

instance (FromField r, ValidSVector n r, IsScalar r, FreeModule r) => FromRecord (SVector (n::Symbol) r) where
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

    write (Mutable_SVector ref) (SVector_Dynamic fp2 off2 n2) = do
        (SVector_Dynamic fp1 off1 n1) <- readPrimRef ref
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
binopDyn :: forall a b n m.
    ( Storable a
    , Monoid a
    ) => (a -> a -> a) -> SVector (n::Symbol) a -> SVector (n::Symbol) a -> SVector (n::Symbol) a
binopDyn f v1@(SVector_Dynamic fp1 off1 n1) v2@(SVector_Dynamic fp2 off2 n2) = if
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
            v1 <- peekElemOff p1 i
            v2 <- peekElemOff p2 i
            pokeElemOff p3 i (f v1 v2)
            go p1 p2 p3 (i-1)

{-# INLINE monopDyn #-}
monopDyn :: forall a b n m.
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
monopDynM :: forall a b n m.
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

instance (Module r, ValidSVector n r, IsScalar r) => Module (SVector (n::Symbol) r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopDyn  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopDynM (.*r) v

instance (FreeModule r, ValidSVector n r, IsScalar r) => FreeModule (SVector (n::Symbol) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopDyn  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopDynM (.*.)

instance (VectorSpace r, ValidSVector n r, IsScalar r) => VectorSpace (SVector (n::Symbol) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopDyn  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopDynM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopDyn  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopDynM (./.)

----------------------------------------
-- container

instance
    ( Monoid r
    , ValidLogic r
    , ValidSVector n r
    , IsScalar r
    , FreeModule r
    ) => IxContainer (SVector (n::Symbol) r)
        where

    {-# INLINE (!) #-}
    (!) (SVector_Dynamic fp off n) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p (off+i)

    {-# INLINABLE toIxList #-}
    toIxList v = P.zip [0..] $ go (dim v-1) []
        where
            go (-1) xs = xs
            go    i xs = go (i-1) (v!i : xs)

    {-# INLINABLE imap #-}
    imap f v = unsafeToModule $ imap f $ values v

    type ValidElem (SVector n r) e = (ClassicalLogic e, IsScalar e, FiniteModule e, ValidSVector n e)

instance (FreeModule r, ValidLogic r, ValidSVector n r, IsScalar r) => FiniteModule (SVector (n::Symbol) r) where

    {-# INLINE dim #-}
    dim (SVector_Dynamic _ _ n) = n

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = unsafeInlineIO $ do
        fp <- mallocForeignPtrArray n
        withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
        return $ SVector_Dynamic fp 0 n

        where
            n = length xs

            go p []  (-1) = return ()
            go p (x:xs) i = do
                pokeElemOff p i x
                go p xs (i-1)

----------------------------------------
-- comparison

instance (Eq r, Monoid r, ValidSVector n r) => Eq_ (SVector (n::Symbol) r) where
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

----------------------------------------
-- distances

instance
    ( ValidSVector n r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (SVector (n::Symbol) r)
        where

    {-# INLINE[2] distance #-}
    distance v1@(SVector_Dynamic fp1 _ n) v2@(SVector_Dynamic fp2 _ _) = {-# SCC distance_SVector #-} if
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
    distanceUB v1@(SVector_Dynamic fp1 _ n) v2@(SVector_Dynamic fp2 _ _) ub = {-# SCC distanceUB_SVector #-}if
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

instance (VectorSpace r, ValidSVector n r, IsScalar r, ExpField r) => Normed (SVector (n::Symbol) r) where
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
    ( VectorSpace r
    , ValidSVector n r
    , IsScalar r
    , ExpField r
    , Real r
    ) => Banach (SVector (n::Symbol) r)

instance
    ( VectorSpace r
    , ValidSVector n r
    , IsScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Hilbert (SVector (n::Symbol) r)
        where

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
    , IsScalar r
    ) => Arbitrary (SVector (n::Nat) r)
        where
    arbitrary = do
        xs <- replicateM n arbitrary
        return $ unsafeToModule xs
        where
            n = nat2int (Proxy::Proxy n)

instance (NFData r, ValidSVector n r) => NFData (SVector (n::Nat) r) where
    rnf (SVector_Nat fp) = seq fp ()

static2dynamic :: forall n m r. KnownNat n => SVector (n::Nat) r -> SVector (m::Symbol) r
static2dynamic (SVector_Nat fp) = SVector_Dynamic fp 0 $ nat2int (Proxy::Proxy n)

--------------------

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
binopStatic :: forall a b n m.
    ( Storable a
    , KnownNat n
    ) => (a -> a -> a) -> SVector n a -> SVector n a -> SVector n a
binopStatic f v1@(SVector_Nat fp1) v2@(SVector_Nat fp2) = unsafeInlineIO $ do
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
    ) => (a -> a) -> SVector n a -> SVector n a
monopStatic f v@(SVector_Nat fp1) = unsafeInlineIO $ do
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
monopStaticM :: forall a b n m.
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

-------------------

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

instance (KnownNat n, Module r, ValidSVector n r, IsScalar r) => Module (SVector (n::Nat) r) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopStatic  (.*r) v
    {-# INLINE (.*=)  #-} ;  (.*=) v r = monopStaticM (.*r) v

instance (KnownNat n, FreeModule r, ValidSVector n r, IsScalar r) => FreeModule (SVector (n::Nat) r) where
    {-# INLINE (.*.)  #-} ;  (.*.)     = binopStatic  (.*.)
    {-# INLINE (.*.=) #-} ;  (.*.=)    = binopStaticM (.*.)

instance (KnownNat n, VectorSpace r, ValidSVector n r, IsScalar r) => VectorSpace (SVector (n::Nat) r) where
    {-# INLINE (./)   #-} ;  (./)  v r = monopStatic  (./r) v
    {-# INLINE (./=)  #-} ;  (./=) v r = monopStaticM (./r) v

    {-# INLINE (./.)  #-} ;  (./.)     = binopStatic  (./.)
    {-# INLINE (./.=) #-} ;  (./.=)    = binopStaticM (./.)

----------------------------------------
-- "container"

instance
    ( KnownNat n
    , Monoid r
    , ValidLogic r
    , ValidSVector n r
    , IsScalar r
    , FreeModule r
    ) => IxContainer (SVector (n::Nat) r)
        where

    {-# INLINE (!) #-}
    (!) (SVector_Nat fp) i = unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i

    {-# INLINABLE toIxList #-}
    toIxList v = P.zip [0..] $ go (dim v-1) []
        where
            go (-1) xs = xs
            go    i xs = go (i-1) (v!i : xs)

    {-# INLINABLE imap #-}
    imap f v = unsafeToModule $ imap f $ values v

    type ValidElem (SVector n r) e = (ClassicalLogic e, IsScalar e, FiniteModule e, ValidSVector n e)

instance
    ( KnownNat n
    , FreeModule r
    , ValidLogic r
    , ValidSVector n r
    , IsScalar r
    ) => FiniteModule (SVector (n::Nat) r)
        where

    {-# INLINE dim #-}
    dim v = nat2int (Proxy::Proxy n)

    {-# INLINABLE unsafeToModule #-}
    unsafeToModule xs = if n /= length xs
        then error "unsafeToModule size mismatch"
        else unsafeInlineIO $ do
            fp <- mallocForeignPtrArray n
            withForeignPtr fp $ \p -> go p (P.reverse xs) (n-1)
            return $ SVector_Nat fp

        where
            n = nat2int (Proxy::Proxy n)

            go p []  (-1) = return ()
            go p (x:xs) i = do
                pokeElemOff p i x
                go p xs (i-1)


----------------------------------------
-- comparison

instance (KnownNat n, Eq_ r, ValidLogic r, ValidSVector n r) => Eq_ (SVector (n::Nat) r) where
    {-# INLINE (==) #-}
    (SVector_Nat fp1)==(SVector_Nat fp2) = unsafeInlineIO $
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

----------------------------------------
-- distances

instance
    ( KnownNat n
    , ValidSVector n r
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~Bool
    , IsScalar r
    , VectorSpace r
    , ValidSVector "dyn" r
    ) => Metric (SVector (n::Nat) r)
        where

    -- For some reason, using the dynamic vector is a little faster than a straight implementation
    {-# INLINE[2] distance #-}
    distance v1 v2 = distance (static2dynamic v1) (static2dynamic v2 :: SVector "dyn" r)
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
    distanceUB v1 v2 ub = {-# SCC distanceUB_SVector #-} sqrt $ go 0 (n-1)
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
    , ValidSVector n r
    , IsScalar r
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
    , VectorSpace r
    , ValidSVector n r
    , IsScalar r
    , ExpField r
    , Real r
    , ValidSVector n r
    , ValidSVector "dyn" r
    ) => Banach (SVector (n::Nat) r)

instance
    ( KnownNat n
    , VectorSpace r
    , ValidSVector n r
    , IsScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , ValidSVector n r
    , ValidSVector "dyn" r
    , P.Num (HM.Vector r)
    ) => Hilbert (SVector (n::Nat) r)
        where

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



type MatrixField r =
    ( IsScalar r
    , VectorSpace r
    , Field r
    , HM.Field r
    , HM.Container HM.Vector r
    , HM.Product r
    )

{-
data Matrix r (m::k1) (n::k2) where
    Zero  ::                                Matrix r m n
    Id    :: {-#UNPACK#-}!r              -> Matrix r m m
    Diag  :: {-#UNPACK#-}!(SVector m r)  -> Matrix r m m
    Mat   :: {-#UNPACK#-}!(HM.Matrix r)  -> Matrix r m n

type instance Scalar (Matrix r m n) = Scalar r
type instance (Matrix r m n)><r = Matrix r m n

mkMutable [t| forall a b c. Matrix a b c |]

mkMatrix :: MatrixField r => Int -> Int -> [r] -> Matrix r m n
mkMatrix m n rs = Mat $ (m HM.>< n) rs

--------------------------------------------------------------------------------
-- class instances

deriving instance
    ( MatrixField r
    , Show (SVector n r)
    , Show r
    ) => Show (Matrix r m n)

----------------------------------------
-- misc

instance (Storable r, NFData r) => NFData (Matrix r m n) where
    rnf (Id  r) = ()
    rnf (Mat m) = rnf m

----------------------------------------
-- category

instance MatrixField r => Category (Matrix r) where
    type ValidCategory (Matrix r) a = ()

    id = Id 1

    (Id  r1).(Id  r2) = Id (r1*r2)
    (Id  r ).(Mat m ) = Mat $ HM.scale r m
    (Mat m ).(Id  r ) = Mat $ HM.scale r m
    (Mat m1).(Mat m2) = Mat $ m2 HM.<> m1

instance MatrixField r => Matrix r (m::Symbol) (n::Symbol) <: (SVector m r -> SVector n r) where
    embedType_ = Embed0 $ embedType go
        where
            go :: Matrix r m n -> SVector m r -> SVector n r
            go (Id  r) (SVector_Dynamic fp off n) = (SVector_Dynamic fp off n).*r
            go (Mat m) (SVector_Dynamic fp off n) = SVector_Dynamic fp' off' n'
                where
                    (fp',off',n') = VS.unsafeToForeignPtr $ m HM.<> VS.unsafeFromForeignPtr fp off n

type family ToHask (cat :: ka -> kb -> *) (a :: ka) (b :: kb) :: * where
    ToHask (Matrix r) a b = SVector r a -> SVector r b

infixr 0 $$$
-- ($$$) :: (Matrix r a b <: (SVector a r -> SVector b r)) => Matrix r a b -> SVector a r -> SVector b r
($$$) :: (Matrix r a b <: ToHask (Matrix r) a b) => Matrix r a b -> ToHask (Matrix r) a b
($$$) = embedType

instance MatrixField r => Dagger (Matrix r) where
    dagger (Id  r) = Id r
    dagger (Mat m) = Mat $ HM.trans m

----------------------------------------
-- size

instance MatrixField r => Normed (Matrix r m n) where
    size (Id r) = r
    size (Mat m) = HM.det m

----------------------------------------
-- algebra

instance MatrixField r => Semigroup (Matrix r m n) where
    (Id  r1)+(Id  r2) = Id (r1+r2)
    (Id  r )+(Mat m ) = Mat $ HM.scale r (HM.ident (HM.rows m)) `HM.add` m
    (Mat m )+(Id  r ) = Mat $ m `HM.add` HM.scale r (HM.ident (HM.rows m))
    (Mat m1)+(Mat m2) = Mat $ m1 `HM.add` m2

instance MatrixField r => Monoid (Matrix r m n) where
    zero = Zero

instance MatrixField r => Cancellative (Matrix r m n) where
    (Id  r1)-(Id  r2) = Id (r1-r2)
    (Id  r )-(Mat m ) = Mat $ HM.scale r (HM.ident (HM.rows m)) `HM.sub` m
    (Mat m )-(Id  r ) = Mat $ m `HM.sub` HM.scale r (HM.ident (HM.rows m))
    (Mat m1)-(Mat m2) = Mat $ m1 `HM.sub` m2

instance MatrixField r => Group (Matrix r m n) where
    negate (Id r) = Id $ negate r
    negate (Mat m) = Mat $ HM.scale (-1) m

instance MatrixField r => Abelian (Matrix r m n)

-------------------
-- modules

instance MatrixField r => Module (Matrix r m n) where
    (Id r1) .* r2 = Id $ r1*r2
    (Mat m) .* r2 = Mat $ HM.scale r2 m

instance MatrixField r => FreeModule (Matrix r m n) where
    (Id  r1) .*. (Id  r2) = Id $ r1*r2
    (Id  r ) .*. (Mat m ) = Mat $ HM.scale r (HM.ident (HM.rows m)) `HM.mul` m
    (Mat m ) .*. (Id  r ) = Mat $ m `HM.mul` HM.scale r (HM.ident (HM.rows m))
    (Mat m1) .*. (Mat m2) = Mat $ m1 `HM.mul` m2

instance MatrixField r => VectorSpace (Matrix r m n) where
    (Id  r1) ./. (Id  r2) = Id $ r1/r2
    (Id  r ) ./. (Mat m ) = Mat $ HM.scale r (HM.ident (HM.rows m)) `HM.divide` m
    (Mat m ) ./. (Id  r ) = Mat $ m `HM.divide` HM.scale r (HM.ident (HM.rows m))
    (Mat m1) ./. (Mat m2) = Mat $ m1 `HM.divide` m2

-------------------
-- rings
--
-- NOTE: matrices are only a ring when their dimensions are equal

instance MatrixField r => Rg (Matrix r m m) where
    (*) = (>>>)

instance MatrixField r => Rig (Matrix r m m) where
    one = id

instance MatrixField r => Ring (Matrix r m m) where
    fromInteger i = Id $ fromInteger i

instance MatrixField r => Field (Matrix r m m) where
    fromRational r = Id $ fromRational r

    reciprocal (Id r ) = Id $ reciprocal r
    reciprocal (Mat m) = Mat $ HM.inv m

----------------------------------------

instance
    ( FiniteModule (SVector n r)
    , VectorSpace (SVector n r)
    , MatrixField r
    ) => TensorAlgebra (SVector n r)
        where
    v1><v2 = mkMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

-}
--------------------------------------------------------------------------------

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
            (fp,off,n) = VS.unsafeToForeignPtr v

---------

apMat_ ::
    ( Scalar a~Scalar b
    , Scalar b ~ Scalar (Scalar b)
    , MatrixField (Scalar a)
    , ToFromVector a
    , ToFromVector b
    ) => HM.Matrix (Scalar a) -> a -> b
apMat_ m a = fromVector $ HM.flatten $ m HM.<> HM.asColumn (toVector a)

---------------------------------------

data a +> b where
    Zero ::
        ( Module a
        , Module b
        ) => a +> b

    Id_ ::
        ( VectorSpace b
        ) => {-#UNPACK#-}!(Scalar b) -> b +> b

    Mat_ ::
        ( MatrixField (Scalar b)
        , Scalar a~Scalar b
        , Scalar b~Scalar (Scalar b)
        , VectorSpace a
        , VectorSpace b
        , ToFromVector a
        , ToFromVector b
        , P.Num (HM.Vector (Scalar a))
        ) => {-#UNPACK#-}!(HM.Matrix (Scalar b)) -> a +> b

type instance Scalar (a +> b) = Scalar b
type instance Logic (a +> b) = Bool

type instance (a +> b) >< c = Tensor_Linear (a +> b) c
type family Tensor_Linear a b where
--     Tensor_SVector (SVector n r1) (SVector m r2) = SVector n r1 +> SVector m r2
--     Tensor_Linear (a +> b) (c +> d) = (a +> b) +> (c +> d)
    Tensor_Linear (a +> b) c = a +> b

mkMutable [t| forall a b. a +> b |]

-- | A slightly more convenient type for linear functions between "SVector"s
type SMatrix r m n = SVector m r +> SVector n r

-- | Construct an "SMatrix"
unsafeMkSMatrix ::
    ( VectorSpace (SVector m r)
    , VectorSpace (SVector n r)
    , ToFromVector (SVector m r)
    , ToFromVector (SVector n r)
    , MatrixField r
    , P.Num (HM.Vector r)
    ) => Int -> Int -> [r] -> SMatrix r m n
unsafeMkSMatrix m n rs = Mat_ $ (m HM.>< n) rs

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

    (Id_  r ) . Zero      = Zero
    (Id_  r1) . (Id_  r2) = Id_ (r1*r2)
    (Id_  r ) . (Mat_ m ) = Mat_ $ HM.scale r m

    (Mat_ m1) . Zero      = Zero
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
    trans Zero     = Zero
    trans (Id_  r) = Id_ r
    trans (Mat_ m) = Mat_ $ HM.tr' m

instance Groupoid (+>) where
    inverse (Id_  r) = Id_  $ reciprocal r
    inverse (Mat_ m) = Mat_ $ HM.inv m

----------------------------------------
-- size

-- FIXME: what's the norm of a tensor?
instance MatrixField r => Normed (SVector m r +> SVector n r) where
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

instance (VectorSpace a, VectorSpace b) => Monoid (a +> b) where
    zero = Zero

instance (VectorSpace a, VectorSpace b) => Cancellative (a +> b) where
    a         - Zero      = a
    Zero      - a         = negate a
    (Id_  r1) - (Id_  r2) = Id_ (r1-r2)
    (Id_  r ) - (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.- m
    (Mat_ m ) - (Id_  r ) = Mat_ $ m P.- HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) - (Mat_ m2) = Mat_ $ m1 P.- m2

instance (VectorSpace a, VectorSpace b) => Group (a +> b) where
    negate Zero     = Zero
    negate (Id_  r) = Id_ $ negate r
    negate (Mat_ m) = Mat_ $ HM.scale (-1) m

instance Abelian (a +> b)

-------------------
-- modules

instance (VectorSpace a, VectorSpace b) => Module (a +> b) where
    Zero     .* _  = Zero
    (Id_ r1) .* r2 = Id_ $ r1*r2
    (Mat_ m) .* r2 = Mat_ $ HM.scale r2 m

instance (VectorSpace a, VectorSpace b) => FreeModule (a +> b) where
    Zero      .*. _         = Zero
    _         .*. Zero      = Zero
    (Id_  r1) .*. (Id_  r2) = Id_ $ r1*r2
    (Id_  r ) .*. (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.* m
    (Mat_ m ) .*. (Id_  r ) = Mat_ $ m P.* HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) .*. (Mat_ m2) = Mat_ $ m1 P.* m2

instance (VectorSpace a, VectorSpace b) => VectorSpace (a +> b) where
    Zero      ./. _         = Zero
    (Id_  r1) ./. (Id_  r2) = Id_ $ r1/r2
    (Id_  r ) ./. (Mat_ m ) = Mat_ $ (HM.scale r (HM.ident (HM.rows m))) P./ m
    (Mat_ m ) ./. (Id_  r ) = Mat_ $ m P./ HM.scale r (HM.ident (HM.rows m))
    (Mat_ m1) ./. (Mat_ m2) = Mat_ $ m1 P./ m2

-------------------
-- rings
--
-- NOTE: matrices are only a ring when their dimensions are equal

instance VectorSpace a => Rg (a +> a) where
    (*) = (>>>)

instance VectorSpace a => Rig (a +> a) where
    one = Id_ one

instance VectorSpace a => Ring (a +> a) where
    fromInteger i = Id_ $ fromInteger i

instance VectorSpace a => Field (a +> a) where
    fromRational r = Id_ $ fromRational r

    reciprocal (Id_ r ) = Id_ $ reciprocal r
    reciprocal (Mat_ m) = Mat_ $ HM.inv m

instance
    ( FiniteModule (SVector n r)
    , VectorSpace (SVector n r)
    , MatrixField r
    , ToFromVector (SVector n r)
    , P.Num (HM.Vector r)
    ) => TensorAlgebra (SVector n r)
        where
    v1><v2 = unsafeMkSMatrix (dim v1) (dim v2) [ v1!i * v2!j | i <- [0..dim v1-1], j <- [0..dim v2-1] ]

    mXv m v = m $ v
    vXm v m = trans m $ v
