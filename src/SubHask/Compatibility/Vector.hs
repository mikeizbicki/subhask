{-# LANGUAGE BangPatterns #-}

-- |
--
-- FIXME: Lots of operations are currently very slow:
--
-- 1. In order to generically support "zero" vectors, every add/mul/etc must do lots of extra checks.
--
-- 2. RULES support is very poor
--
-- 3. We'll need mutable algebraic operations
--
module SubHask.Compatibility.Vector
    (
    -- * Vectors
    VS.Vector
    , BoxedVector
    , UnboxedVector

    , listToVector

    -- * Arrays
    , Array
    , UnboxedArray
    , StorableArray
    , ArrayT (..)
    , ArrayTM (..)

    -- * RULES
    , eqVectorFloat
    , eqVectorDouble
    , eqVectorInt
    , eqUnboxedVectorFloat
    , eqUnboxedVectorDouble
    , eqUnboxedVectorInt
    )
    where

import Foreign.Storable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as Stream
import Test.QuickCheck (frequency)

import Data.Vector.Fusion.Stream (Step(..))
import qualified Data.Vector.Fusion.Stream as VFS
import Data.Vector.Fusion.Stream.Monadic (Stream(..), SPEC(..))
import Data.Vector.Fusion.Stream.Size (Size(..))
import Data.Vector.Fusion.Util (Id(..))

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST

import qualified Prelude as P
import SubHask.Internal.Prelude
import SubHask.Algebra
import SubHask.Algebra.Container
import SubHask.Algebra.Ord
import SubHask.Algebra.Parallel
import SubHask.Category
import SubHask.Compatibility.Base


-- | This function is copied and paster from the original library;
-- this is sufficient to make it use our updated notion of equality and logic.
eq :: Eq a => Stream Id a -> Stream Id a -> Bool
{-# INLINE eq #-}
-- {-# INLINE_STREAM eq #-}
eq (Stream step1 s1 _) (Stream step2 s2 _) = eq_loop0 SPEC s1 s2
  where
    eq_loop0 !sPEC s1 s2 = case unId (step1 s1) of
                             Yield x s1' -> eq_loop1 SPEC x s1' s2
                             Skip    s1' -> eq_loop0 SPEC   s1' s2
                             Done        -> VFS.null (Stream step2 s2 Unknown)

    eq_loop1 !sPEC x s1 s2 = case unId (step2 s2) of
                               Yield y s2' -> x == y && eq_loop0 SPEC   s1 s2'
                               Skip    s2' ->           eq_loop1 SPEC x s1 s2'
                               Done        -> False


-- FIXME:
-- For some reason, the comparison function above is slower than the standard one.
-- So we need these rewrite opts for speed.
-- Is there a more generic way to write these?
-- {-# RULES
--
-- "subhask/eqVectorDouble"  (==) = eqVectorDouble
-- "subhask/eqVectorFloat"  (==) = eqVectorFloat
-- "subhask/eqVectorInt"  (==) = eqVectorInt
--
--   #-}

eqVectorFloat :: VS.Vector Float -> VS.Vector Float -> Bool
eqVectorFloat = (P.==)

eqVectorDouble :: VS.Vector Double -> VS.Vector Double -> Bool
eqVectorDouble = (P.==)

eqVectorInt :: VS.Vector Int -> VS.Vector Int -> Bool
eqVectorInt = (P.==)

eqUnboxedVectorFloat :: VU.Vector Float -> VU.Vector Float -> Bool
eqUnboxedVectorFloat = (P.==)

eqUnboxedVectorDouble :: VU.Vector Double -> VU.Vector Double -> Bool
eqUnboxedVectorDouble = (P.==)

eqUnboxedVectorInt :: VU.Vector Int -> VU.Vector Int -> Bool
eqUnboxedVectorInt = (P.==)

class (ValidEq (v r), ValidEq r, VG.Vector v r, Logic (v r)~Logic r) => ValidVector v r
instance (ValidEq (v r), ValidEq r, VG.Vector v r, Logic (v r)~Logic r) => ValidVector v r

-------------------------------------------------------------------------------

listToVector :: VG.Vector v a => [a] -> v a
listToVector = VG.fromList

mkMutable [t| forall a. VS.Vector a |]
mkMutable [t| forall a. VU.Vector a |]
mkMutable [t| forall a. V.Vector a |]

-------------------------------------------------------------------------------

newtype ArrayT v r = ArrayT { unArrayT :: v r }
    deriving (Read,Show,Arbitrary,Typeable)

mkMutable [t| forall a b. ArrayT a b |]

type Array = ArrayT BoxedVector
type UnboxedArray = ArrayT UnboxedVector
type StorableArray = ArrayT VS.Vector

type instance Scalar (ArrayT v r) = Int
type instance Logic (ArrayT v r) = Logic r
type instance Elem (ArrayT v r) = r
type instance SetElem (ArrayT v r) r' = ArrayT v r'

instance ValidVector v r => Eq_ (ArrayT v r) where
    (ArrayT v1)==(ArrayT v2) = v1==v2
    (ArrayT v1)/=(ArrayT v2) = v1/=v2

instance NFData (v r) => NFData (ArrayT v r) where
    rnf (ArrayT v) = rnf v

-- instance Eq (v r) => Eq (ArrayT v r) where
--     (ArrayT v1)==(ArrayT v2) = v1==v2

instance ValidVector v r => Normed (ArrayT v r) where
    size = VG.length

instance VG.Vector v a => VG.Vector (ArrayT v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (ArrayTM v) = liftM ArrayT $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (ArrayT v) = liftM ArrayTM $ VG.basicUnsafeThaw v
    basicLength (ArrayT v) = VG.basicLength v
    basicUnsafeSlice s t (ArrayT v) = ArrayT $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (ArrayT v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (ArrayTM vm) (ArrayT v) = VG.basicUnsafeCopy vm v
    elemseq (ArrayT v) a b = VG.elemseq v a b

newtype ArrayTM v s a = ArrayTM { unArrayTM :: v s a }

instance VGM.MVector v a => VGM.MVector (ArrayTM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (ArrayTM v) = VGM.basicLength v
    basicUnsafeSlice s t (ArrayTM v) = ArrayTM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (ArrayTM v1) (ArrayTM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM ArrayTM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM ArrayTM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (ArrayTM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (ArrayTM v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (ArrayT v) = ArrayTM (VG.Mutable v)

instance ValidVector v r => Semigroup (ArrayT v r) where
    (ArrayT v1)+(ArrayT v2) = ArrayT $ v1 VG.++ v2

instance ValidVector v r => Monoid (ArrayT v r) where
    zero = ArrayT $ VG.empty

instance ValidVector v r => Container (ArrayT v r) where
    elem r (ArrayT v) = elem r $ VG.toList v
    notElem r (ArrayT v) = not $ elem r $ VG.toList v

instance ValidVector v r => Constructible (ArrayT v r) where
    singleton r = ArrayT $ VG.singleton r

    fromList1 x xs = ArrayT $ VG.fromList (x:xs)

    fromList1N n x xs = ArrayT $ VG.fromListN n (x:xs)

instance ValidVector v r => Foldable (ArrayT v r) where

    {-# INLINE toList #-}
    toList (ArrayT v) = VG.toList v

    {-# INLINE uncons #-}
    uncons (ArrayT v) = if VG.null v
        then Nothing
        else Just (VG.head v, ArrayT $ VG.tail v)

    {-# INLINE unsnoc #-}
    unsnoc (ArrayT v) = if VG.null v
        then Nothing
        else Just (ArrayT $ VG.init v, VG.last v)

    {-# INLINE foldMap #-}
    foldMap f   (ArrayT v) = VG.foldl' (\a e -> a + f e) zero v

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    {-# INLINE foldr1' #-}
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldl1' #-}
    foldr   f x (ArrayT v) = VG.foldr   f x v
--     foldr'  f x (ArrayT v) = {-# SCC foldr'_ArrayT #-} VG.foldr'  f x v
    foldr'  !f !x (ArrayT !v) = {-# SCC foldr'_ArrayT #-} vecfold  f x v
    foldr1  f   (ArrayT v) = VG.foldr1  f   v
    foldr1' f   (ArrayT v) = VG.foldr1' f   v
    foldl   f x (ArrayT v) = VG.foldl   f x v
    foldl'  f x (ArrayT v) = VG.foldl'  f x v
    foldl1  f   (ArrayT v) = VG.foldl1  f   v
    foldl1' f   (ArrayT v) = VG.foldl1' f   v

{-# INLINE vecfold #-}
-- vecfold :: VG.Vector v a => (a -> b -> a) -> b -> v a -> b
vecfold !f !tot !v = {-# SCC vecfold #-} if VG.length v > 0
    then goEach 0 tot
    else tot
    where
        goEach !i !tot = if i>=VG.length v
            then tot
            else goEach (i+1) $ f (v `VG.unsafeIndex` i) tot

instance
    ( ClassicalLogic a
    , ClassicalLogic (v a)
    , Eq_ (v a)
    , POrd_ a
    , VG.Vector v a
    ) => Partitionable (ArrayT v a)
        where
    partition n (ArrayT vec) = go 0
        where
            go i = if i>=VG.length vec
                then []
                else (ArrayT $ VG.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= VG.length vec
                        then (VG.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ VG.length vec :: Double) / (fromIntegral n)

instance (Eq (v r), POrd r, ValidVector v r) => POrd_ (ArrayT v r) where
    inf (ArrayT v1) (ArrayT v2) = ArrayT $ VG.fromList $ inf (VG.toList v1) (VG.toList v2)

instance (Eq (v r), POrd r, ValidVector v r) => MinBound_ (ArrayT v r) where
    minBound = zero

type instance Index (ArrayT v s) = Int

instance (Eq_ s, Complemented (Logic s)) => IxContainer (Array s) where
    lookup i s = s VG.!? i
    (!) = VG.unsafeIndex
    indices s = [0..VG.length s-1]
    values = VG.toList
    imap = VG.imap

instance (VU.Unbox s, Eq_ s, Complemented (Logic s)) => IxContainer (UnboxedArray s) where
    lookup i s = s VG.!? i
    (!) = VG.unsafeIndex
    indices s = [0..VG.length s-1]
    values = VG.toList
--     imap = VG.imap

-------------------------------------------------------------------------------

type UnboxedVector = VU.Vector

type instance Scalar (VU.Vector r) = Scalar r
type instance Logic (VU.Vector r) = Logic r

type instance VU.Vector a >< b = VU.Vector (a><b)
type instance V.Vector  a >< b = V.Vector  (a><b)

instance (VU.Unbox r, Arbitrary r) => Arbitrary (VU.Vector r) where
    arbitrary = liftM VG.fromList arbitrary
    shrink v = map VG.fromList $ shrink (VG.toList v)

instance (VU.Unbox r, ValidEq r) => Eq_ (VU.Vector r) where
    {-# INLINE (==) #-}
    xs == ys = toList (ArrayT xs) == toList (ArrayT ys)
--     xs == ys = eq (VG.stream xs) (VG.stream ys)

instance (VU.Unbox r, Ord r) => POrd_ (VU.Vector r) where
    inf v1 v2 = unArrayT $ unLexical $ inf (Lexical (ArrayT v1)) (Lexical (ArrayT v2))


instance (VU.Unbox r, Ord r) => Lattice_ (VU.Vector r) where
    sup v1 v2 = unArrayT $ unLexical $ sup (Lexical (ArrayT v1)) (Lexical (ArrayT v2))

instance (VU.Unbox r, Ord r) => Ord_ (VU.Vector r) where

instance (VU.Unbox r,  Semigroup r) => Semigroup (VU.Vector r) where
    {-# INLINE (+) #-}
    v1 + v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "VU.Vector + two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i + v2 `VG.unsafeIndex` i

instance (VU.Unbox r,  Monoid r) => Monoid (VU.Vector r) where
    {-# INLINE zero #-}
    zero = VG.empty

instance (VU.Unbox r,  Abelian r) => Abelian (VU.Vector r)

instance (VU.Unbox r,  Cancellative r) => Cancellative (VU.Vector r) where
    {-# INLINE (-) #-}
    v1 - v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "VU.Vector - two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i - v2 `VG.unsafeIndex` i


instance (VU.Unbox r,  Group r) => Group (VU.Vector r) where
    {-# INLINE negate #-}
    negate v = VG.map negate v

instance (VU.Unbox r,  Module r, IsScalar (Scalar r)) => Module (VU.Vector r) where
    {-# INLINE (.*) #-}
    v .* r = VG.map (r*.) v

instance (VU.Unbox r,  FreeModule r, IsScalar (Scalar r)) => FreeModule (VU.Vector r) where
    {-# INLINE (.*.) #-}
    u .*. v = if VG.length u == VG.length v
        then VG.zipWith (.*.) u v
        else error "(.*.): u and v have different lengths"

instance (VU.Unbox r, VectorSpace r, IsScalar (Scalar r)) => VectorSpace (VU.Vector r) where
    {-# INLINE (./) #-}
    v ./ r = VG.map (./r) v

    {-# INLINE (./.) #-}
    u ./. v = if VG.length u == VG.length v
        then VG.zipWith (./.) u v
        else error "(./.): u and v have different lengths"

-- instance
--     ( IsScalar r
--     , Normed r
--     , Logic r~Bool
--     , VectorSpace r
--     , ExpField r
--     , VU.Unbox r
--     ) => Normed (VU.Vector r)
--         where
--     size = innerProductNorm
--
-- instance
--     ( IsScalar r
--     , Normed r
--     , Logic r~Bool
--     , VectorSpace r
--     , ExpField r
--     , VU.Unbox r
--     ) => Banach (VU.Vector r)
--
-- instance
--     ( IsScalar r
--     , Normed r
--     , Logic r~Bool
--     , VectorSpace r
--     , ExpField r
--     , VU.Unbox r
--     ) => Hilbert (VU.Vector r)
--         where
--     v1 <> v2 = if VG.length v1 == 0
--         then zero
--         else if VG.length v2 == 0
--             then zero
--             else if VG.length v1 /= VG.length v2
--                 then error "inner product on storable vectors of different sizes"
--                 else VG.foldl' (+) zero $ VG.zipWith (*) v1 v2

instance Metric (VU.Vector Float)
        where

    {-# INLINABLE[1] distance #-}
    distance v1 v2 = {-# SCC distance_l2_hask #-} sqrt $ go 0 0
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

    {-# INLINE[1] distanceUB #-}
    distanceUB !v1 !v2 !dist = {-# SCC distanceUB_UVector #-}
        go 0 0
        where
            dist2=dist*dist

            go !tot !i = if i>VG.length v1-4
                then goEach tot i
                else if tot'>dist2
                    then tot'
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
                then sqrt tot
                else if tot'>dist2
                    then tot'
                    else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

type instance Index (VU.Vector r) = Int
type instance Elem (VU.Vector r) = r
type instance SetElem (VU.Vector r) b = VU.Vector b

instance (IsScalar r, VU.Unbox r) => IxContainer (VU.Vector r) where
    lookup i s = s VG.!? i
    indices s = [0..VG.length s-1]
    values = VG.toList

instance (IsScalar r, FreeModule r, VU.Unbox r) => FiniteModule (VU.Vector r) where
    unsafeToModule = VG.fromList

-------------------------------------------------------------------------------

type BoxedVector = V.Vector

type instance Scalar (V.Vector r) = Scalar r
type instance Logic (V.Vector r) = Logic r

instance Arbitrary r => Arbitrary (V.Vector r) where
    arbitrary = liftM VG.fromList arbitrary
    shrink v = map VG.fromList $ shrink (VG.toList v)

instance ValidEq r => Eq_ (V.Vector r) where
    {-# INLINE (==) #-}
    xs == ys = toList (ArrayT xs) == toList (ArrayT ys)
--     xs == ys = eq (VG.stream xs) (VG.stream ys)

instance (VG.Vector V.Vector r, Ord r) => POrd_ (V.Vector r) where
    inf v1 v2 = unArrayT $ unLexical $ inf (Lexical (ArrayT v1)) (Lexical (ArrayT v2))

instance (VG.Vector V.Vector r, Ord r) => Lattice_ (V.Vector r) where
    sup v1 v2 = unArrayT $ unLexical $ sup (Lexical (ArrayT v1)) (Lexical (ArrayT v2))

instance (VG.Vector V.Vector r, Ord r) => Ord_ (V.Vector r)

instance ( Semigroup r) => Semigroup (V.Vector r) where
    {-# INLINE (+) #-}
    v1 + v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "V.Vector + two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i + v2 `VG.unsafeIndex` i

instance ( Monoid r) => Monoid (V.Vector r) where
    {-# INLINE zero #-}
    zero = VG.empty

instance ( Abelian r) => Abelian (V.Vector r)

instance ( Cancellative r) => Cancellative (V.Vector r) where
    {-# INLINE (-) #-}
    v1 - v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "V.Vector - two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i - v2 `VG.unsafeIndex` i


instance ( Group r) => Group (V.Vector r) where
    {-# INLINE negate #-}
    negate v = VG.map negate v

instance ( Module r, IsScalar (Scalar r)) => Module (V.Vector r) where
    {-# INLINE (.*) #-}
    v .* r = VG.map (r*.) v

instance ( FreeModule r, IsScalar (Scalar r)) => FreeModule (V.Vector r) where
    {-# INLINE (.*.) #-}
    u .*. v = if VG.length u == VG.length v
        then VG.zipWith (.*.) u v
        else error "(.*.): u and v have different lengths"

instance ( VectorSpace r, IsScalar (Scalar r)) => VectorSpace (V.Vector r) where
    {-# INLINE (./) #-}
    v ./ r = VG.map (./r) v

    {-# INLINE (./.) #-}
    u ./. v = if VG.length u == VG.length v
        then VG.zipWith (./.) u v
        else error "(./.): u and v have different lengths"

-- instance
--     ( Hilbert r
--     , ExpField (Scalar r)
--     ) => Normed (V.Vector r)
--         where
--     size = innerProductNorm
--
-- instance
--     ( Hilbert r
--     , ExpField (Scalar r)
--     ) => Banach (V.Vector r)

-- instance
--     ( Hilbert r
--     , ExpField (Scalar r)
--     ) => Metric (V.Vector r)
--         where
--     distance = innerProductDistance
--
-- instance
--     ( Hilbert r
--     , ExpField (Scalar r)
--     ) => Hilbert (V.Vector r)
--         where
--     v1 <> v2 = if VG.length v1 == 0 || VG.length v2 == 0
--         then zero
--         else if VG.length v1 /= VG.length v2
--             then error "inner product on vectors of different sizes"
--             else VG.foldl' (+) zero $ VG.zipWith (<>) v1 v2


type instance Index (V.Vector r) = Int
type instance Elem (V.Vector r) = r
type instance SetElem (V.Vector r) b = V.Vector b

instance (IsScalar r) => IxContainer (V.Vector r) where
    lookup i s = s VG.!? i
    indices s = [0..VG.length s-1]
    values = VG.toList

instance (IsScalar r, FreeModule r) => FiniteModule (V.Vector r) where
    unsafeToModule = VG.fromList

-------------------------------------------------------------------------------

type instance Scalar (VS.Vector r) = Scalar r
type instance Logic (VS.Vector r) = Logic r

instance (Storable r, Arbitrary r) => Arbitrary (VS.Vector r) where
--     arbitrary = liftM VG.fromList arbitrary
--     shrink v = map VG.fromList $ shrink (VG.toList v)
    arbitrary = frequency
        [ (7, VG.replicateM 10 arbitrary)
--         , (1, return VG.empty)
        ]

instance (VG.Vector VS.Vector r, Storable r, ValidEq r) => Eq_ (VS.Vector r) where
    {-# INLINE[1] (==) #-}
    xs == ys = toList (ArrayT xs) == toList (ArrayT ys)
--     xs == ys = eq (VG.stream xs) (VG.stream ys)

instance (VG.Vector VS.Vector r, Ord r, Storable r) => POrd_ (VS.Vector r) where
    inf v1 v2 = unArrayT $ unLexical $ inf (Lexical (ArrayT v1)) (Lexical (ArrayT v2))

instance (VG.Vector VS.Vector r, Ord r, Storable r) => Lattice_ (VS.Vector r) where
    sup v1 v2 = unArrayT $ unLexical $ sup (Lexical (ArrayT v1)) (Lexical (ArrayT v2))

instance (VG.Vector VS.Vector r, Ord r, Storable r) => Ord_ (VS.Vector r)

instance (Storable r, Semigroup r) => Semigroup (VS.Vector r) where
    {-# INLINE (+) #-}
    v1 + v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "VS.Vector + two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i + v2 `VG.unsafeIndex` i

instance (Storable r, Monoid r) => Monoid (VS.Vector r) where
    {-# INLINE zero #-}
    zero = VG.empty

instance (Storable r, Abelian r) => Abelian (VS.Vector r)

instance (Storable r, Cancellative r) => Cancellative (VS.Vector r) where
    {-# INLINE (-) #-}
    v1 - v2 = if VG.length v1 == 0
        then v2
        else if VG.length v2 == 0
            then v1
            else if VG.length v1 /= VG.length v2
                then error "VS.Vector - two different non-zero lengths"
                else VG.generate (VG.length v1) go
        where
            go i = v1 `VG.unsafeIndex` i - v2 `VG.unsafeIndex` i


instance (Storable r, Group r) => Group (VS.Vector r) where
    {-# INLINE negate #-}
    negate v = VG.map negate v
