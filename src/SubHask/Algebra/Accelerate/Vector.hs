module SubHask.Algebra.Accelerate.Vector
    (
    ValidACCVector
    , ACCVector (..)
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

import qualified Data.Array.Accelerate as A

import SubHask.Algebra
import SubHask.Algebra.Accelerate.AccelerateBackend (Backend)
import SubHask.Category
import SubHask.Algebra.Vector (SVector, type (+>))
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.SubType

import System.IO.Unsafe
import Unsafe.Coerce




--------------------------------------------------------------------------------

-- | Accelerate based Vector
-- | A.Acc is an accelreate computation, A.Array A.DIM1 a is a one dimensional array

newtype ACCVector (bknd::Backend) (n::k) a = ACCVector (A.Acc (A.Array A.DIM1 a))

type instance Scalar (ACCVector bknd n r) = A.Exp r--Scalar r
type instance Logic (ACCVector bknd n r) = A.Exp r--Logic r

type instance ACCVector bknd m a >< b = Tensor_ACCVector (ACCVector bknd m a) b
type family Tensor_ACCVector a b where
    Tensor_ACCVector (ACCVector bknd n r1) (ACCVector bknd m r2) = ACCVector bknd n r1 +> ACCVector bknd m r2
    Tensor_ACCVector (ACCVector bknd n r1) r1 = ACCVector bknd n r1 -- (r1><r2)

type IsExpScalar r = (Ring (A.Exp r), Ord_ (A.Exp r), Scalar (A.Exp r)~(A.Exp r), Normed (A.Exp r),(A.Exp r)~((A.Exp r)><(A.Exp r)))

type ValidACCVector bknd n a = ((ACCVector (bknd::Backend) n a><a)
                                ~ACCVector (bknd::Backend) n a
                                -- , (ACCVector (bknd::Backend) n a><Scalar (A.Exp a))
                                  -- ~ACCVector (bknd::Backend) n a
                                -- , (A.Exp a >< A.Exp a) ~ A.Exp a
                                -- , ACCVector (bknd::Backend) n a ~ ACCVector (bknd::Backend) n (A.Exp a)
                                , Prim a
                                , IsExpScalar a
                                , A.Elt a
                                --, Elem a ~ A.Exp a
                                --, A.IsNum a
                                , Tensor_ACCVector (ACCVector bknd n a) a
                                  ~ ACCVector bknd n a
                                , Tensor_ACCVector (ACCVector bknd n a) (A.Exp a)
                                  ~ ACCVector bknd n a
                                -- , A.Eq (A.Array A.DIM1 a)
                                -- , A.Lift A.Exp (A.Acc (A.Array A.DIM1 a))
                                , P.Num (A.Exp a)
                                --, P.Floating (A.Exp a)
                                --, A.IsFloating a
                                -- , Scalar (Scalar (A.Exp a)) ~ A.Exp a
                                , a ~ Scalar (Scalar a)
                                -- , Scalar a ~ a
                                -- , Scalar (A.Exp a) ~ Scalar (Scalar (A.Exp a))
                                -- , Scalar (A.Exp a) ~ (A.Exp a)
                                -- , Logic (A.Exp Bool) ~ A.Exp Bool
                                --, Logic (A.Exp a) ~ A.Exp Bool
                                -- , Logic (A.Exp a) ~ Bool
                                --, Normed (A.Exp a)
                                -- , Ord_ (A.Exp a)
                                --, Ring (A.Exp a)
                                -- , Field (A.Exp a)
                                , P.Fractional (A.Exp a)
                                , P.Floating (A.Exp a)
                                -- , Actor a ~ A.Exp a
                                , A.Eq (A.Array A.DIM1 a)
                                , A.Lift A.Exp (A.Acc (A.Array A.DIM1 a))
                                --, P.Floating (A.Acc (A.Scalar a))
                                )

type instance Index (ACCVector bknd n r) = A.Exp Int --Index r
type instance Elem (ACCVector bknd n r) = A.Exp r
type instance SetElem (ACCVector (bknd::Backend) n r) b = ACCVector (bknd::Backend) n b


type instance Actor (ACCVector (bknd::Backend) n r) = A.Exp r

instance (KnownNat n, Prim a) => IsMutable (ACCVector (bknd::Backend) (n::Nat) a)

instance (KnownNat n, Monoid r, ValidACCVector b n r) => Semigroup (ACCVector (b::Backend) (n::Nat) r) where
    {-# INLINE (+)  #-}
    (+) (ACCVector v1) (ACCVector v2)=ACCVector (A.zipWith (A.+) v1 v2)

instance (KnownNat n, ValidACCVector bknd n r, Action r, Semigroup r, Prim r) => Action (ACCVector (bknd::Backend) (n::Nat) r) where
  {-# INLINE (.+)   #-}
  (.+) (ACCVector v) r = ACCVector (A.map (P.+  r) v)

instance (KnownNat n, Monoid r, Cancellative r, ValidACCVector bknd n r) => Cancellative (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (-)  #-}
    (-) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (P.-) a1 a2)

--The zero method wants a Ring r in the case where zero is the integer "0"
--or Field r in the case of "0.0"
--In either case, the Group instance wants the same constraint. Not exactly sure how to handle this.
instance (KnownNat n, Monoid r, ValidACCVector bknd n r) => Monoid (ACCVector (bknd::Backend) (n::Nat) r) where
--     {-# INLINE zero #-}
--     zero = ACCVector(A.use (A.fromList (A.Z A.:.1) [(0::r)]))

instance (KnownNat n, Group r, ValidACCVector bknd n r) => Group (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE negate #-}
    negate = negate

instance (KnownNat n, Monoid r, Abelian r, ValidACCVector bknd n r) => Abelian (ACCVector (bknd::Backend)  (n::Nat) r)

instance (KnownNat n, FreeModule r, ValidACCVector bknd n r, IsExpScalar r) => FreeModule (ACCVector (bknd::Backend)  (n::Nat) r) where
    {-# INLINE (.*.)   #-}
    (.*.) (ACCVector a1) (ACCVector a2) = ACCVector( A.zipWith (P.*) a1 a2)

instance (KnownNat n, Module r, ValidACCVector bknd n r) => Module (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (.*)   #-}
    (.*) (ACCVector  v) r = ACCVector (A.map (P.* r) v)

instance (KnownNat n, VectorSpace r, ValidACCVector bknd n r) => VectorSpace (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (./)   #-}
    (./) (ACCVector  v) r = ACCVector (A.map (P./ r) v)

    {-# INLINE (./.)  #-}
    (./.) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (P./) a1 a2)

-- Could not deduce (r ~ Elem r)
-- In the instance declaration for ‘FiniteModule (ACCVector b n r)’

instance (KnownNat n, FreeModule r, ValidLogic r, ValidACCVector b n r) => FiniteModule (ACCVector b (n::Nat) r)
--Couldn't match expected type ‘Int’ with actual type ‘A.Exp Int’
  where
    --dim :: ACCVector b (n::Nat) r -> Index(A.Exp Int)
    {-# INLINE dim #-}
    dim (ACCVector v) = A.size v


-- Could not deduce (r ~ Elem r)
instance
    ( Monoid r
    , ValidLogic r
    , ValidACCVector b n r
    , IsExpScalar r
    , KnownNat n
    , FreeModule r
    ) => IxContainer (ACCVector b (n::Nat) r)
        where

    {-# INLINE (!) #-}
    (!) (ACCVector v) i =  A.the (v A.! A.index1 i)

    {-# INLINABLE imap #-}
    imap f (ACCVector v) = let
      mpd = A.imap (\x i -> f i x) v
      in ACCVector mpd

    type ValidElem (ACCVector b n r) e = (FiniteModule e, ValidACCVector b n e)

instance (A.Eq r, KnownNat n, Eq_ r, Monoid r, ValidACCVector b n r) => Eq_ (ACCVector b (n::Nat) r) where
    --(==) :: ACCVector b n r -> ACCVector b n r -> A.Acc (A.Scalar Bool)
    {-# INLINE (==) #-}
    (ACCVector v2) == (ACCVector v1) = let
      l = A.zipWith (A.==*) v1 v2
      ele = l A.! A.index1 (A.constant 0)
      bl = A.all (A.&&* ele) l
      in A.the bl

instance
    ( ValidACCVector b n r
    , ExpField r
    --, Normed r
    , Ord_ r
    , IsExpScalar r
    , VectorSpace r
    , KnownNat n
    ) => Metric (ACCVector b (n::Nat) r)

        where
    {-# INLINE[2] distance #-}
    distance (ACCVector v1) (ACCVector v2) = {-# SCC distance_ACCVector #-}let
      dmag = A.zipWith (P.-) v1 v2
      dsq = A.zipWith (P.*) dmag dmag
      drt = A.sqrt (A.sum dsq)
      in A.the drt

instance (P.Floating (A.Acc (A.Array A.DIM0 r)), KnownNat n, VectorSpace r, ValidACCVector b n r, ExpField r) => Normed (ACCVector b (n::Nat) r) where
    {-# INLINE size #-}
    --Could not deduce (r ~ A.Exp r)
    size :: ACCVector b (n::Nat) r -> A.Exp r
    size (ACCVector v1) = let
      sq = A.zipWith (P.*) v1 v1 :: A.Acc (A.Array A.DIM1 r)
      s = A.fold (P.+) (A.constant 0.0) sq
      srt = A.sqrt (s::A.Acc (A.Array A.DIM0 r))
      in A.the srt :: A.Exp r


-- -- Couldn't match type ‘A.Exp Bool’ with ‘Bool’

instance
    ( VectorSpace r
    , ValidACCVector b n r
    , IsExpScalar r
    , ExpField r
    , Real r
    , KnownNat n
    ) => Banach (ACCVector b (n::Nat) r)

instance
    ( FiniteModule (ACCVector b (n::Nat) r)
    , VectorSpace (ACCVector b (n::Nat) r)
    , Normed (ACCVector b n r +> ACCVector b n r)
    , KnownNat n
    , MatrixField r
    ) => TensorAlgebra (ACCVector b (n::Nat) r)
        where
    (ACCVector v1)><(ACCVector v2) = let
      r = A.size v1
      c = A.size v2
      arr = A.map (\i -> A.lift (A.map (\j -> i * j ) v1)) v2
      m = A.reshape (A.index2 r c) arr :: ACCVector bknd n r +> ACCVector bknd m r
      in m

instance
    ( VectorSpace r
    , ValidACCVector b n r
    , IsExpScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , KnownNat n
    , P.Num r
    ) => Hilbert (ACCVector b (n::Nat) r)
    where
    {-# INLINE (<>) #-}
    (<>) (ACCVector v1) (ACCVector v2) = let
      singleton = A.fold (+) 0 (A.zipWith (*) v1 v2)
      in A.the singleton :: A.Exp r

-- In Alegebra.Vector.hs this is defined in terms of HMatrix
-- recreated here to satisfy constraints
type MatrixField r =
    ( IsExpScalar r
    , VectorSpace r
    , Field r
    )
