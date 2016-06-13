{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SubHask.Algebra.Accelerate.Vector
    (
    ValidACCVector
    , ValidSVector
    , ACCVector (..)
    , mkAccVector
    , mkAccVectorFromList
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

newtype ACCVector (bknd::Backend) (n::k) a = ACCVector (A.Array A.DIM1 a)

type instance Scalar (ACCVector bknd n r) = Scalar r
type instance Logic (ACCVector bknd n r) = Logic  r

-- type instance ACCVector bknd m a >< b = A.Exp (Tensor_ACCVector (ACCVector bknd m a) b)
-- type family Tensor_ACCVector a b where
--     Tensor_ACCVector (ACCVector bknd n r1) (ACCVector bknd m r2) = ACCVector bknd n r1 +> ACCVector bknd m r2
--     Tensor_ACCVector (ACCVector bknd n r1) r1 = ACCVector bknd n r1 -- (r1><r2)

type ValidACCVector bknd n a = ((ACCVector (bknd::Backend) n a><Scalar a)
                                ~ACCVector (bknd::Backend) n a
                                , Prim a
                                , A.Elt a
                                , A.IsNum a
                                -- , A.Eq (A.Array A.DIM1 a)
                                -- , A.Lift A.Exp (A.Acc (A.Array A.DIM1 a))
                                , P.Num (A.Exp a)
                                , P.Floating (A.Exp a)
                                , A.IsFloating a
                                , Scalar a ~ Scalar (Scalar a)
                                -- , Scalar a ~ Scalar (A.Exp a)
                                -- , Logic a ~ Logic (A.Exp a)
                                -- , Actor a ~ A.Exp a
                                -- , Index a ~ A.Exp a
                                -- , Elem a ~ A.Exp a
                                -- , Scalar (Scalar a)~ A.Exp (Scalar (Scalar a))
                                , A.Plain a ~ a
                                -- , P.Floating (A.Acc (A.Scalar a))
                                )

type instance Index (ACCVector bknd n r) = Int
type instance Elem (ACCVector bknd n r) = Elem r
type instance SetElem (ACCVector (bknd::Backend) n r) b = ACCVector (bknd::Backend) n b


type instance Actor (ACCVector (bknd::Backend) n r) = Actor r

instance Prim a => IsMutable (ACCVector (bknd::Backend) (n::Symbol) a)

instance (Monoid r, ValidACCVector b n r) => Semigroup (ACCVector (b::Backend) (n::Symbol) r) where
    {-# INLINE (+)  #-}
    (+) (ACCVector v1) (ACCVector v2)=ACCVector (A.zipWith (A.+) v1 v2)

instance (ValidACCVector bknd n r, Action r, Semigroup r, Prim r) => Action (ACCVector (bknd::Backend) (n::Symbol) r) where
  {-# INLINE (.+)   #-}
  (.+) (ACCVector v) r = ACCVector (A.map (P.+ r) v)

instance (Monoid r, Cancellative r, ValidACCVector bknd n r) => Cancellative (ACCVector (bknd::Backend) (n::Symbol) r) where
    {-# INLINE (-)  #-}
    (-) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (P.-) a1 a2)

--The zero method wants a Ring r in the case of "0"
--or Field r in the case of "0.0"  Not exactly sure how to handle this.
instance (Monoid r, ValidACCVector bknd n r) => Monoid (ACCVector (bknd::Backend) (n::Symbol) r) where
    -- {-# INLINE zero #-}
    -- zero = ACCVector(A.fill (A.index1 (A.constant 1)) (A.constant (0::r)))
    -- zero = ACCVector(A.use (A.fromList (A.Z A.:.1) [(0::r)]))


instance (Group r, ValidACCVector bknd n r) => Group (ACCVector (bknd::Backend) (n::Symbol) r) where
    {-# INLINE negate #-}
    negate = negate

instance (Monoid r, Abelian r, ValidACCVector bknd n r) => Abelian (ACCVector (bknd::Backend)  (n::Symbol) r)

instance (FreeModule r, ValidACCVector bknd n r, IsScalar r) => FreeModule (ACCVector (bknd::Backend)  (n::Symbol) r) where
    {-# INLINE (.*.)   #-}
    (.*.) (ACCVector a1) (ACCVector a2) = ACCVector( A.zipWith (P.*) a1 a2)

instance (Module r, ValidACCVector bknd n r, IsScalar r) => Module (ACCVector (bknd::Backend) (n::Symbol) r) where
    {-# INLINE (.*)   #-}
    (.*) (ACCVector  v) r = ACCVector (A.map (P.* (A.constant r)) v)

instance (VectorSpace r, ValidACCVector bknd n r, IsScalar r) => VectorSpace (ACCVector (bknd::Backend) (n::Symbol) r) where
    {-# INLINE (./)   #-}
    (./) (ACCVector  v) r = ACCVector (A.map (P./ (A.constant r)) v)

    {-# INLINE (./.)  #-}
    (./.) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (P./) a1 a2)


--Full error from FiniteModule instance:
  -- Could not deduce (r ~ A.Exp r)
  -- from the context (FreeModule r,
  --                   ValidLogic r,
  --                   ValidACCVector b n r,
  --                   IsScalar r)
  --   bound by the instance declaration
  --   at src/SubHask/Algebra/Accelerate/Vector.hs:123:10-115
  --   ‘r’ is a rigid type variable bound by
  --       the instance declaration
  --       at src/SubHask/Algebra/Accelerate/Vector.hs:123:10
  -- In the instance declaration for ‘FiniteModule (ACCVector b n r)’


instance (FreeModule r, ValidLogic r, ValidACCVector b n r, IsScalar r) => FiniteModule (A.Exp (ACCVector b (n::Symbol) r))
  where
    {-# INLINE dim #-}
    dim (ACCVector v) = A.size v



instance
    ( P.Num (A.Exp r)
    , Monoid r
    , ValidLogic r
    , ValidACCVector b n r
    , IsScalar r
    , FreeModule r
    ) => IxContainer (ACCVector b (n::Symbol) r)
        where

    {-# INLINE (!) #-}
    (!) (ACCVector v) i =  v A.! (A.index1 (A.lift i))

    --Couldn't match type ‘A.Exp Bool’ with ‘Bool’
    {-# INLINABLE imap #-}
    imap f (ACCVector v) = let
      shp = A.shape v
      idxs = A.generate shp P.id
      mpd = A.zipWith f idxs v :: f (A.Exp r) -> f (A.Exp r) -> f (A.Exp r)
      in ACCVector mpd

    type ValidElem (ACCVector b n r) e = (ClassicalLogic e, IsScalar e, FiniteModule e, ValidACCVector b n e)

instance (Eq r, Monoid r, ValidACCVector b n r) => Eq_ (ACCVector b (n::Symbol) r) where
    {-# INLINE (==) #-}
    (ACCVector v2) == (ACCVector v1) = let
      l = (A.lift v1) A.==* (A.lift v2)
      in l

instance
    ( ValidACCVector b n r
    , P.Num (A.Exp r)
    , ExpField r
    , Normed r
    , Ord_ r
    , Logic r~ Bool
    , IsScalar r
    , VectorSpace r
    ) => Metric (ACCVector b (n::Symbol) r)

        where
    {-# INLINE[2] distance #-}
    distance (ACCVector v1) (ACCVector v2) = {-# SCC distance_ACCVector #-}let
      dmag = A.zipWith (P.-) v1 v2
      dsq = A.zipWith (P.*) dmag dmag
      drt = A.sqrt (A.sum dsq)
      in A.lift (A.the drt)

instance (VectorSpace r, ValidACCVector b n r, IsScalar r, ExpField r) => Normed (ACCVector b (n::Symbol) r) where
    {-# INLINE size #-}
    size (ACCVector v1) = let
      sq = A.zipWith (P.*) v1 v1
      s = A.fold (P.+) (A.constant 0.0) sq
      in A.the (A.sqrt s)

instance
    ( VectorSpace r
    , ValidACCVector b n r
    , IsScalar r
    , ExpField r
    , Real r
    ) => Banach (ACCVector b (n::Symbol) r)

instance
    ( FiniteModule (ACCVector b (n::Symbol) r)
    , VectorSpace (ACCVector b (n::Symbol) r)
    , MatrixField r
    ) => TensorAlgebra (ACCVector b (n::Symbol) r)
        where
    (ACCVector v1)><(ACCVector v2) = let
      r = A.size v1
      c = A.size v2
      arr = A.map (\i -> A.lift (A.map (\j -> i * j ) v1)) v2
      m = A.reshape (A.Z A.:. r A.:. c) arr
      in m

instance
    ( VectorSpace r
    , ValidACCVector b n r
    , IsScalar r
    , ExpField r
    , Real r
    , OrdField r
    , MatrixField r
    , P.Num r
    ) => Hilbert (ACCVector b (n::Symbol) r)
    where
    {-# INLINE (<>) #-}
    (<>) (ACCVector v1) (ACCVector v2) = let
      singleton = A.fold (+) 0 (A.zipWith (*) v1 v2)
      in A.the singleton

-- In Alegebra.Vector.hs this is defined in terms of HMatrix
-- recreated here to satisfy constraints
type MatrixField r =
    ( IsScalar r
    , VectorSpace r
    , Field r
    )
