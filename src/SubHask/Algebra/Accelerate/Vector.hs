module SubHask.Algebra.Accelerate.Vector
    (
    ValidACCVector
    , ACCVector (..)
    , ValidBackend(..)
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
import SubHask.Algebra.Accelerate.AccelerateBackend (Backend(..))
import SubHask.Category
import SubHask.Algebra.Vector (SVector, type (+>))
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.SubType

import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Interpreter as I
import SubHask.Internal.Prelude
import qualified Prelude as P

--FIXME:  Replace all intermediary lists with correct use of acclerate-io
mkAccVectorFromList :: A.Elt a => [a] -> ACCVector bknd (n::Nat) a
mkAccVectorFromList l = let
    len = P.length l
  in ACCVector (A.use (A.fromList (A.Z A.:.len) l))

-- mkAccVector :: (A.Elt a, ValidSVector (n::Symbol) a) => SVector (n::Symbol) a -> ACCVector (bknd::Backend) (n::Symbol) a
-- mkAccVector v @(SVector_Dynamic fp off n) = let
--   arr = A.fromList (A.Z A.:. n) $ unsafeInlineIO $ go (n-1) []
--   go (-1) xs = return $ xs
--   go i    xs = withForeignPtr fp $ \p -> do
--       x <- peekElemOff p (off+i)
--       go (i-1) (x:xs)
--   in ACCVector (A.use arr)

-- acc2SVector :: ValidACCVector (b::Backend) n a => ACCVector (b::Backend) n a  -> SVector n a
-- acc2SVector (ACCVector v) = unsafeToModule $ (runAccVector v) :: SVector n a


class ValidBackend (b::Backend) where
    runAccVector :: (ValidACCVector (b::Backend) n a) => ACCVector (b::Backend) n a -> [a]
    -- runAccMatrix :: (ValidACCMatrix (b::Backend) v r, A.IsScalar a, A.Elt a) => ACCMatrix (b::Backend) v n m a -> [a]

instance ValidBackend 'Interpreter where
    runAccVector (ACCVector a) =  A.toList (I.run a)
    -- runAccMatrix (ACCMatrix a) =  A.toList (I.run a)

instance ValidBackend 'CUDA where
    runAccVector (ACCVector a) = A.toList (CUDA.run a)
    -- runAccMatrix (ACCMatrix a) = A.toList (CUDA.run a)

-- instance ValidBackend LLVM where
--     runAccVector (ACCVector a) = A.toList (LLVM.runArray a)
--     runAccMatrix (ACCMatrix a) = A.toList (LLVM.runArray a)


--------------------------------------------------------------------------------

-- | Accelerate based Vector
-- | A.Acc is an accelreate computation, A.Array A.DIM1 a is a one dimensional array

newtype ACCVector (bknd::Backend) (n::Nat) a = ACCVector (A.Acc (A.Array A.DIM1 a))

type instance Scalar (A.Acc(A.Scalar r)) = A.Acc(A.Scalar r)
type instance Scalar (ACCVector bknd n r) =  Scalar (A.Acc(A.Scalar r))
type instance Logic (A.Acc(A.Scalar r)) = A.Acc(A.Scalar Bool)
type instance Logic (ACCVector bknd n r) = Logic (A.Acc(A.Scalar r))

type ValidACCVector bknd n a = (
                                 Prim a
                                , A.Elt a
                                , P.Num (A.Exp a)
                                --, Scalar (A.Acc (A.Scalar a)) ~ A.Acc (A.Scalar a)
                                , Ring (A.Acc (A.Scalar a))
                                --, Logic (Logic (A.Acc (A.Scalar Bool))) ~ Logic (A.Acc (A.Scalar Bool))
                                , Container (A.Acc (A.Scalar Bool))
                                , Boolean (A.Acc (A.Scalar Bool))
                                , Ord (A.Acc (A.Scalar a))
                                , Normed (A.Acc (A.Scalar a))
                                , Vector (ACCVector bknd n a)
                                , Vector (Square (ACCVector bknd n a))

                                , Semigroup (A.Exp a)
                                , Field (A.Exp a)
                                , Rg (A.Exp a)
                                -- , Actor (A.Acc (A.Scalar a)) ~ A.Acc (A.Scalar a)
                                -- , Container (A.Acc (A.Scalar a))
                                -- , Container (Logic (A.Acc (A.Scalar Bool)))
                                -- , Boolean (Logic (A.Acc (A.Scalar Bool)))
                                -- , Logic (Logic (A.Acc (A.Scalar Bool))) ~  Logic (A.Acc (A.Scalar Bool))
                                -- , Logic (A.Acc (A.Scalar Bool)) ~ A.Acc (A.Scalar Bool)
                                -- , Elem (A.Acc (A.Scalar a)) ~ A.Acc (A.Scalar a)
                                -- , P.Fractional (A.Exp a)
                                -- , P.Floating (A.Exp a)
                                -- , P.Floating (A.Acc (A.Scalar a))
                                --, P.Floating (A.Acc (A.Array A.DIM0 a))
                                -- , Elem (Square (ACCVector bknd n a)) ~ ACCVector bknd n a
                                -- , Index (Square (ACCVector bknd n a)) ~ A.Acc (A.Scalar Int)
                                -- , Index (A.Acc (A.Scalar Int)) ~  A.Acc (A.Scalar Int)
                                -- , Vector (Square (ACCVector bknd n a))
                                -- , Transposable (Square (ACCVector bknd n a))
                                -- , IxContainer (Square (ACCVector bknd n a))
                                -- , FreeModule a
                                -- , Vector (ACCVector bknd n a)

                                )

type instance Index (ACCVector bknd n r) =  A.Acc(A.Scalar Int)
type instance Elem (ACCVector bknd n r) =  Scalar (A.Acc(A.Scalar r))

type instance Actor (ACCVector (bknd::Backend) n r) = Scalar (A.Acc(A.Scalar r))

instance (KnownNat n, Prim a) => IsMutable (ACCVector (bknd::Backend) (n::Nat) a)

instance (KnownNat n, Monoid r, ValidACCVector b n r) => Semigroup (ACCVector (b::Backend) (n::Nat) r) where
    {-# INLINE (+)  #-}
    (+) :: ACCVector bknd n r -> ACCVector bknd n r -> ACCVector bknd n r
    (+) (ACCVector v1) (ACCVector v2)=ACCVector (A.zipWith (P.+) v1 v2)

instance (Semigroup (A.Acc (A.Scalar r)), KnownNat n, ValidACCVector bknd n r, Action r, Semigroup r, Prim r) => Action (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (.+)   #-}
    (.+) (ACCVector v) r = ACCVector (A.map (A.+ (A.the r)) v)

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

instance ( KnownNat n, FreeModule r, ValidACCVector bknd n r) => FreeModule (ACCVector (bknd::Backend)  (n::Nat) r) where
    {-# INLINE (.*.)   #-}
    (.*.) (ACCVector a1) (ACCVector a2) = ACCVector( A.zipWith (P.*) a1 a2)

instance (KnownNat n, Vector r, P.Fractional (A.Exp r), ValidACCVector bknd n r) => Vector (ACCVector (bknd::Backend) (n::Nat) r) where
  {-# INLINE (./)   #-} ;  (./)  (ACCVector  v) r  =  ACCVector $ A.map (P./ (A.the r)) v
  {-# INLINE (./.)  #-} ;  (./.)   (ACCVector a1) (ACCVector a2)   = ACCVector $ (A.zipWith (P./) a1 a2)

instance ( KnownNat n, Module r, ValidACCVector bknd n r) => Module (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (.*)   #-}
    (.*) (ACCVector  v) r = ACCVector (A.map (P.* (A.the r)) v)

-- instance (Field  (A.Acc (A.Scalar r)), KnownNat n, VectorSpace r, ValidACCVector bknd n r) => VectorSpace (ACCVector (bknd::Backend) (n::Nat) r) where
--     {-# INLINE (./)   #-}
--     (./) (ACCVector  v) r = ACCVector (A.map (P./ (A.the r)) v)
--
--     {-# INLINE (./.)  #-}
--     (./.) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (P./) a1 a2)

instance (KnownNat n, FreeModule r, ValidACCVector b n r) => FiniteModule (ACCVector b (n::Nat) r)
--dim wants an Int but here gets an A.Exp Int.  I tried changing the signiture to a generic type in Alegbra.hs but that produced numerous errors.
  where
    -- dim :: ACCVector b (n::Nat) r -> A.Exp Int
    -- {-# INLINE dim #-}
    -- dim (ACCVector v) = A.size v


instance
    (
    Eq r
    , A.Eq r
    , Monoid r
    , ValidACCVector b n r
    , KnownNat n
    , Eq (ACCVector b n r)
    , FreeModule r
    ) => IxContainer (ACCVector b (n::Nat) r)
        where

    {-# INLINE (!) #-}
    (!) (ACCVector v) i = A.unit (v A.! A.index1 (A.the (i)))

    -- {-# INLINABLE imap #-}
    -- -- imap f (ACCVector v) = A.zipWith (\i x -> f ((A.unit i)::A.Acc (A.Scalar Int)) ((A.unit x)::A.Acc (A.Scalar r))) ((A.generate (A.shape v) P.id):: A.Array A.DIM1 Int) v
    -- imap f (ACCVector v) = let
    --   mapd = A.imap (\x (i::A.Exp r) -> let A.Z A.:. idx = A.unlift x -- This dance is probably not optimal but f expects A.Scalars so we have to build them
    --     in A.the (f  ((A.unit i) :: Index (ACCVector b n r)) (x ! idx))) v
    --   in ACCVector mapd

    type ValidElem (ACCVector b n r) e = (ClassicalLogic e, ValidScalar e, FiniteModule e, ValidACCVector b n e)

instance  (A.Eq r, KnownNat n, Eq r, Monoid r, ValidACCVector b n r) => Eq (ACCVector b (n::Nat) r) where
--     {-# INLINE (==) #-}
--     (ACCVector v2) == (ACCVector v1) = let
--       l = A.zipWith (\x y -> x A.==* y) v1 v2
--       ele = l A.! A.index1 (A.constant 0)
--       bl = A.all (A.&&* ele) l
--       in bl


instance
    ( ValidACCVector b n r
    , A.Eq r
    , ExpField r
    , Ord r
    , Eq (ACCVector b n r)
    -- , VectorSpace r
    , P.Floating (A.Acc (A.Scalar r))
    , KnownNat n
    ) => Metric (ACCVector b (n::Nat) r)

        where
    {-# INLINE[2] distance #-}
    distance (ACCVector v1) (ACCVector v2) = {-# SCC distance_ACCVector #-}let
      dmag = A.zipWith (P.-) v1 v2
      dsq = A.zipWith (P.*) dmag dmag
      drt = A.sqrt (A.sum dsq)
      in drt

instance (KnownNat n, P.Floating (A.Acc (A.Scalar r)), ValidACCVector b n r, ExpField r) => Normed (ACCVector b (n::Nat) r) where
    {-# INLINE size #-}
    size (ACCVector v1) = let
      sq = A.zipWith (P.*) v1 v1
      s = A.fold (P.+) (A.constant 0.0) sq
      srt = A.sqrt s
      in srt

instance
    ( A.Eq r
    , Normed r
    , ValidACCVector b n r
    , ExpField r
    , Real r
    , Eq (ACCVector b n r)
    , Ord r
    , P.Floating (A.Acc (A.Scalar r))
    , KnownNat n
    ) => Banach (ACCVector b (n::Nat) r)

-- instance
--     ( FiniteModule (ACCVector b (n::Nat) r)
--     , VectorSpace (ACCVector b (n::Nat) r)
--     , Normed (ACCVector b n r +> ACCVector b n r)
--     , KnownNat n
--     , MatrixField r
--     ) => TensorAlgebra (ACCVector b (n::Nat) r)
--         where
--     (ACCVector v1)><(ACCVector v2) = let
--       r = A.size v1
--       c = A.size v2
--       arr = A.map (\i -> A.lift (A.map (\j -> i * j ) v1)) v2
--       m = A.reshape (A.index2 r c) arr :: ACCVector bknd n r +> ACCVector bknd m r
--       in m

instance
    (  ValidACCVector b n r
    , Transposable (Square (ACCVector b n r))
    , IxContainer (Square (ACCVector b n r))
    , FreeModule r
    , ExpField r
    , Eq (ACCVector b n r)
    , Real r
    , A.Eq r
    , OrdField r
    , Index (Square (ACCVector b n r)) ~ A.Acc (A.Scalar Int)
    , MatrixField r
    , KnownNat n
    , P.Floating (A.Acc (A.Scalar r))
    , P.Num r
    ,  Elem (Square (ACCVector b n r)) ~ ACCVector b n r
    ) => Hilbert (ACCVector b (n::Nat) r)
    where
    {-# INLINE (<>) #-}
    (<>) (ACCVector v1) (ACCVector v2) = let
      singlton = A.fold (+) 0.0 (A.zipWith (*) v1 v2) --This float-valued accumulator forces a Field (A.Exp r) constraint above.  Is there a way to formulate the constraints such that a more general zero-value could be used?
      in singlton

    type Square (ACCVector b n r) = ACCVector b n r +> ACCVector b n r

instance (Show r, ValidBackend b, ValidACCVector (b::Backend) n r, KnownNat n) => Show (ACCVector (b::Backend) n r) where
    show v = show (runAccVector v)


type MatrixField r =
    (
     Field r
    )
--
-- instance ValidACCVector (b::Backend) n a => Rg (a +> a) where
--     (*) = (>>>)

-- instance ValidACCVector (b::Backend) n a => Rig (a +> a)
-- where
--     one = Id_ one

-- instance ValidACCVector (b::Backend) n a => Ring (a +> a)
-- where
--     fromInteger i = Id_ $ fromInteger i

-- instance ValidACCVector (b::Backend) n a=> Field (a +> a)
-- where
--     fromRational r = Id_ $ fromRational r
--
--     reciprocal Zero = undefined
--     reciprocal (Id_ r ) = Id_ $ reciprocal r
--     reciprocal (Mat_ m) = Mat_ $ HM.inv m

-- instance (ValidACCVector (b::Backend) n a, ValidACCVector (b::Backend) n a) => Module (a +> b)
-- where
--     Zero     .* _  = Zero
--     (Id_ r1) .* r2 = Id_ $ r1*r2
--     (Mat_ m) .* r2 = Mat_ $ HM.scale r2 m

-- instance (ValidACCVector (b::Backend) n a, ValidACCVector (b::Backend) n a) => FreeModule (a +> b)
-- where
--     Zero      .*. _         = Zero
--     _         .*. Zero      = Zero
--     (Id_  r1) .*. (Id_  r2) = Id_ $ r1*r2
--     (Id_  r ) .*. (Mat_ m ) = Mat_ $ HM.scale r (HM.ident (HM.rows m)) P.* m
--     (Mat_ m ) .*. (Id_  r ) = Mat_ $ m P.* HM.scale r (HM.ident (HM.rows m))
--     (Mat_ m1) .*. (Mat_ m2) = Mat_ $ m1 P.* m2

-- instance (ValidACCVector (b::Backend) n a, ValidACCVector (b::Backend) n a) => Vector (a +> b)
-- where
--     Zero      ./. _         = Zero
--     (Id_  _) ./. Zero = undefined
--     (Mat_  _) ./. Zero = undefined
--     (Id_  r1) ./. (Id_  r2) = Id_ $ r1/r2
--     (Id_  r ) ./. (Mat_ m ) = Mat_ $ (HM.scale r (HM.ident (HM.rows m))) P./ m
--     (Mat_ m ) ./. (Id_  r ) = Mat_ $ m P./ HM.scale r (HM.ident (HM.rows m))
--     (Mat_ m1) ./. (Mat_ m2) = Mat_ $ m1 P./ m2
