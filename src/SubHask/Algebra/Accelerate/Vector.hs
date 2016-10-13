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

-- import qualified Data.Array.Accelerate.CUDA as CUDA
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

-- instance ValidBackend 'CUDA where
--     runAccVector (ACCVector a) = A.toList (CUDA.run a)
    -- runAccMatrix (ACCMatrix a) = A.toList (CUDA.run a)

-- instance ValidBackend LLVM where
--     runAccVector (ACCVector a) = A.toList (LLVM.runArray a)
--     runAccMatrix (ACCMatrix a) = A.toList (LLVM.runArray a)

--------------------------------------------------------------------------------
--A.Exp Instances
instance (Prim r) => IsMutable (A.Exp r )

instance (Monoid r, Prim r ) => Semigroup (A.Exp r )
instance (Prim r, FreeModule r) => Field (A.Exp r )
instance (Prim r, Monoid r, Cancellative r) => Cancellative (A.Exp r )
instance (Semigroup (Actor(A.Exp r)), Action r, Semigroup r, Prim r) => Action (A.Exp r )
instance (Prim r, Monoid r) => Monoid (A.Exp r )
instance (Monoid r, Abelian r, Prim r) => Abelian (A.Exp r )
instance (Prim r, Cancellative r, Monoid r) => Group (A.Exp r )
instance (P.Num (A.Exp r ), FreeModule r, Prim r, Abelian r , Group r, Ring (Scalar r), Scalar (Scalar r) ~ Scalar r ) => Module (A.Exp r) where
  {-# INLINE (.*)   #-}
  (.*) = (A.*)
instance (Abelian r, Cancellative r, Monoid r, Prim r) => Ring(A.Exp r )
instance (Abelian r, Prim r, Monoid r) => Rig(A.Exp r )
instance (Prim r, Monoid r, Abelian r) => Rg(A.Exp r  )
instance (Prim r, P.Num (A.Exp r), P.Num r, FreeModule r) => FreeModule (A.Exp r )
instance Module (A.Exp Int) where (.*) = (A.*)
instance (P.Num (A.Exp Integer), Prim Integer) => Module (A.Exp Integer) where (.*) = (A.*)
instance Module (A.Exp Float) where (.*) = (A.*)
instance Module (A.Exp Double) where (.*) = (A.*)
-- instance (Prim (GHC.Real.Ratio Integer), P.Num (A.Exp Rational), Prim Integer) => Module (A.Exp Rational) where (.*) = (A.*)
--------------------------------------------------------------------------------
--A.Acc (A.Scalar r)  Instances
-- newtype ACCScalar r = ACCScalar (A.Acc (A.Scalar r))
instance (Prim r) => IsMutable (A.Acc (A.Scalar r))
-- instance  (Prim r) => Scalar(A.Acc (A.Scalar r))
instance (Prim r, Monoid r) => Semigroup (A.Acc (A.Scalar r))

instance (Prim r, Monoid r) => Monoid (A.Acc (A.Scalar r))
--
instance (Monoid r, Abelian r, Prim r) => Abelian (A.Acc (A.Scalar r))
instance (Scalar (Scalar (A.Acc (A.Scalar r)))
                       ~
                       Scalar (A.Acc (A.Scalar r)), Ring (Scalar (A.Acc (A.Scalar r))), Prim r, Abelian r, Group r, Ring (Scalar r), Scalar (Scalar r) ~ Scalar r) => Module (A.Acc (A.Scalar r)) where

instance ( Prim r, Cancellative r, Monoid r) => Group (A.Acc (A.Scalar r))
instance (Prim r, Monoid r, Cancellative r) => Cancellative (A.Acc (A.Scalar r))
instance (Prim r, FreeModule r) => Ring(A.Acc (A.Scalar r))
instance (Prim r, FreeModule r) => Rig(A.Acc (A.Scalar r))
instance (Prim r, FreeModule r) => Rg(A.Acc (A.Scalar r))
instance (Prim r, FreeModule r) => Field (A.Acc (A.Scalar r))
--------------------------------------------------------------------------------

-- | Accelerate based Vector
-- | A.Acc is an accelreate computation, A.Array A.DIM1 a is a one dimensional array

newtype ACCVector (bknd::Backend) (n::Nat) a = ACCVector (A.Acc (A.Array A.DIM1 a))

type instance Scalar (A.Acc(A.Scalar Int)) = A.Acc(A.Scalar Int)
type instance Scalar (A.Acc(A.Scalar Float)) = A.Acc(A.Scalar Float)
type instance Scalar (A.Acc(A.Scalar Double)) = A.Acc(A.Scalar Double)
type instance Scalar (A.Acc(A.Scalar Rational)) = A.Acc(A.Scalar Rational)
type instance Scalar (A.Exp r) = A.Exp r
type instance Actor (A.Exp r) = Scalar (A.Exp r)
type instance Logic (A.Exp r) = A.Exp Bool

-- type instance Scalar (A.Acc(A.Scalar r)) = ACCExp r
type instance Scalar (ACCVector bknd n r) = Scalar (A.Exp r)

-- type instance Logic (A.Acc(A.Scalar r)) = A.Acc(A.Scalar Bool)
type instance Logic (ACCVector bknd n r) = Logic (A.Exp r)

type ValidACCVector bknd n a = (
                                 Prim a
                                , A.Elt a
                                , P.Num (A.Exp a)
                                -- , P.Fractional (A.Exp a)
                                -- , P.Floating (A.Exp a)
                                -- , P.Num (A.Acc(A.Scalar a))
                                -- , P.Fractional (A.Acc(A.Scalar a))
                                -- , P.Floating (A.Acc(A.Scalar a))
                                )

type instance Index (ACCVector bknd n r) =  Scalar (A.Exp Int)
type instance Elem (ACCVector bknd n r) =  Scalar (A.Exp r)

type instance Actor (ACCVector (bknd::Backend) n r) = Scalar (A.Exp r)


instance (Arbitrary r, ValidACCVector b n r, FreeModule r, ValidScalar r) => Arbitrary (ACCVector (bknd::Backend) n r)
-- where
-- arbitrary = frequency
--     [ (1,return zero)
--     , (9,fmap unsafeToModule $ replicateM 27 arbitrary)
--     ]
-- instance NFData (ACCVector (bknd::Backend) n r)
-- where
--     rnf (SVector_Dynamic fp _ _) = seq fp ()


-- instance (FromField r, ACCVector (bknd::Backend) n r, ValidScalar r, FreeModule r) => FromRecord (ACCVector (bknd::Backend) n r)
--   where
--     parseRecord r = do
--         rs :: [r] <- parseRecord r
--         return $ mkAccVectorFromList rs


instance (KnownNat n, Prim a) => IsMutable (ACCVector (bknd::Backend) (n::Nat) a)

instance (KnownNat n, Monoid r, ValidACCVector b n r) => Semigroup (ACCVector (b::Backend) (n::Nat) r) where
    {-# INLINE (+)  #-}
    (+) (ACCVector v1) (ACCVector v2)=ACCVector (A.zipWith (A.+) v1 v2)

instance ( Semigroup (A.Exp r), Monoid r, KnownNat n, ValidACCVector bknd n r, Action r, Semigroup r, Prim r) => Action (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (.+)   #-}
    (.+) (ACCVector v) r = ACCVector (A.map ((A.+) r) v)

instance (KnownNat n, Monoid r, Cancellative r, ValidACCVector bknd n r) => Cancellative (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (-)  #-}
    (-) (ACCVector a1) (ACCVector a2) = ACCVector (A.zipWith (A.-) a1 a2)

--The zero method wants a Ring r in the case where zero is the integer "0"
--or Field r in the case of "0.0"
--In either case, the Group instance wants the same constraint. Not exactly sure how to handle this.
instance (KnownNat n, Monoid r, ValidACCVector bknd n r) => Monoid (ACCVector (bknd::Backend) (n::Nat) r)
  -- where
--     {-# INLINE zero #-}
--     zero = ACCVector(A.use (A.fromList (A.Z A.:.1) [(0::r)]))

instance (KnownNat n, Group r, ValidACCVector bknd n r) => Group (ACCVector (bknd::Backend) (n::Nat) r)
  where
    {-# INLINE negate #-}
    negate = negate

instance (KnownNat n, Monoid r, Abelian r, ValidACCVector bknd n r) => Abelian (ACCVector (bknd::Backend)  (n::Nat) r)

instance (Rg (A.Exp r), Ring (A.Exp r), KnownNat n, FreeModule r, ValidACCVector bknd n r) => FreeModule (ACCVector (bknd::Backend)  (n::Nat) r) where
    {-# INLINE (.*.)   #-}
    (.*.) (ACCVector a1) (ACCVector a2) = ACCVector( A.zipWith (A.*) a1 a2)

instance (Ring (A.Acc (A.Scalar r)), P.Fractional (A.Exp r), Ring (A.Exp r),Field  (A.Exp r), KnownNat n, Vector r, ValidACCVector bknd n r) => Vector (ACCVector (bknd::Backend) (n::Nat) r) where
  {-# INLINE (./)   #-} ;  (./)  (ACCVector  v) r  =  ACCVector $ A.map ((A./) r) v
  {-# INLINE (./.)  #-} ;  (./.)   (ACCVector a1) (ACCVector a2)   = ACCVector $ (A.zipWith (A./) a1 a2)

instance (Rg (A.Exp r), Ring(A.Exp r), KnownNat n, Module r, ValidACCVector bknd n r) => Module (ACCVector (bknd::Backend) (n::Nat) r) where
    {-# INLINE (.*)   #-}
    (.*) (ACCVector  v) r = ACCVector (A.map ((A.*) r) v)

instance (Ring (A.Exp r), Rg (A.Exp r), Ring (A.Acc (A.Scalar r)), KnownNat n, FreeModule r, ValidACCVector b n r) => FiniteModule (ACCVector b (n::Nat) r)
--dim wants an Int but here gets an A.Exp Int.  I tried changing the signiture to a generic type in Alegbra.hs but that produced numerous errors.
  -- where
  --   {-# INLINE dim #-}
  --   dim (ACCVector v) = A.size v


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
    (!) (ACCVector v) i =  (v A.! A.index1  i)

    {-# INLINABLE imap #-}
    -- imap f (ACCVector v) = A.zipWith (\i x -> f ((A.unit i)::A.Acc (A.Scalar Int)) ((A.unit x)::A.Acc (A.Scalar r))) ((A.generate (A.shape v) P.id):: A.Array A.DIM1 Int) v
    imap f (ACCVector v) = ACCVector $ A.imap (\i x ->let
        A.Z A.:. idx = A.unlift i
      in  f idx x

      )  v


    type ValidElem (ACCVector b n r) e = (ClassicalLogic e, ValidScalar e, FiniteModule e, ValidACCVector b n e)

instance  (Ord (A.Exp r), A.Eq r, Boolean (A.Exp Bool), Container (A.Exp Bool), KnownNat n, Eq r, Monoid r, ValidACCVector b n r) => Eq (ACCVector b (n::Nat) r) where
    {-# INLINE (==) #-}
    (ACCVector v2) == (ACCVector v1) = let
      l = A.zipWith (\x y -> x A.==* y) v1 v2
      ele = l A.! A.index1 (A.constant 0)
      bl = A.all (A.&&* ele) l
      in A.the bl


instance
    ( ValidACCVector b n r
    , A.Eq r
    , Ord (A.Exp r)
    , Normed (A.Acc (A.Scalar r))
    , ExpField r
    , Ord r
    , Eq (ACCVector b n r)
    , KnownNat n
    , Ring (A.Exp r)
    , P.Num r
    , Normed (A.Exp r)
   , ExpField (A.Acc (A.Scalar r))
    ) => Metric (ACCVector b (n::Nat) r)

        where
    {-# INLINE[2] distance #-}
    distance (ACCVector v1) (ACCVector v2) = {-# SCC distance_ACCVector #-}let
      dmag = A.zipWith (A.-) v1 v2
      dsq = A.zipWith (A.*) dmag dmag
      drt = sqrt (A.sum dsq)
      in A.the drt

instance (Ord (A.Exp r), Ring (A.Exp r), ExpField (A.Acc (A.Scalar r)), Rg (A.Exp r), Ord (A.Acc (A.Scalar r)), Ring (A.Acc (A.Scalar r)), KnownNat n, ValidACCVector b n r, ExpField r) => Normed (ACCVector b (n::Nat) r) where
    {-# INLINE size #-}
    size (ACCVector v1) = let
      sq = A.zipWith (A.*) v1 v1
      s = A.fold (A.+) (A.constant 0.0) sq
      srt = sqrt s
      in A.the srt

instance
    ( A.Eq r
    , Normed r
    , Normed (A.Exp r)
    , Ord (A.Exp r)
    , Ring (A.Exp r)
    , Field (A.Exp r)
    , ValidACCVector b n r
    , ExpField r
    , Vector r
    , Ord (A.Acc (A.Scalar r))
    , Real r
    , P.Num r
    , ExpField (A.Acc (A.Scalar r))
    , Rg (A.Exp r)
    , Normed (A.Acc (A.Scalar r))
    , P.Fractional (A.Exp r)
    , Field (A.Acc (A.Scalar r))
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
    , Normed (A.Acc (A.Scalar r))
    , Ord (A.Exp r)
    , Ord (A.Acc (A.Scalar r))
    , FreeModule r
    , ExpField (A.Acc (A.Scalar r))
    , Ring (A.Acc (A.Scalar r))
    , P.Floating (A.Acc (A.Scalar r))
    , ExpField r
    , P.Fractional (A.Exp r)
    , Normed (A.Exp r)
    , Eq (ACCVector b n r)
    , Real r
    , A.Eq r
    , Vector r
    , OrdField r
    , MatrixField r
    -- , Field (A.Acc (A.Scalar r))
    , KnownNat n
    , Field (A.Exp r)
    , P.Num r
    ,  Elem (Square (ACCVector b n r)) ~ ACCVector b n r
    ) => Hilbert (ACCVector b (n::Nat) r)
    where
    {-# INLINE (<>) #-}
    (<>) (ACCVector v1) (ACCVector v2) = let
      s = A.sum (A.zipWith (A.*) v1 v2)
      in A.the s

    type Square (ACCVector b n r) = ACCVector b n r +> ACCVector b n r

instance (Show r, ValidBackend b, ValidACCVector (b::Backend) n r, KnownNat n) => Show (ACCVector (b::Backend) n r) where
    show v = show (runAccVector v)


type MatrixField r =
    (
     Field r
    )
