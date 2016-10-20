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
import SubHask.Algebra.Vector (SVector)
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

class ValidBackend (b::Backend) where
    runAccVector :: (ValidACCVector (b::Backend) n a) => ACCVector (b::Backend) n a -> [a]
    runAccMatrix :: (A.IsScalar a, A.Elt a) => ACCMatrix (b::Backend) n m a -> [a]

instance ValidBackend 'Interpreter where
    runAccVector (ACCVector a) =  A.toList (I.run a)
    --how to preserve the >< = a+>a interface since I.run wants a plain A.Acc?
    --runAccMatrix (ACCMatrix m) =  A.toList (I.run m)

-- instance ValidBackend 'CUDA where
--     runAccVector (ACCVector a) = A.toList (CUDA.run a)
    -- runAccMatrix (ACCMatrix a) = A.toList (CUDA.run a)

-- instance ValidBackend LLVM where
--     runAccVector (ACCVector a) = A.toList (LLVM.runArray a)
--     runAccMatrix (ACCMatrix a) = A.toList (LLVM.runArray a)

--------------------------------------------------------------------------------
--A.Exp Orphaned Instances; I'm sure there's a way to clean these up . . .
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

--------------------------------------------------------------------------------
--A.Acc (A.Scalar r)  Instances
instance (Prim r) => IsMutable (A.Acc (A.Scalar r))
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
    , Normed (A.Exp r)
    , A.Eq r
    , Ord (A.Exp r)
    , Normed (A.Acc (A.Scalar r))
    , ExpField r
    , Ord r
    , Ring (A.Exp r)
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


-- So far unable to get the dagger interface to square with the fact that any operation returns inside the A.Acc monad.
--newtype ACCMatrix b m n a = ACCMatrix( A.Acc (A.Array A.DIM2 a))--ACCVector b m a +> ACCVector b n a
newtype ACCMatrix b m n a = ACCMatrix (ACCVector b m a +> ACCVector b n a)

mkACCMatrix r c l = Mat_ $ A.use $  A.fromList (A.Z A.:. r A.:. c) l

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
    , KnownNat n
    , Field (A.Exp r)
    , IsMutable (ACCVector b n r +> ACCVector b n r)
    , P.Num r
    ,  Elem (Square (ACCVector b n r)) ~ ACCVector b n r
    ) => Hilbert (ACCVector b (n::Nat) r)
    where
    {-# INLINE (<>) #-}
    (<>) (ACCVector v1) (ACCVector v2) = let
      s = A.sum (A.zipWith (A.*) v1 v2)
      in A.the s

    type Square (ACCVector b n r) = ACCVector b n r +> ACCVector b n r
    --Does this need an unsafeCoerce to get GHC to accept that the result is ACCVector b n r + > ACCVector b n r and not the acclerate types?
    -- (ACCVector v1)><(ACCVector v2) = let
    --   r = A.size v1 
    --   c = A.size v2
    --   m = A.zipWith (A.*) (A.replicate  (A.lift $ A.Any A.:. r A.:. A.All) v1 )  (A.replicate  (A.lift $ A.Any A.:. A.All A.:. c) v2)
    --   in m

    mXv m v= m $ v
    vXm v m = trans m $ v

instance (Show r, ValidBackend b, ValidACCVector (b::Backend) n r, KnownNat n) => Show (ACCVector (b::Backend) n r) where
    show v = show (runAccVector v)


type MatrixField r =
  (ValidScalar r
  , Vector r
   , Field r
  )

class ToFromVector a where
  toVector   :: a -> ACCVector b n a
  fromVector :: ACCVector b n a -> a

instance ToFromVector Double where
  toVector x = mkAccVectorFromList [x]
  --fromVector v = A.toList v

-- These are building what from what, exactly?
-- instance MatrixField r => ToFromVector (ACCVector b (n::Symbol) r) where
--   toVector (SVector_Dynamic fp off n) = VS.unsafeFromForeignPtr fp off n
--   fromVector v = SVector_Dynamic fp off n
--       where
--           (fp,off,n) = VS.unsafeToForeignPtr v

-- instance (KnownNat n, MatrixField r) => ToFromVector (SVector (n::Nat) r) where
  --   toVector (SVector_Nat fp) = VS.unsafeFromForeignPtr fp 0 n
--       where
--           n = nat2int (Proxy::Proxy n)
--   fromVector v = SVector_Nat fp
--       where
--           (fp,_,_) = VS.unsafeToForeignPtr v

-- what is this?
-- apMat_ ::
--   ( Scalar a~Scalar b
--   , Scalar b ~ Scalar (Scalar b)
--   , MatrixField (Scalar a)
--   , ToFromVector a
--   , ToFromVector b
--   ) => HM.Matrix (Scalar a) -> a -> b
-- apMat_ m a = fromVector $ A.flatten $ m HM.<> HM.asColumn (toVector a)


data (+>) a b where
        Zero :: (Module a, Module b) => a +> b
        Id_ :: (Vector b) => !(Scalar b) -> b +> b
        Mat_ ::
            (MatrixField (Scalar b), Scalar a ~ Scalar b,
             Scalar b ~ Scalar (Scalar b), Scalar a ~ A.Exp a,
             Scalar b ~ A.Exp b, P.Fractional (A.Exp a), P.Fractional (A.Exp b),
             P.Num (A.Exp b), P.Num (A.Exp a), Prim a, Prim b, A.Elt b, A.Elt a,
             Vector a, Vector b, ToFromVector a, ToFromVector b) =>
            A.Acc (A.Array A.DIM2 b) -> a +> b

type instance Scalar (a +> b) = Scalar b
type instance Logic (a +> b) = A.Exp Bool

-- type instance (a +> b) >< c = Tensor_Linear (a +> b) c
-- type family Tensor_Linear a b where
--     Tensor_Linear (a +> b) c = a +> b

--------------------------------------------------------------------------------
-- instances

deriving instance (A.Elt b, A.Elt a, MatrixField (Scalar b), Show (Scalar b) ) => Show (a +> b)






---------------------------------------------------------------------------------
-- accelerate linear algebra helpers
-- omitted some signitures because I couldn't deduce what ghc wanted and it's likely to need refactoring anyway

--fillsqu_:: (A.Elt a, A.Num a) => (A.Exp((A.Z A.:. Int ) A.:. Int) -> A.Exp a) -> A.Exp Int -> ACCMatrix b m m a
fillsqu_ f d = A.generate (A.index2 d d) f

--ident_:: A.Exp((A.Z A.:. Int ) A.:. Int) -> A.Exp Int
ident_ d = let
  A.Z A.:. rows A.:. cols     = A.unlift d 
  in A.cond (rows A.==* cols ) (A.constant one) (A.constant zero)

identFrm_ :: (A.IsNum a , A.Elt a ) => A.Acc (A.Array A.DIM2 a) -> A.Acc (A.Array A.DIM2 Int)
identFrm_ m = fillsqu_ ident_ (fst $ matrixShape_ m)

matrixShape_ :: (A.IsNum a , A.Elt a ) => A.Acc (A.Array A.DIM2 a) -> (A.Exp Int, A.Exp Int)
matrixShape_ arr = let
    A.Z A.:. rows A.:. cols     = A.unlift (A.shape arr)
  in (rows, cols)

--multiplyMatrixMatrix_ :: (A.IsNum b , A.Elt b ) => (A.Acc (A.Array A.DIM2 b)) -> (A.Acc (A.Array A.DIM2 b)) -> (A.Acc (A.Array A.DIM2 b)) -> (A.Acc (A.Array A.DIM2 b))
multiplyMatrixMatrix_ arr brr = A.fold1 (+)  (A.zipWith (*) arrRepl brrRepl)
  where
    A.Z A.:. rowsA A.:. _     = A.unlift (A.shape arr) :: A.Z A.:. A.Exp Int A.:. A.Exp Int
    A.Z A.:. _     A.:. colsB = A.unlift (A.shape brr) :: A.Z A.:. A.Exp Int A.:. A.Exp Int

    arrRepl             = A.replicate (A.lift $ A.Z A.:. A.All   A.:. colsB A.:. A.All) arr
    brrRepl             = A.replicate (A.lift $ A.Z A.:. rowsA A.:. A.All   A.:. A.All) (A.transpose brr)


----------------------------------------
-- category

instance Category (+>) where
    type ValidCategory (+>) a = MatrixField a
    -- This needs to be an A.Exp a in order for the below binary operations to typecheck.
    -- However, I haven't been able to successfully make id an "A.Exp 1" . . .
    id = Id_ 1

    Zero      . Zero      = Zero
    Zero      . (Id_  _ ) = Zero
    Zero      . (Mat_ _ ) = Zero

    (Id_  _ ) . Zero      = Zero
    (Id_  r1) . (Id_  r2) = Id_ (r1*r2)
    (Id_  r ) . (Mat_ m ) = Mat_ $ A.map (A.* r) m

    (Mat_ _) . Zero      = Zero
    (Mat_ m ) . (Id_  r ) = Mat_ $ A.map (A.* r) m
    (Mat_ m1) . (Mat_ m2) = Mat_ $ multiplyMatrixMatrix_ m1 m2

instance Sup (+>) (->) (->)
instance Sup (->) (+>) (->)

instance (+>) <: (->) where
    embedType_ = Embed2 (embedType2 go)
        where
            go :: a +> b -> a -> b
            go Zero     = zero
            go (Id_  r) = (r*.)
            -- go (Mat_ m) = apMat_ m

instance Dagger (+>) where
    dagger Zero     = Zero
    dagger (Id_  r) = Id_ r
    dagger (Mat_ m) = Mat_ $ A.transpose m

instance Groupoid (+>) where
    inverse Zero = undefined
    inverse (Id_  r) = Id_  $ reciprocal r
    -- inverse (Mat_ m) = Mat_ $ HM.inv m

-- FIXME
type instance Elem (a +> b) = b
type instance Index (a +> b) = Index a

instance (Container (A.Exp Bool)) => Eq (a +> b)
instance (Container (A.Exp Bool)) => IxContainer (a +> b)
instance Transposable (a +> a) where
    trans = dagger

----------------------------------------
-- size

-- FIXME: what's the norm of a tensor?
instance (Ord (A.Exp r), Prim r, MatrixField r) => Normed (ACCVector b m r +> ACCVector b m r) where
    size Zero = zero
    size (Id_ r) = r
    -- size (Mat_ m) = HM.det m

----------------------------------------
-- algebra

instance (IsMutable(a +> b)) => Semigroup (a +> b) where
    Zero      + a         = a
    a         + Zero      = a
    (Id_  r1) + (Id_  r2) = Id_ (r1+r2)
    -- (Id_  r ) + (Mat_ m ) = Mat_ $ A.zipWith (+) (A.map ((*) $ A.constant r) (identFrm_ m)) m
    -- (Mat_ m ) + (Id_  r ) = Mat_ $ A.zipWith (A.+) m (A.map ((A.*) $ A.constant r) (identFrm_ m))
    (Mat_ m1) + (Mat_ m2) = Mat_ $ A.zipWith (A.+) m1 m2

instance (Vector a, Vector b, IsMutable(a +> b)) => Monoid (a +> b) where
    zero = Zero

instance (Vector a, Vector b, IsMutable(a +> b)) => Cancellative (a +> b) where
    a         - Zero      = a
    Zero      - a         = negate a
    (Id_  r1) - (Id_  r2) = Id_ (r1-r2)
    -- (Id_  r ) - (Mat_ m ) = Mat_ $ A.zipWith (A.-) (A.map ((A.*) r) (identFrm_ m)) m
    -- (Mat_ m ) - (Id_  r ) = Mat_ $ A.zipWith (A.-) m (A.map ((A.*) r) (identFrm_ m))
    (Mat_ m1) - (Mat_ m2) = Mat_ $ A.zipWith (A.-) m1 m2

instance (Vector a, Vector b, IsMutable(a +> b)) => Group (a +> b) where
    negate Zero     = Zero
    negate (Id_  r) = Id_ $ negate r
    negate (Mat_ m) = Mat_ $ A.map (A.* (-1)) m

instance (IsMutable(a +> b)) => Abelian (a +> b)

-------------------
-- modules

instance (Vector a, Vector b, IsMutable(a +> b)) => Module (a +> b) where
    Zero     .* _  = Zero
    (Id_ r1) .* r2 = Id_ $ r1*r2
    (Mat_ m) .* r2 = Mat_ $ A.map (A.* (r2)) m

instance (Vector a, Vector b, IsMutable(a +> b)) => FreeModule (a +> b) where
    Zero      .*. _          = Zero
    _         .*. Zero      = Zero
    (Id_  r1) .*. (Id_  r2) = Id_ $ r1*r2
    -- (Id_  r ) .*. (Mat_ m ) = Mat_ $ A.zipWith (A.*) (A.map ((A.*) r) (identFrm_ m)) m
    -- (Mat_ m ) .*. (Id_  r ) = Mat_ $ A.zipWith (A.*) m (A.map ((A.*) r) (identFrm_ m))
    (Mat_ m1) .*. (Mat_ m2) = Mat_ $ A.zipWith (A.*) m1 m2

instance (Vector a, Vector b, IsMutable(a +> b)) => Vector (a +> b) where
    Zero      ./. _         = Zero
    (Id_  _) ./. Zero = undefined
    (Mat_  _) ./. Zero = undefined
    (Id_  r1) ./. (Id_  r2) = Id_ $ r1/r2
    -- (Id_  r ) ./. (Mat_ m ) = Mat_ $ A.zipWith (A./) (A.map ((A.*) r) (identFrm_ m)) m
    -- (Mat_ m ) ./. (Id_  r ) = Mat_ $ A.zipWith (A./) m (A.map ((A.*) r) (identFrm_ m))
    (Mat_ m1) ./. (Mat_ m2) = Mat_ $ A.zipWith (A./) m1 m2

-------------------
-- rings
--
-- NOTE: matrices are only a ring when their dimensions are equal

instance (Vector a, IsMutable(a +> a)) => Rg (a +> a) where
    (*) = (>>>)

instance (Vector a, IsMutable(a +> a)) => Rig (a +> a) where
    one = Id_ one

instance  (Vector a, IsMutable(a +> a)) => Ring (a +> a) where
    fromInteger i = Id_ $ fromInteger i

instance  (Vector a, IsMutable(a +> a)) => Field (a +> a) where
    fromRational r = Id_ $ fromRational r

    reciprocal Zero = undefined
    reciprocal (Id_ r ) = Id_ $ reciprocal r
    -- reciprocal (Mat_ m) = Mat_ $ HM.inv m
