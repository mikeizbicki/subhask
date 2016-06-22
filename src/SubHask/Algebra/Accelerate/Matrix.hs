{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}

module SubHask.Algebra.Accelerate.Matrix
    (
    --ValidMatrix
     ACCMatrix (..)
    , ValidACCMatrix
    , ACCMatrix'(..)
    , mmult
    , transpose
    , row
    , col
    , nCols
    , nRows
    , (!!)
    , mkAccMatrixFromList
    , mkAccMatrixFromMatrix
    )
    where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Interpreter as I
import SubHask.Algebra.Accelerate.Vector (ACCVector(..))
import SubHask.Algebra.Accelerate.AccelerateBackend (Backend)

import Data.Primitive hiding (sizeOf)
import Control.Monad.Primitive
import Control.Monad

import SubHask.Algebra
import SubHask.Category
import qualified SubHask.Algebra.Matrix as M (Matrix(..), ValidMatrix, colLength)
import SubHask.Internal.Prelude


import qualified Prelude as P


newtype ACCMatrix (bknd::Backend) vect (m::k) (n::k) a = ACCMatrix (A.Acc (A.Array A.DIM2 a))

type ValidACCMatrix (bknd::Backend) vect r =
  (
  FiniteModule vect
  , r ~ Scalar (Elem vect)
  , Hilbert vect
  , VectorSpace r
  , Prim r
  , A.Elt r
  , P.Num r
  , P.Num (A.Exp r)
  , Actor r ~ A.Exp r
  , Elem r ~ A.Exp r
  , P.Integral (A.Exp Int)
  , Scalar (A.Exp r) ~ A.Exp r
  , (A.Exp r >< A.Exp r) ~ A.Exp r
  , Ring (A.Exp Int)
  , Ord_ (A.Exp r)
  , Normed(A.Exp r)
  , Ring(A.Exp r)
  , Logic(A.Exp r) ~ Bool
  , Field (A.Exp r)

  )

type instance Scalar (ACCMatrix b v m n r) = Scalar r
type instance ACCMatrix b v m n r ><r = ACCMatrix b v m n r

type instance Logic (ACCMatrix b v m n r) = A.Acc (A.Scalar Bool)
type instance Index (ACCMatrix b v m n r) = Index r
type instance Elem (ACCMatrix b v m n r) = Elem r
type instance SetElem (ACCMatrix b v m n r) a = ACCMatrix b v m n r
type instance Actor (ACCMatrix b v m n r)  = Actor (A.Exp r)


instance Prim r => IsMutable (ACCMatrix bknd v m n r)



{-# INLINE nCols #-}
nCols :: (A.Elt r) => ACCMatrix b v m n r -> A.Exp Int
nCols (ACCMatrix arr) = let
  (A.Z A.:. cols A.:. rows) = A.unlift (A.shape arr) :: (A.Z A.:. A.Exp Int A.:. A.Exp Int)
  in cols
{-# INLINE nRows #-}
nRows :: (A.Elt r) => ACCMatrix b v m n r -> A.Exp Int
nRows (ACCMatrix arr) = let
  (A.Z A.:. cols A.:. rows) = A.unlift (A.shape arr) :: (A.Z A.:. A.Exp Int A.:. A.Exp Int)
  in rows
{-# INLINE (!!) #-}
(!!) (ACCMatrix m) (i,j) = m A.! A.index2 i j


-- algebra
instance
  (Prim r, Monoid r, ValidACCMatrix b v r) =>
  Semigroup (ACCMatrix b v m n r) where
    {-# INLINE (+)  #-}
    (+) (ACCMatrix m1) (ACCMatrix m2) = ACCMatrix (A.zipWith (P.+) m1 m2)

instance
  (Monoid r, Cancellative r, Prim r, ValidACCMatrix b v r)
  => Cancellative (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (-)  #-} ;
    (-) (ACCMatrix m1) (ACCMatrix m2) = ACCMatrix (A.zipWith (P.-) m1 m2)

--Need the correct dim for this fill
--also not sure to to handle the types of the index and zero value;
--the Ring() constraints made it happy for Ints
instance
  (Monoid r, Prim r, ValidACCMatrix b v r) =>
  Monoid (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE zero #-}
    zero = ACCMatrix (A.fill (A.index2 2 4) 0)

instance
  (Monoid r, Abelian r, Prim r, ValidACCMatrix b v r) =>
  Abelian (ACCMatrix b v (n::Symbol) (m::Symbol) r)

instance
  (Module r, Prim r, ValidACCMatrix b v r) =>
  Module (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (.*)   #-}
    (.*)  (ACCMatrix v) r = ACCMatrix( A.map (\x -> x P.* A.constant r) v)

instance -- had to add Monoid r, to this instance
  (Group r, Prim r, ValidACCMatrix b v r) =>
  Group (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE negate #-}
    negate (ACCMatrix v) =  ACCMatrix( A.map (\x -> x P.* A.constant (P.negate 1)) v)

--Could not deduce (r ~ A.Exp r)
instance
  (Actor(A.Exp r) ~ (A.Exp r), Semigroup (Actor(A.Exp r)), Action r, Semigroup r, Prim r, ValidACCMatrix b v r) =>
  Action (ACCMatrix b v (n::Symbol) (m::Symbol) r)
   where
    (.+) :: ACCMatrix b v m n r -> A.Exp r -> ACCMatrix b v m n r
    {-# INLINE (.+) #-}
    (.+) (ACCMatrix v) r = ACCMatrix( A.map (P.+ r) v)

instance
  (FreeModule r, Prim r, ValidACCMatrix b v r) =>
  FreeModule (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (.*.) #-}
    (.*.) (ACCMatrix v1) (ACCMatrix v2) = ACCMatrix(A.zipWith (P.*) v1 v2)
    ones = undefined

instance
  (P.Fractional (A.Exp r), VectorSpace r, Prim r, ValidACCMatrix b v r) =>
  VectorSpace (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    (./) :: ACCMatrix b v m n r -> A.Exp r -> ACCMatrix b v m n r
    {-# INLINE (./) #-}
    (./)  (ACCMatrix v) r = ACCMatrix(A.map ( P./ r) v)
    {-# INLINE (./.) #-}
    (./.) (ACCMatrix v1) (ACCMatrix v2) = ACCMatrix(A.zipWith (A./) v1 v2)

instance
  (Index r ~ A.Exp r, Complemented (A.Acc(A.Scalar Bool)), Integral(A.Exp Int), Ring (A.Exp r), Ring (A.Exp Int), Complemented r, ValidACCMatrix b v r, Monoid r, ValidLogic r, Prim r, IsScalar r)
  => IxContainer (ACCMatrix b v (n::Symbol) (m::Symbol) r) where

  {-# INLINE (!) #-}
  (!) :: ACCMatrix b v m n r -> A.Exp r -> A.Exp r--A.Acc (Scalar r)
  (!) (ACCMatrix m) i = let
    l = A.size m
    rval = m!!(i `div` l, i `mod` l)
    in A.the rval

{-# INLINE col #-}
col :: (ValidACCMatrix b v r
  ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> ACCVector b n r
col (ACCMatrix m) i = ACCVector (A.slice m (A.lift (A.Z A.:. i A.:. A.All)))

{-# INLINE row #-}
row ::
  ( ValidACCMatrix b v r
  ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> ACCVector b m r
row (ACCMatrix m) j = ACCVector (A.slice m (A.lift (A.Z A.:. A.All A.:. j)))

--taken from http://www.cse.unsw.edu.au/~chak/papers/repa.pdf
{-# INLINE mmult #-}
mmult ::
  ( ValidACCMatrix b v r
  , Field (A.Exp r)
  )
  => ACCMatrix b v (n::Symbol) (x0::Symbol) r
  -> ACCMatrix b v (x0::Symbol) (m::Symbol) r
  -> ACCMatrix b v (n::Symbol) (m::Symbol) r
mmult (ACCMatrix arr) (ACCMatrix brr) = ACCMatrix out
  where
    trr = A.transpose brr
    (A.Z A.:. colsA A.:. rowsA) = A.unlift (A.shape arr) :: (A.Z A.:. A.Exp Int A.:. A.Exp Int)
    (A.Z A.:. colsB A.:. rowsB) = A.unlift (A.shape brr) :: (A.Z A.:. A.Exp Int A.:. A.Exp Int)
    arrRepl = A.replicate (A.lift $ A.Z A.:. A.All A.:. colsB A.:. A.All) arr
    brrRepl = A.replicate (A.lift $ A.Z A.:. rowsA A.:. A.All A.:. A.All) trr
    out = A.fold (P.+) 0.0 $ (A.zipWith (P.*) arrRepl brrRepl)


{-# INLINE transpose #-}
transpose ::
  ( ValidACCMatrix b v r
  )
  => ACCMatrix b v (m::Symbol) (n::Symbol) r
  -> ACCMatrix b v (m::Symbol) (n::Symbol) r
transpose (ACCMatrix m) = ACCMatrix (A.transpose (A.unlift m))

data ACCMatrix' b v r (m::Symbol) (n::Symbol) where
    Zero ::
        (ValidACCMatrix b v r) =>
        ACCMatrix' b v r (m::Symbol) (n::Symbol)

    Id ::
        (ValidACCMatrix b v r) =>
        {-#UNPACK#-}!(Scalar r) -> ACCMatrix' b v r (m::Symbol) (n::Symbol)

    Mat ::
        (ValidACCMatrix b v r) =>
        {-#UNPACK#-}!(ACCMatrix b v (m::Symbol) (n::Symbol) r)
        -> ACCMatrix' b v r (m::Symbol) (n::Symbol)

type instance Scalar (ACCMatrix' b v r (m::Symbol) (n::Symbol)) = Scalar r
type instance Logic (ACCMatrix' b v r (m::Symbol) (n::Symbol)) = Logic r--Bool

type instance ACCMatrix' b v r (m::Symbol) (n::Symbol) >< m =
  ACCTensor_Linear (ACCMatrix' b v r (m::Symbol) (n::Symbol)) m
type family ACCTensor_Linear m n where
    ACCTensor_Linear (ACCMatrix' b v r (m::Symbol) (n::Symbol)) c =
      ACCMatrix' b v r (m::Symbol) (n::Symbol)

instance Category (ACCMatrix' b v r) where
    type ValidCategory (ACCMatrix' b v r ) m = ValidACCMatrix b v r

    id = Id 1

    Zero . Zero     = Zero
    Zero . (Id  _ ) = Zero
    Zero . (Mat _ ) = Zero

    (Id  _ ) . Zero     = Zero
    (Id  r1) . (Id  r2) = Id  $ r1 * r2
    -- Could not deduce (b ~ c)
    --(Id  (r::Scalar r)) . (Mat (m::ACCMatrix b v (m::Symbol) (n::Symbol) r)) = Mat $ m .* A.constant r

    (Mat _) . Zero      = Zero
    --Could not deduce (b ~ a)
    --(Mat m ) . (Id  r ) = Mat $ m .* A.constant r
    --Could not deduce (b1 ~ b2)
    --(Mat m1) . (Mat m2) = Mat $ mmult m2 m1

mkAccMatrixFromList :: A.Elt a => Int -> [a] -> ACCMatrix b v m n a
mkAccMatrixFromList m l = let
    ln = P.length l
    n = ln `div` m
  in  ACCMatrix (A.use (A.fromList (A.Z A.:.m A.:.n) l))

--FIXME: use accelerate-io functions https://github.com/AccelerateHS/accelerate-io/tree/master/Data/Array/Accelerate/IO
mkAccMatrixFromMatrix :: (M.ValidMatrix vect r, A.Elt r) =>  M.Matrix vect r (m::Symbol) (n::Symbol) -> ACCMatrix b v m n r
mkAccMatrixFromMatrix mat@(M.Matrix_Dynamic vect ln) =
  mkAccMatrixFromList cln l
  where
    cln = M.colLength mat
    l = P.foldr (\x xs -> vect!x : xs) [] [0..(ln * cln)-1]
