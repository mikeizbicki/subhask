{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}

module SubHask.Algebra.Accelerate.Matrix
    (
    ValidMatrix
    , ACCMatrix (..)
    , ValidACCMatrix
    , ACCMatrix'(..)
    , mmult
    , transpose
    , row
    , col
    , (!!)
    , mkAccMatrixFromList
    , mkAccMatrixFromMatrix
    )
    where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Interpreter as I
import SubHask.Algebra.Accelerate.Vector (ACCVector)
import SubHask.Algebra.Accelerate.AccelerateBackend (Backend)

import Data.Primitive hiding (sizeOf)
import Control.Monad.Primitive
import Control.Monad

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude


import qualified Prelude as P


newtype ACCMatrix (bknd::Backend) vect (m::k) (n::k) a = ACCMatrix (A.Acc (A.Array A.DIM2 a))

type ValidACCMatrix b vect m n r =
  ( FiniteModule vect
  , r ~ Scalar (Elem vect)
  , Hilbert vect
  , VectorSpace r
  , Prim r
  , A.Elt r -- A.Elt is the scalar element type; I don't know if this shoud be here but the instance signitures below seem to want it.
  , P.Num r
  , P.Num (A.Exp r)
  )

type instance Scalar (ACCMatrix b v m n r) = Scalar r
type instance ACCMatrix b v m n r ><r = ACCMatrix b v m n r

type instance Logic (ACCMatrix b v m n r) = Logic r
type instance Index (ACCMatrix b v m n r) = Int
type instance Elem (ACCMatrix b v m n r) = Scalar r
type instance SetElem (ACCMatrix b v m n r) a = ACCMatrix b v m n r

instance Prim r => IsMutable (ACCMatrix bknd v m n r)



{-# INLINE rowLength #-}
rowLength :: ACCMatrix b v m n r -> Integer
rowLength arr = snd (A.arrayShape arr)
{-# INLINE colLength #-}
colLength :: ACCMatrix b v m n r -> Integer
colLength arr = fst (A.arrayShape arr)
{-# INLINE (!!) #-}
(!!) (ACCMatrix v) (i,j) = v A.! A.index2 (i,j)


-- algebra
instance
  (P.Num (A.Exp r), Prim r, Monoid r, ValidACCMatrix b v m n r) =>
  Semigroup (ACCMatrix b v m n r) where
    {-# INLINE (+)  #-}
    (+) (ACCMatrix m1) (ACCMatrix m2) = ACCMatrix (A.zipWith (P.+) m1 m2)

instance
  (P.Num (A.Exp r), Monoid r, Cancellative r, Prim r, ValidACCMatrix b v m n r)
  => Cancellative (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (-)  #-} ;
    (-) (ACCMatrix m1) (ACCMatrix m2) = ACCMatrix (A.zipWith (P.-) m1 m2)

--Need the correct dim for this fill
instance
  (P.Num (A.Exp r), Monoid r, Prim r, ValidACCMatrix b v m n r) =>
  Monoid (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE zero #-}
    zero = ACCMatrix (A.fill (A.index2 2 4) 0)

instance
  (Monoid r, Abelian r, Prim r, ValidACCMatrix b v m n r) =>
  Abelian (ACCMatrix b v (n::Symbol) (m::Symbol) r)

instance
  (Module r, Prim r, ValidACCMatrix b v m n r) =>
  Module (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (.*)   #-} ;  (.*)  (ACCMatrix v) r = ACCMatrix( A.map (\x -> x P.* A.constant r) v)

type instance Actor (ACCMatrix b v (n::Symbol) (m::Symbol) r) = Actor r


instance -- had to add Monoid r, to this instance
  (P.Num (A.Exp r), Group r, Prim r, ValidACCMatrix b v m n r) =>
  Group (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE negate #-}
    negate (ACCMatrix v) =  ACCMatrix( A.map (\x -> x P.* A.constant (P.negate 1)) v)

--Could not deduce (r ~ Actor r)
instance
  (Action r, Semigroup r, Prim r, ValidACCMatrix b v m n r) =>
  Action (ACCMatrix b v (n::Symbol) (m::Symbol) r)
   where
    {-# INLINE (.+) #-}
    (.+) (ACCMatrix v) r = ACCMatrix( A.map (\x -> x P.+ A.constant r) v)

instance
  (FreeModule r, Prim r, ValidACCMatrix b v m n r) =>
  FreeModule (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (.*.) #-}
    (.*.) (ACCMatrix v1) (ACCMatrix v2) = ACCMatrix(A.zipWith (P.*) v1 v2)
    ones = undefined

instance
  (P.Fractional (A.Exp r), VectorSpace r, Prim r, ValidACCMatrix b v m n r) =>
  VectorSpace (ACCMatrix b v (n::Symbol) (m::Symbol) r) where
    {-# INLINE (./) #-}
    (./)  (ACCMatrix v) r = ACCMatrix(A.map (\x -> x A./ (A.constant r)) v)
    {-# INLINE (./.) #-}
    (./.) (ACCMatrix v1) (ACCMatrix v2) = ACCMatrix(A.zipWith (A./) v1 v2)

instance
  (ValidACCMatrix b v m n r, Monoid r, ValidLogic r, Prim r, IsScalar r)
  => IxContainer (ACCMatrix b v (n::Symbol) (m::Symbol) r) where

  {-# INLINE (!) #-}
  (!) (ACCMatrix m) i = let
    l = A.size m
    in ACCMatrix m!!(i `div` l, i `mod` l)

{-# INLINE row #-}
row :: (ValidACCMatrix b v m n r
  ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> vect
  row m i = A.slice m (A.Z A.:. (i::Int) A.:. A.All)

{-# INLINE col #-}
col ::
  ( ValidACCMatrix b v m n r
  ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> vect
col m j = A.slice m (A.Z A.:. A.All A.:. (j::Int))


--taken from http://www.cse.unsw.edu.au/~chak/papers/repa.pdf
{-# INLINE mmult #-}
mmult ::
  ( ValidACCMatrix b v m n r
  )
  => ACCMatrix b v (n::Symbol) (x0::Symbol) r
  -> ACCMatrix b v (x0::Symbol) (m::Symbol) r
  -> ACCMatrix b v (n::Symbol) (m::Symbol) r
mmult arr brr = A.sum (A.zipWith (*) arrRepl brrRepl)
  where
    trr = A.transpose brr
    arrRepl = A.replicate (A.Z A.:. A.All A.:. colsB A.:. A.All) arr
    brrRepl = A.replicate (A.Z A.:. rowsA A.:. A.All A.:. A.All) trr
    (A.Z A.:. colsA A.:. rowsA) = A.shape arr
    (A.Z A.:. colsB A.:. rowsB) = A.shape brr

{-# INLINE transpose #-}
transpose ::
  ( ValidACCMatrix b v m n r
  )
  => ACCMatrix b v (m::Symbol) (n::Symbol) r
  -> ACCMatrix b v (m::Symbol) (n::Symbol) r
transpose m = A.transpose m

data ACCMatrix' b v (m::Symbol) (n::Symbol) r where
    ACCZero ::
        (ValidACCMatrix b v m n r) =>
        ACCMatrix' b v (m::Symbol) (n::Symbol) r

    ACCId ::
        (ValidACCMatrix b v m n r) =>
        {-#UNPACK#-}!(Scalar r) -> ACCMatrix' b v (m::Symbol) (n::Symbol) r

    ACCMat ::
        (ValidACCMatrix b v m n r) =>
        {-#UNPACK#-}!(ACCMatrix b v (m::Symbol) (n::Symbol) r)
        -> ACCMatrix' b v (m::Symbol) (n::Symbol) r

type instance Scalar (ACCMatrix' b v (m::Symbol) (n::Symbol) r) = Scalar r
type instance Logic (ACCMatrix' b v (m::Symbol) (n::Symbol) r) = Bool

type instance ACCMatrix' b v (m::Symbol) (n::Symbol) r  >< n =
  ACCTensor_Linear (ACCMatrix' b v (m::Symbol) (n::Symbol) r ) n
type family ACCTensor_Linear m n where
    ACCTensor_Linear (ACCMatrix' b v (m::Symbol) (n::Symbol) r) c =
      ACCMatrix' b v (m::Symbol) (n::Symbol) r

instance Category (ACCMatrix' b v (m::Symbol) (n::Symbol) r ) where
    type ValidCategory (ACCMatrix' b v (m::Symbol) (n::Symbol) r ) o = ValidACCMatrix b v m n r

    id = Id 1

    Zero . Zero     = Zero
    Zero . (Id  _ ) = Zero
    Zero . (Mat _ ) = Zero

    (Id  _ ) . Zero     = Zero
    (Id  r1) . (Id  r2) = Id  $ r1 * r2
    (Id  r ) . (Mat m ) = Mat $ m .* r

    (Mat _) . Zero      = Zero
    (Mat m ) . (Id  r ) = Mat $ m .* r
    (Mat m1) . (Mat m2) = Mat $ mmult m2 m1

mkAccMatrixFromList :: A.Elt a => Int -> [a] -> ACCMatrix b v m n a
mkAccMatrixFromList m l = let
    ln = P.length l
    n = ln `div` m
  in  ACCMatrix (A.use (A.fromList (A.Z A.:.m A.:.n) l))

--FIXME: use accelerate-io functions https://github.com/AccelerateHS/accelerate-io/tree/master/Data/Array/Accelerate/IO
mkAccMatrixFromMatrix :: (ValidMatrix vect r, A.Elt r) =>  Matrix vect r (m::Symbol) (n::Symbol) -> ACCMatrix b v m n r
mkAccMatrixFromMatrix mat@(Matrix_Dynamic vect ln) =
  mkAccMatrixFromList cln l
  where
    cln = colLength mat
    l = P.foldr (\x xs -> vect!x : xs) [] [0..(ln * cln)-1]
