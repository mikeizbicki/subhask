{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}

module SubHask.Algebra.Matrix
    ( Matrix (..)
    , unsafeToModuleM
    , ValidMatrix
    , ACCMatrix (..)
    , ValidACCMatrix
    , ACCMatrix'(..)
    , mmult
    , transpose
    , row
    , col
    , (!!)
    , Matrix'(..)
    , mkAccMatrixFromList
    , mkAccMatrixFromMatrix
    )
    where

import Data.Primitive hiding (sizeOf)
import Control.Monad.Primitive
import Control.Monad

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.Interpreter as I
import SubHask.Algebra.Vector (ACCVector)
import SubHask.Algebra.AccelerateBackend (Backend)
import qualified Prelude as P

data family Matrix vect r (a::k) (b::k)

type ValidMatrix vect r =
  ( FiniteModule vect
  , r ~ Scalar (Elem vect)
  , Hilbert vect
  , VectorSpace r
  , Prim r
  )

type instance Scalar (Matrix vect r m n) = Scalar r
type instance Logic (Matrix vect r m n) = Logic r
type instance Matrix vect r m n >< a = Matrix vect (r><a) m n
type instance Index (Matrix vect r m n) = Int
type instance Elem (Matrix vect r m n) = Scalar r
type instance SetElem (Matrix vect r m n) b = Matrix vect b m n

-- | matrix type
data instance Matrix vect r (a::Symbol) (b::Symbol) =
  Matrix_Dynamic
  vect
  {-#UNPACK#-}!Int -- row length (aka number of columns)

{-# INLINE rowLength #-}
rowLength :: Matrix vect r (a::Symbol) (b::Symbol) -> Int
rowLength (Matrix_Dynamic _ l) = l

{-# INLINE colLength #-}
colLength :: (ValidMatrix vect r) => Matrix vect r (a::Symbol) (b::Symbol) -> Int
colLength (Matrix_Dynamic v l) = dim v `div` l

{-# INLINE (!!) #-}
(!!) ::
  (ValidMatrix vect r) =>
  Matrix vect r (a::Symbol) (b::Symbol) -> (Int, Int) -> r
(!!) (Matrix_Dynamic vect l) (i,j) = vect!(i*l+j)

instance
  (ValidMatrix vect r, Show r) =>
  Show (Matrix vect r (a::Symbol) (b::Symbol)) where
    show m = if isZero rowLength m || isZero rowLength m
        then "zero"
        else go (rows-1) (cols-1) $ "(" ++ show rows ++ "><" ++ show cols ++ ")\n "
        where
          cols = rowLength m
          rows = colLength m
          go :: Int -> Int -> String -> String
          go (-1) _ xs = xs ++ "]"
          go i (-1) xs = go (i-1) (cols-1) (xs ++ "\n ")
          go i j xs = go i (j-1) (xs ++ (if j==(cols-1) && i==(rows-1) then "[ " else ", ") ++ show (m!!(rows-1-i,cols-1-j)))

-- | FiniteModule attempt
{-# INLINE unsafeToModuleM #-}
unsafeToModuleM :: forall vect r a b.
  (ValidMatrix vect r)
  => Int
  -> [Scalar vect]
  -> Matrix vect r (a::Symbol) (b::Symbol)
unsafeToModuleM l xs = Matrix_Dynamic (unsafeToModule xs) l

---------------------------------------
-- mutable

newtype instance Mutable m' (Matrix vect r (a::Symbol) (b::Symbol))
    = Mutable_Matrix (PrimRef m' (Matrix vect r (a::Symbol) (b::Symbol)))

instance Prim r => IsMutable (Matrix vect r (a::Symbol) (b::Symbol)) where

    freeze mv = copy mv >>= unsafeFreeze
    thaw v = unsafeThaw v >>= copy

    unsafeFreeze (Mutable_Matrix ref) = readPrimRef ref
    unsafeThaw v = do
        ref <- newPrimRef v
        return $ Mutable_Matrix ref

    write (Mutable_Matrix ref) m = writePrimRef ref m

{-# INLINE monopDyn #-}
monopDyn :: forall vect r a b.
    ( ValidMatrix vect r
    )
    => (r -> r)
    -> Matrix vect r (a::Symbol) (b::Symbol)
    -> Matrix vect r (a::Symbol) (b::Symbol)
monopDyn f m@(Matrix_Dynamic vect l) = if l==0
    then m
    else Matrix_Dynamic (unsafeToModule [f (vect!i) | i <- [0..(dim vect - 1)]]) l

{-# INLINE binopDyn #-}
binopDyn :: forall vect r (a::Symbol) (b::Symbol).
    ( ValidMatrix vect r
    , Monoid r
    )
    => (r -> r -> r)
    -> Matrix vect r (a::Symbol) (b::Symbol)
    -> Matrix vect r (a::Symbol) (b::Symbol)
    -> Matrix vect r (a::Symbol) (b::Symbol)
binopDyn f m1@(Matrix_Dynamic vect1 l1) m2@(Matrix_Dynamic vect2 l2) = if
  | isZero l1 -> m2
  | isZero l2 -> m1
  | otherwise ->
      Matrix_Dynamic
      (unsafeToModule
       [ f (vect1!i) (vect2!i)
       | i <- [0..(dim vect1 - 1)]
       ])
      l1

-- algebra
instance
  (Prim r, Monoid r, ValidMatrix vect r) =>
  Semigroup (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (+)  #-} ; (+)  = binopDyn  (+)

instance
  (Monoid r, Cancellative r, Prim r, ValidMatrix vect r)
  => Cancellative (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (-)  #-} ; (-)  = binopDyn  (-)

instance
  (Monoid r, Prim r, ValidMatrix vect r) =>
  Monoid (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE zero #-}
    zero = unsafeInlineIO $ do
        let vect = unsafeToModule []
        return $ Matrix_Dynamic vect 0

instance
  (Group r, Prim r, ValidMatrix vect r) =>
  Group (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE negate #-}
    negate v = monopDyn negate v

instance
  (Monoid r, Abelian r, Prim r, ValidMatrix vect r) =>
  Abelian (Matrix vect r (a::Symbol) (b::Symbol))

instance
  (Module r, Prim r, ValidMatrix vect r) =>
  Module (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (.*)   #-} ;  (.*)  v r = monopDyn  (.*r) v

type instance Actor (Matrix vect r (a::Symbol) (b::Symbol)) = Actor r

instance
  (Action r, Semigroup r, Prim r, ValidMatrix vect r) =>
  Action (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (.+) #-}
    (.+) v r = monopDyn (.+r) v

instance
  (FreeModule r, Prim r, ValidMatrix vect r) =>
  FreeModule (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (.*.) #-}
    (.*.) = binopDyn (.*.)
    ones = undefined

instance
  (VectorSpace r, Prim r, ValidMatrix vect r) =>
  VectorSpace (Matrix vect r (a::Symbol) (b::Symbol)) where
    {-# INLINE (./) #-} ;  (./)  v r = monopDyn  (./r) v
    {-# INLINE (./.) #-} ;  (./.)     = binopDyn  (./.)

----------------------------------------
-- container

instance
  (ValidMatrix vect r, Monoid r, ValidLogic r, Prim r, IsScalar r)
  => IxContainer (Matrix vect r (a::Symbol) (b::Symbol)) where

  {-# INLINE (!) #-}
  (!) m@(Matrix_Dynamic _ l) i = m!!(i `div` l, i `mod` l)

instance
  (Prim r, FreeModule r, ValidMatrix vect r, ValidLogic r, IsScalar r)
  => FiniteModule (Matrix vect r (a::Symbol) (b::Symbol)) where

  {-# INLINE dim #-}
  dim m = colLength m * rowLength m

  {-# INLINABLE unsafeToModule #-}
  -- unsafeToModule xs = unsafeToModuleM r xs

{-# INLINE row #-}
row :: (ValidMatrix vect r) => Matrix vect r (a::Symbol) (b::Symbol) -> Int -> vect
row m@(Matrix_Dynamic v l) i =
  unsafeToModule
  [ v!(i*l+j)
  | j <- [0..(rowLength m -1)]
  ]

{-# INLINE col #-}
col ::
  ( ValidMatrix vect r
  ) => Matrix vect r (a::Symbol) (b::Symbol) -> Int -> vect
col m@(Matrix_Dynamic v l) j =
  unsafeToModule
  [ v!(i*l+j)
  | i <- [0..(colLength m -1)]
  ]

{-# INLINE mmult #-}
mmult ::
  ( ValidMatrix vect (Scalar r)
  )
  => Matrix vect (Scalar r) (a::Symbol) (x0::Symbol)
  -> Matrix vect (Scalar r) (x0::Symbol) (b::Symbol)
  -> Matrix vect r (a::Symbol) (b::Symbol)
mmult m1@(Matrix_Dynamic _ _) m2@(Matrix_Dynamic _ cols2) =
  Matrix_Dynamic v cols2
  where
    v = unsafeToModule
      [ m1 `row` i <> m2 `col` j
      | i <- [0..cols2-1], j <- [0..cols2-1]
      ]

{-# INLINE transpose #-}
transpose ::
  ( ValidMatrix vect r
  )
  => Matrix vect (Scalar r) (a::Symbol) (b::Symbol)
  -> Matrix vect r (a::Symbol) (b::Symbol)
transpose m =
  unsafeToModuleM (colLength m)
  [ m!!(j,i)
  | i <- [0..(rowLength m - 1)]
  , j <- [0..(colLength m -1)]
  ]

data Matrix' vect r (a::Symbol) (b::Symbol) where
    Zero ::
        (ValidMatrix vect r) =>
        Matrix' vect r (a::Symbol) (b::Symbol)

    Id ::
        (ValidMatrix vect r) =>
        {-#UNPACK#-}!(Scalar r) -> Matrix' vect r (a::Symbol) (a::Symbol)

    Mat ::
        (ValidMatrix vect r) =>
        {-#UNPACK#-}!(Matrix vect r (a::Symbol) (b::Symbol))
        -> Matrix' vect r (a::Symbol) (b::Symbol)

type instance Scalar (Matrix' vect r (a::Symbol) (b::Symbol)) = Scalar r
type instance Logic (Matrix' vect r (a::Symbol) (b::Symbol)) = Bool

type instance Matrix' vect r (a::Symbol) (b::Symbol) >< a =
  Tensor_Linear (Matrix' vect r (a::Symbol) (b::Symbol)) a
type family Tensor_Linear a b where
    Tensor_Linear (Matrix' vect r (a::Symbol) (b::Symbol)) c =
      Matrix' vect r (a::Symbol) (b::Symbol)

deriving instance ( ValidMatrix vect (Scalar r), Show (Scalar r) ) =>
  Show (Matrix' vect r (a::Symbol) (b::Symbol))

instance Category (Matrix' vect r) where
    type ValidCategory (Matrix' vect r) m = ValidMatrix vect r

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



    -- name collision, how to instance these?
    -- {-# INLINE rowLength #-}
    -- rowLength :: ACCMatrix b v m n r -> Integer
    -- rowLength arr = snd (A.arrayShape arr)
    -- {-# INLINE colLength #-}
    -- colLength :: ACCMatrix b v m n r -> Integer
    -- colLength arr = fst (A.arrayShape arr)
    -- {-# INLINE (!!) #-}
    -- (!!) (ACCMatrix v) (i,j) = v A.! A.index2 (i,j)


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
    -- {-# INLINE zero #-}
    -- zero = ACCMatrix (A.fill (A.index2 2 4) 0)

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
  --  where
    -- {-# INLINE (.+) #-}
    -- (.+) (ACCMatrix v) r = ACCMatrix( A.map (\x -> x P.+ A.constant r) v)
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

  -- {-# INLINE (!) #-}
  -- (!) (ACCMatrix m) i = let
  --   l = A.size m
  --   in ACCMatrix m!!(i `div` l, i `mod` l)

-- {-# INLINE row #-}
-- row :: (ValidACCMatrix b v m n r
--   ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> vect
--   row m i = A.slice m (A.Z A.:. (i::Int) A.:. A.All)
--
-- {-# INLINE col #-}
-- col ::
--   ( ValidACCMatrix b v m n r
--   ) => ACCMatrix b v (n::Symbol) (m::Symbol) r -> Int -> vect
-- col m j = A.slice m (A.Z A.:. A.All A.:. (j::Int))
--
--
-- --taken from http://www.cse.unsw.edu.au/~chak/papers/repa.pdf
-- {-# INLINE mmult #-}
-- mmult ::
--   ( ValidACCMatrix b v m n r
--   )
--   => ACCMatrix b v (n::Symbol) (x0::Symbol) r
--   -> ACCMatrix b v (x0::Symbol) (m::Symbol) r
--   -> ACCMatrix b v (n::Symbol) (m::Symbol) r
-- mmult arr brr = A.sum (A.zipWith (*) arrRepl brrRepl)
--   where
--     trr = A.transpose brr
--     arrRepl = A.replicate (A.Z A.:. A.All A.:. colsB A.:. A.All) arr
--     brrRepl = A.replicate (A.Z A.:. rowsA A.:. A.All A.:. A.All) trr
--     (A.Z A.:. colsA A.:. rowsA) = A.shape arr
--     (A.Z A.:. colsB A.:. rowsB) = A.shape brr
--
-- {-# INLINE transpose #-}
-- transpose ::
--   ( ValidACCMatrix b v m n r
--   )
--   => ACCMatrix b v (m::Symbol) (n::Symbol) r
--   -> ACCMatrix b v (m::Symbol) (n::Symbol) r
-- transpose m = A.transpose m

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

-- Categor failes with:
--       The first argument of ‘Category’ should have kind ‘k0 -> k0 -> *’,
--         but ‘ACCMatrix' b v (m :: Symbol) (n :: Symbol) r’ has kind ‘*’
--       In the instance declaration for
--         ‘Category (ACCMatrix' b v (m :: Symbol) (n :: Symbol) r)’

-- instance Category (ACCMatrix' b v (m::Symbol) (n::Symbol) r ) where
--     type ValidCategory (ACCMatrix' b v (m::Symbol) (n::Symbol) r ) o = ValidACCMatrix b v m n r

    -- id = Id 1
    --
    -- Zero . Zero     = Zero
    -- Zero . (Id  _ ) = Zero
    -- Zero . (Mat _ ) = Zero
    --
    -- (Id  _ ) . Zero     = Zero
    -- (Id  r1) . (Id  r2) = Id  $ r1 * r2
    -- (Id  r ) . (Mat m ) = Mat $ m .* r
    --
    -- (Mat _) . Zero      = Zero
    -- (Mat m ) . (Id  r ) = Mat $ m .* r
    -- (Mat m1) . (Mat m2) = Mat $ mmult m2 m1

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
