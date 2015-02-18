-- {-# LANGUAGE ScopedTypeVariables #-}

{- |
Finite categories are categories with a finite number of arrows.
In our case, this corresponds to functions with finite domains (and hence, ranges).
These functions have a number of possible representations.
Which is best will depend on the given function.
One common property is that these functions support decidable equality.
-}
module SubHask.Category.Finite
    (

    -- * Function representations
    -- ** Sparse permutations
    SparseFunction
    , proveSparseFunction
    , list2sparseFunction

    -- ** Sparse monoids
    , SparseFunctionMonoid

    -- ** Dense functions
    , DenseFunction
    , proveDenseFunction

    -- * Finite types
    , FiniteType (..)
    , ZIndex
    )
    where

import Control.Monad
import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Algebra.Group
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.SubType
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------

-- | A type is finite if there is a bijection between it and the natural numbers.
-- The 'index'/'deZIndex' functions implement this bijection.
class KnownNat (Order a) => FiniteType a where
    type Order a :: Nat
    index :: a -> ZIndex a
    deZIndex :: ZIndex a -> a
    enumerate :: [a]
    getOrder :: a -> Integer

instance KnownNat n => FiniteType (Z n) where
    type Order (Z n) = n
    index i = ZIndex i
    deZIndex (ZIndex i) = i
    enumerate = [ mkQuotient i | i <- [0..n - 1] ]
        where
            n = natVal (Proxy :: Proxy n)
    getOrder z = natVal (Proxy :: Proxy n)

-- | The 'ZIndex' class is a newtype wrapper around the natural numbers 'Z'.
--
-- FIXME: remove this layer; I don't think it helps
--
newtype ZIndex a = ZIndex (Z (Order a))

deriveHierarchy ''ZIndex [ ''Eq_, ''P.Ord ]

-- | Swap the phantom type between two indices.
swapZIndex :: Order a ~ Order b => ZIndex a -> ZIndex b
swapZIndex (ZIndex i) = ZIndex i

-------------------------------------------------------------------------------

-- | Represents finite functions as a map associating input/output pairs.
data SparseFunction a b where
    SparseFunction ::
        ( FiniteType a
        , FiniteType b
        , Order a ~ Order b
        ) => Map.Map (ZIndex a) (ZIndex b) -> SparseFunction a b

instance Category SparseFunction where
    type ValidCategory SparseFunction a =
        ( FiniteType a
        )

    id = SparseFunction $ Map.empty

    (SparseFunction f1).(SparseFunction f2) = SparseFunction
        (Map.map (\a -> find a f1) f2)
        where
            find k map = case Map.lookup k map of
                Just v -> v
                Nothing -> swapZIndex k

-- instance Sup SparseFunction (->) (->)
-- instance Sup (->) SparseFunction (->)
-- instance SparseFunction <: (->) where
--     embedType_ = Embed2 $ map2function f
--         where
--             map2function map k = case Map.lookup (index k) map of
--                 Just v -> deZIndex v
--                 Nothing -> deZIndex $ swapZIndex $ index k

-- | Generates a sparse representation of a 'Hask' function.
-- This proof will always succeed, although it may be computationally expensive if the 'Order' of a and b is large.
proveSparseFunction ::
    ( ValidCategory SparseFunction a
    , ValidCategory SparseFunction b
    , Order a ~ Order b
    ) => (a -> b) -> SparseFunction a b
proveSparseFunction f = SparseFunction
    $ Map.fromList
    $ P.map (\a -> (index a,index $ f a)) enumerate

-- | Generate sparse functions on some subset of the domain.
list2sparseFunction ::
    ( ValidCategory SparseFunction a
    , ValidCategory SparseFunction b
    , Order a ~ Order b
    ) => [Z (Order a)] -> SparseFunction a b
list2sparseFunction xs = SparseFunction $ Map.fromList $ go xs
    where
        go (y:[]) = [(ZIndex y, ZIndex $ P.head xs)]
        go (y1:y2:ys) = (ZIndex y1,ZIndex y2):go (y2:ys)

-------------------------------------------------------------------------------

data SparseFunctionMonoid a b where
    SparseFunctionMonoid ::
        ( FiniteType a
        , FiniteType b
        , Monoid a
        , Monoid b
        , Order a ~ Order b
        ) => Map.Map (ZIndex a) (ZIndex b) -> SparseFunctionMonoid a b

instance Category SparseFunctionMonoid where
    type ValidCategory SparseFunctionMonoid a =
        ( FiniteType a
        , Monoid a
        )

    id :: forall a. ValidCategory SparseFunctionMonoid a => SparseFunctionMonoid a a
    id = SparseFunctionMonoid $ Map.fromList $ P.zip xs xs
        where
            xs = P.map index (enumerate :: [a])

    (SparseFunctionMonoid f1).(SparseFunctionMonoid f2) = SparseFunctionMonoid
        (Map.map (\a -> find a f1) f2)
        where
            find k map = case Map.lookup k map of
                Just v -> v
                Nothing -> index zero

-- instance Sup SparseFunctionMonoid (->) (->)
-- instance Sup (->) SparseFunctionMonoid (->)
-- instance (SparseFunctionMonoid <: (->)) where
--     embedType_ = Embed2 $ map2function f
--         where
--             map2function map k = case Map.lookup (index k) map of
--                 Just v -> deZIndex v
--                 Nothing -> zero

---------------------------------------

{-
instance (FiniteType b, Semigroup b) => Semigroup (SparseFunctionMonoid a b) where
    (SparseFunctionMonoid f1)+(SparseFunctionMonoid f2) = SparseFunctionMonoid $ Map.unionWith go f1 f2
        where
            go b1 b2 = index $ deZIndex b1 + deZIndex b2

instance
    ( FiniteType a
    , FiniteType b
    , Monoid a
    , Monoid b
    , Order a ~ Order b
    ) => Monoid (SparseFunctionMonoid a b) where
    zero = SparseFunctionMonoid $ Map.empty

instance
    ( FiniteType b
    , Abelian b
    ) => Abelian (SparseFunctionMonoid a b)

instance (FiniteType b, Group b) => Group (SparseFunctionMonoid a b) where
    negate (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.negate.deZIndex) f

type instance Scalar (SparseFunctionMonoid a b) = Scalar b

instance (FiniteType b, Module b) => Module (SparseFunctionMonoid a b) where
    r *. (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.(r*.).deZIndex) f

instance (FiniteType b, VectorSpace b) => VectorSpace (SparseFunctionMonoid a b) where
    (SparseFunctionMonoid f) ./ r = SparseFunctionMonoid $ Map.map (index.(./r).deZIndex) f
-}

-------------------------------------------------------------------------------

-- | Represents finite functions as a hash table associating input/output value pairs.
data DenseFunction (a :: *) (b :: *) where
    DenseFunction ::
        ( FiniteType a
        , FiniteType b
        ) => VU.Vector Int ->  DenseFunction a b

instance Category DenseFunction where
    type ValidCategory DenseFunction (a :: *) =
        ( FiniteType a
        )

    id :: forall a. ValidCategory DenseFunction a => DenseFunction a a
    id = DenseFunction $ VU.generate n id
        where
            n = fromIntegral $ natVal (Proxy :: Proxy (Order a))

    (DenseFunction f).(DenseFunction g) = DenseFunction $ VU.map (f VU.!) g

-- instance SubCategory DenseFunction (->) where
--     embed (DenseFunction f) = \x -> deZIndex $ int2index $ f VU.! (index2int $ index x)

-- | Generates a dense representation of a 'Hask' function.
-- This proof will always succeed; however, if the 'Order' of the finite types
-- are very large, it may take a long time.
-- In that case, a `SparseFunction` is probably the better representation.
proveDenseFunction :: forall a b.
    ( ValidCategory DenseFunction a
    , ValidCategory DenseFunction b
    ) => (a -> b) -> DenseFunction a b
proveDenseFunction f = DenseFunction $ VU.generate n (index2int . index . f . deZIndex . int2index)
    where
        n = fromIntegral $ natVal (Proxy :: Proxy (Order a))

---------------------------------------
-- internal functions only

int2index :: Int -> ZIndex a
int2index i = ZIndex $ Mod $ fromIntegral i

index2int :: ZIndex a -> Int
index2int (ZIndex (Mod i)) = fromIntegral i
