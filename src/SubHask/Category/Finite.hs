{-# LANGUAGE ScopedTypeVariables #-}

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
    , Index
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
import SubHask.Category
import SubHask.Quotient
import SubHask.Algebra.Objects
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

-- | Represents finite functions as a map associating input/output pairs.
data SparseFunction a b where
    SparseFunction :: 
        ( FiniteType a
        , FiniteType b
        , Order a ~ Order b 
        ) => Map.Map (Index a) (Index b) -> SparseFunction a b

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
                Nothing -> swapIndex k

instance SubCategory SparseFunction (->) where
    embed (SparseFunction f) = map2function f
        where
            map2function map k = case Map.lookup (index k) map of
                Just v -> deIndex v
                Nothing -> deIndex $ swapIndex $ index k

-- | Generates a sparse representation of a 'Hask' function.  
-- This proof will always succeed, although it may be computationally expensive if the 'Order' of a and b is large.
proveSparseFunction :: 
    ( ValidCategory SparseFunction a 
    , ValidCategory SparseFunction b
    , Order a ~ Order b
    ) => (a -> b) -> SparseFunction a b
proveSparseFunction f = SparseFunction 
    $ Map.fromList
    $ map (\a -> (index a,index $ f a)) enumerate

-- | Generate sparse functions on some subset of the domain.
list2sparseFunction :: 
    ( ValidCategory SparseFunction a 
    , ValidCategory SparseFunction b
    , Order a ~ Order b
    ) => [Z (Order a)] -> SparseFunction a b
list2sparseFunction xs = SparseFunction $ Map.fromList $ go xs 
    where
        go (y:[]) = [(Index y, Index $ head xs)]
        go (y1:y2:ys) = (Index y1,Index y2):go (y2:ys)

-------------------------------------------------------------------------------

data SparseFunctionMonoid a b where
    SparseFunctionMonoid :: 
        ( FiniteType a
        , FiniteType b
        , Monoid a
        , Monoid b
        , Order a ~ Order b 
        ) => Map.Map (Index a) (Index b) -> SparseFunctionMonoid a b

instance Category SparseFunctionMonoid where
    type ValidCategory SparseFunctionMonoid a =
        ( FiniteType a
        , Monoid a
        )

    id :: forall a. ValidCategory SparseFunctionMonoid a => SparseFunctionMonoid a a
    id = SparseFunctionMonoid $ Map.fromList $ zip xs xs
        where
            xs = map index (enumerate :: [a])

    (SparseFunctionMonoid f1).(SparseFunctionMonoid f2) = SparseFunctionMonoid
        (Map.map (\a -> find a f1) f2) 
        where
            find k map = case Map.lookup k map of
                Just v -> v
                Nothing -> index zero

instance SubCategory SparseFunctionMonoid (->) where
    embed (SparseFunctionMonoid f) = map2function f
        where
            map2function map k = case Map.lookup (index k) map of
                Just v -> deIndex v
                Nothing -> zero

---------------------------------------

{-
instance (FiniteType b, Semigroup b) => Semigroup (SparseFunctionMonoid a b) where
    (SparseFunctionMonoid f1)+(SparseFunctionMonoid f2) = SparseFunctionMonoid $ Map.unionWith go f1 f2
        where
            go b1 b2 = index $ deIndex b1 + deIndex b2

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
    negate (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.negate.deIndex) f

type instance Scalar (SparseFunctionMonoid a b) = Scalar b

instance (FiniteType b, Module b) => Module (SparseFunctionMonoid a b) where
    r *. (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.(r*.).deIndex) f

instance (FiniteType b, VectorSpace b) => VectorSpace (SparseFunctionMonoid a b) where 
    (SparseFunctionMonoid f) ./ r = SparseFunctionMonoid $ Map.map (index.(./r).deIndex) f
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

instance SubCategory DenseFunction (->) where
    embed (DenseFunction f) = \x -> deIndex $ int2index $ f VU.! (index2int $ index x)

-- | Generates a dense representation of a 'Hask' function.  
-- This proof will always succeed; however, if the 'Order' of the finite types  
-- are very large, it may take a long time.  
-- In that case, a `SparseFunction` is probably the better representation.
proveDenseFunction :: forall a b. 
    ( ValidCategory DenseFunction a 
    , ValidCategory DenseFunction b 
    ) => (a -> b) -> DenseFunction a b
proveDenseFunction f = DenseFunction $ VU.generate n (index2int . index . f . deIndex . int2index)
    where
        n = fromIntegral $ natVal (Proxy :: Proxy (Order a))

---------------------------------------

-- | A type is finite if there is a bijection between it and the natural numbers.
-- The 'index'/'deIndex' functions implement this bijection.
class KnownNat (Order a) => FiniteType a where
    type Order a :: Nat
    index :: a -> Index a
    deIndex :: Index a -> a
    enumerate :: [a]
    getOrder :: a -> Integer
    
instance KnownNat n => FiniteType (Z n) where
    type Order (Z n) = n
    index i = Index i
    deIndex (Index i) = i
    enumerate = [ quotient i | i <- [0..n - 1] ]
        where
            n = natVal (Proxy :: Proxy n)
    getOrder z = natVal (Proxy :: Proxy n)

-- | The 'Index' class is a newtype wrapper around the natural numbers 'Z'.
-- This gives us some additional type safety.
newtype Index a = Index (Z (Order a))
    deriving (Read,Show,Eq,Ord)

-- | Swap the phantom type between two indices.
swapIndex :: Order a ~ Order b => Index a -> Index b
swapIndex (Index i) = Index i

---------------------------------------
-- internal functions only

int2index :: Int -> Index a
int2index i = Index $ Mod $ fromIntegral i

index2int :: Index a -> Int
index2int (Index (Mod i)) = fromIntegral i
