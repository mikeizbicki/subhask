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

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

-- | Represents finite functions as a map associating input/output pairs.
newtype SparseFunction a b = SparseFunction (Map.Map (Index a) (Index b)) 
    deriving (Eq)

instance Category SparseFunction where
    type ValidCategory SparseFunction a b = 
        ( FiniteType a
        , FiniteType b
        , Order a ~ Order b
        )

    id = SparseFunction $ Map.empty

    (SparseFunction f1).(SparseFunction f2) = SparseFunction
        (Map.map (\a -> find a f1) f2) 
        where
            find k map = case Map.lookup k map of
                Just v -> v
                Nothing -> swapIndex k

instance SubCategory (->) SparseFunction where
    embed (SparseFunction f) = map2function f
        where
            map2function map k = case Map.lookup (index k) map of
                Just v -> deIndex v
                Nothing -> deIndex $ swapIndex $ index k

-- | Generates a sparse representation of a 'Hask' function.  
-- This proof will always succeed, although it may be computationally expensive if the 'Order' of a and b is large.
proveSparseFunction :: ValidCategory SparseFunction a b => (a -> b) -> SparseFunction a b
proveSparseFunction f = SparseFunction 
    $ Map.fromList
    $ map (\a -> (index a,index $ f a)) enumerate

-- | Generate sparse functions on some subset of the domain.
list2sparseFunction :: ValidCategory SparseFunction a b => [Z (Order a)] -> SparseFunction a b
list2sparseFunction xs = SparseFunction $ Map.fromList $ go xs 
    where
        go (y:[]) = [(Index y, Index $ head xs)]
        go (y1:y2:ys) = (Index y1,Index y2):go (y2:ys)

-------------------------------------------------------------------------------

newtype SparseFunctionMonoid a b = SparseFunctionMonoid (Map.Map (Index a) (Index b))
    deriving (Eq)

instance Category SparseFunctionMonoid where
    type ValidCategory SparseFunctionMonoid a b =
        ( FiniteType a
        , FiniteType b
        , Order a ~ Order b
        )

    id :: forall a. ValidCategory SparseFunctionMonoid a a => SparseFunctionMonoid a a
    id = SparseFunctionMonoid $ Map.fromList $ zip xs xs
        where
            xs = map index (enumerate :: [a])

    (SparseFunctionMonoid f1).(SparseFunctionMonoid f2) = SparseFunctionMonoid
        (Map.map (\a -> find a f1) f2) 
        where
            find k map = case Map.lookup k map of
                Just v -> v
                Nothing -> swapIndex k

---------------------------------------

instance (FiniteType b, Monoid b) => Monoid (SparseFunctionMonoid a b) where
    zero = SparseFunctionMonoid $ Map.empty
    (SparseFunctionMonoid f1)+(SparseFunctionMonoid f2) = SparseFunctionMonoid $ Map.unionWith go f1 f2
        where
            go b1 b2 = index $ deIndex b1 + deIndex b2

instance (FiniteType b, Abelian b) => Abelian (SparseFunctionMonoid a b) 

instance (FiniteType b, Group b) => Group (SparseFunctionMonoid a b) where
    negate (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.negate.deIndex) f

type instance Scalar (SparseFunctionMonoid a b) = Scalar b

instance (FiniteType b, Module r b) => Module r (SparseFunctionMonoid a b) where
    r .* (SparseFunctionMonoid f) = SparseFunctionMonoid $ Map.map (index.(r.*).deIndex) f

instance (FiniteType b, VectorSpace r b) => VectorSpace r (SparseFunctionMonoid a b) where 
    (SparseFunctionMonoid f) /. r = SparseFunctionMonoid $ Map.map (index.(/.r).deIndex) f

-------------------------------------------------------------------------------

-- | Represents finite functions as a hash table associating input/output value pairs.
newtype DenseFunction a b = DenseFunction (VU.Vector Int)
    deriving Eq

instance Category DenseFunction where
    type ValidCategory DenseFunction (a :: *) (b :: *) = 
        ( FiniteType a
        , FiniteType b
        )

    id :: forall a. ValidCategory DenseFunction a a => DenseFunction a a
    id = DenseFunction $ VU.generate n id
        where
            n = fromIntegral $ natVal (Proxy :: Proxy (Order a))

    (DenseFunction f).(DenseFunction g) = DenseFunction $ VU.map (f VU.!) g

instance SubCategory (->) DenseFunction where
    embed (DenseFunction f) = \x -> deIndex $ int2index $ f VU.! (index2int $ index x)

-- | Generates a dense representation of a 'Hask' function.  
-- This proof will always succeed; however, if the 'Order' of the finite types  
-- are very large, it may take a long time.  
-- In that case, a `SparseFunction` is probably the better representation.
proveDenseFunction :: forall a b. ValidCategory DenseFunction a b => (a -> b) -> DenseFunction a b
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
    enumerate = [ Z i | i <- [0..n - 1] ]
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
int2index i = Index $ Z $ fromIntegral i

index2int :: Index a -> Int
index2int (Index (Z i)) = fromIntegral i
