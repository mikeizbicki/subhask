module SubHask.Category.Finite
    (

    -- * Sparse functions
    SparseFunction
    , proveSparseFunction
    , list2sparseFunction

    -- * Dense functions
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

-------------------------------------------------------------------------------

-- | Represents finite functions as a map associating input/output pairs.
newtype SparseFunction a b = SparseFunction (Map.Map (Index a) (Index b)) 

instance Category SparseFunction where
    type ValidCategory SparseFunction a b = (FiniteType a, FiniteType b, Order a ~ Order b)

    id :: forall a. ValidCategory SparseFunction a a => SparseFunction a a
    id = SparseFunction $ Map.empty

    (SparseFunction f1).(SparseFunction f2) = SparseFunction
        (Map.map (\a -> find a f1) f2) 
        where
            find k map = case Map.lookup k map of
                P.Just v -> v
                P.Nothing -> swapIndex k

instance SubCategory (->) SparseFunction where
    embed (SparseFunction f) = map2function f
        where
            map2function map k = case Map.lookup (index k) map of
                P.Just v -> deIndex v
                P.Nothing -> deIndex $ swapIndex $ index k

-- | Generates a sparse representation of a 'Hask' function.  
-- This proof will always succeed.
proveSparseFunction :: ValidCategory SparseFunction a b => (a -> b) -> SparseFunction a b
proveSparseFunction f = SparseFunction 
    $ Map.fromList
    $ P.map (\a -> (index a,index $ f a)) enumerate

-- | Generate sparse functions on some subset of the domain.
list2sparseFunction :: ValidCategory SparseFunction a b => [Z (Order a)] -> SparseFunction a b
list2sparseFunction xs = SparseFunction $ Map.fromList $ go xs 
    where
        go (y:[]) = [(Index y, Index $ P.head xs)]
        go (y1:y2:ys) = (Index y1,Index y2):go (y2:ys)

-------------------------------------------------------------------------------

-- | Represents finite functions as a hash table associating input/output value pairs.
newtype DenseFunction a b = DenseFunction (VU.Vector P.Int)

instance Category DenseFunction where
    type ValidCategory DenseFunction a b = 
        ( FiniteType a
        , FiniteType b
        , Order a ~ Order b
        )
    
    id :: forall a. ValidCategory DenseFunction a a => DenseFunction a a
    id = DenseFunction $ VU.generate n id
        where
            n = P.fromIntegral $ natVal (Proxy :: Proxy (Order a))

    (.) :: forall b c a.
        ( ValidCategory DenseFunction b c
        , ValidCategory DenseFunction a b
        , ValidCategory DenseFunction a c
        ) => DenseFunction b c -> DenseFunction a b -> DenseFunction a c
    (DenseFunction f).(DenseFunction g) = DenseFunction $ VU.generate n go
        where
            go i = f VU.! (g VU.! i)
            n = P.fromIntegral $ natVal (Proxy :: Proxy (Order a))

instance SubCategory (->) DenseFunction where
    embed (DenseFunction f) = \x -> deIndex $ int2index $ f VU.! (index2int $ index x)

-- | Generates a dense representation of a 'Hask' function.  
-- This proof will always succeed, however, if the finite set is very large it may take a long time.  
-- In that case, a `SparseFunction` is probably the better representation.
proveDenseFunction :: forall a b. ValidCategory DenseFunction a b => (a -> b) -> DenseFunction a b
proveDenseFunction f = DenseFunction $ VU.generate n (index2int . index . f . deIndex . int2index)
    where
        n = P.fromIntegral $ natVal (Proxy :: Proxy (Order a))

---------------------------------------

-- | A type is finite if there is a bijection between it and the natural numbers.
-- The 'index'/'deIndex' functions implement this bijection.
class KnownNat (Order a) => FiniteType a where
    type Order a :: Nat
    index :: a -> Index a
    deIndex :: Index a -> a
    enumerate :: [a]
    
instance KnownNat n => FiniteType (Z n) where
    type Order (Z n) = n
    index i = Index i
    deIndex (Index i) = i
    enumerate = [ Z i | i <- [0..n P.- 1] ]
        where
            n = natVal (Proxy :: Proxy n)

-- | The 'Index' class is a newtype wrapper around the natural numbers 'Z'.
-- This gives us some additional type safety.
newtype Index a = Index (Z (Order a))
    deriving (P.Read,P.Show,P.Eq,P.Ord)

-- | Swap the phantom type between two indices.
swapIndex :: Order a ~ Order b => Index a -> Index b
swapIndex (Index i) = Index i

---------------------------------------
-- internal functions only

int2index :: P.Int -> Index a
int2index i = Index $ Z $ P.fromIntegral i

index2int :: Index a -> P.Int
index2int (Index (Z i)) = P.fromIntegral i
