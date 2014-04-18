module SubHask.Category.SparseFunction
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra

-------------------------------------------------------------------------------

newtype SparseFunction a b = SparseFunction (Map.Map (Index a) (Index b)) 

instance Category SparseFunction where
    type ValidCategory SparseFunction a b = (Finite a, Finite b, Order a ~ Order b)

    id :: forall a. ValidCategory SparseFunction a a => SparseFunction a a
    id = SparseFunction $ Map.fromList $ P.map (\a -> (index a,index a)) (enumerate :: [a])

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

list2sparseFunction :: (Finite a, Finite b, Order a ~ Order b) => [Z (Order a)] -> SparseFunction a b
list2sparseFunction xs = SparseFunction $ Map.fromList $ go xs 
    where
        go (y:[]) = [(Index y, Index $ P.head xs)]
        go (y1:y2:ys) = (Index y1,Index y2):go (y2:ys)

-- type Sym (n::Nat) = SparseFunction (Z n) (Z n)
-- 
-- instance Finite a => Monoid (SparseFunction a a) where
--     zero = id
--     (+) = (.)

---------------------------------------

class Finite a where
    type Order a :: Nat
    index :: a -> Index a
    deIndex :: Index a -> a
    enumerate :: [a]
    
instance KnownNat n => Finite (Z n) where
    type Order (Z n) = n
    index i = Index i
    deIndex (Index i) = i
    enumerate = [ Z i | i <- [0..n P.- 1] ]
        where
            n = natVal (Proxy :: Proxy n)

newtype Index a = Index (Z (Order a))
    deriving (P.Read,P.Show,P.Eq,P.Ord)

swapIndex :: Order a ~ Order b => Index a -> Index b
swapIndex (Index i) = Index i
