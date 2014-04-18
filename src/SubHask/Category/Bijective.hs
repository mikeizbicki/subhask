module SubHask.Category.Bijective
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra

-------------------------------------------------------------------------------

data BijectiveHask a b = BijectiveHask 
    { forward :: (a -> b)
    , backward :: (b -> a)
    }

instance Category BijectiveHask where
    id = BijectiveHask id id
    (BijectiveHask f fi).(BijectiveHask g gi) = BijectiveHask (f.g) (gi.fi)

instance SubCategory (->) BijectiveHask where
    embed = forward

instance Groupoid BijectiveHask where
    inverse (BijectiveHask f fi) = BijectiveHask fi f

-------------------

data SparseBijection a b = SparseBijection (Map.Map (Index a) (Index b)) (Map.Map (Index b) (Index a))

instance Category SparseBijection where
    type ValidCategory SparseBijection a b = (Finite a, Finite b, Order a ~ Order b)

    id :: forall a. ValidCategory SparseBijection a a => SparseBijection a a
    id = SparseBijection idmap idmap
        where
            idmap = Map.fromList $ P.map (\a -> (index a,index a)) (enumerate :: [a])

    (SparseBijection f1 fi1).(SparseBijection f2 fi2) = SparseBijection
        (Map.map (\a -> find a f1) f2) 
        (Map.map (\a -> find a fi2) fi1) 
        where
            find k map = case Map.lookup k map of
                P.Just v -> v
                P.Nothing -> swapIndex k

instance SubCategory BijectiveHask SparseBijection where
    embed (SparseBijection f fi) = BijectiveHask (map2function f) (map2function fi)
        where
            map2function map k = case Map.lookup (index k) map of
                P.Just v -> deIndex v
                P.Nothing -> deIndex $ swapIndex $ index k

list2bijection :: (Finite a, Finite b, Order a ~ Order b) => [Z (Order a)] -> SparseBijection a b
list2bijection xs = SparseBijection (Map.fromList newlist) (Map.fromList $ P.map swap newlist)
    where
        newlist = go xs
        swap (a,b) = (b,a)

        go (y:[]) = [(Index y, Index $ P.head xs)]
        go (y1:y2:ys) = (Index y1,Index y2):go (y2:ys)

type Sym (n::Nat) = SparseBijection (Z n) (Z n)

instance Finite a => Monoid (SparseBijection a a) where
    zero = id
    (+) = (.)

