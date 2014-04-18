module SubHask.Category.Monotonic
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category
import SubHask.Category.Constrained

-------------------------------------------------------------------------------

newtype Monotonic a b = Monotonic (a -> b)

instance Category Monotonic where
    type ValidCategory Monotonic a b = (P.Ord a, P.Ord b)
    id = Monotonic P.id
    (Monotonic f) . (Monotonic g) = Monotonic (f.g)

instance SubCategory (->) Monotonic where
    embed (Monotonic f) = f

instance SubCategory (Constrained '[P.Ord]) Monotonic where
    embed (Monotonic f) = Constrained f

embedMonotonic :: 
    ( SubCategory Monotonic subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Monotonic a b
embedMonotonic = embed

