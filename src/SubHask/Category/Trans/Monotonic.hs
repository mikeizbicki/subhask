module SubHask.Category.Trans.Monotonic
    ( MonotonicT
    , Monotonic
    , unsafeProveMonotonic
    )
    where

import GHC.Prim
import Data.Proxy
import qualified Prelude as P

import SubHask.Category
import SubHask.Category.Trans.Constrained

-------------------------------------------------------------------------------

newtype MonotonicT cat a b = MonotonicT (ConstrainedT '[P.Ord] cat a b)

instance Category cat => Category (MonotonicT cat) where
    type ValidCategory (MonotonicT cat) a b = ValidCategory (ConstrainedT '[P.Ord] cat) a b
    id = MonotonicT id
    (MonotonicT f) . (MonotonicT g) = MonotonicT (f.g)

instance SubCategory cat subcat => SubCategory cat (MonotonicT subcat) where
    embed (MonotonicT f) = embed f

unsafeProveMonotonic :: 
    ( P.Ord b
    , P.Ord a
    , ValidCategory cat a b
    ) => (cat a b) -> MonotonicT (cat) a b
unsafeProveMonotonic f = MonotonicT $ proveConstrained (Proxy::Proxy '[P.Ord]) f

---------------------------------------

type Monotonic = MonotonicT (->) 

