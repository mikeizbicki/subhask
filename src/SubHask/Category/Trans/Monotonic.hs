module SubHask.Category.Trans.Monotonic
    ( MonT
    , Mon
    , unsafeProveMon
    )
    where

import GHC.Prim
import Data.Proxy
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra
import SubHask.Category.Trans.Constrained

-------------------------------------------------------------------------------

type Mon = MonT Hask

newtype MonT cat a b = MonT (ConstrainedT '[P.Ord] cat a b)

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a b = ValidCategory (ConstrainedT '[P.Ord] cat) a b
    id = MonT id
    (MonT f) . (MonT g) = MonT (f.g)

instance SubCategory subcat cat => SubCategory (MonT subcat) cat where
    embed (MonT f) = embed f

unsafeProveMon :: 
    ( P.Ord b
    , P.Ord a
    , ValidCategory cat a b
    ) => cat a b -> MonT (cat) a b
unsafeProveMon f = MonT $ proveConstrained (Proxy::Proxy '[P.Ord]) f

mon :: Int -> [Int]
mon i = [i,i+1,i+2]

nomon :: Int -> [Int]
nomon i = if i `mod` 2 == 0
    then mon i
    else mon (i*2)

-- lambda :: ValidCategory Mon a b => a -> Mon a b -> Mon a b
-- lambda a f = MonT $ \a ->  

-- \a = 1+a

-- lambda a (1+)
