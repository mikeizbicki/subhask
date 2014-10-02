module SubHask.Category.Trans.Monotonic
--     ( MonT
--     , Mon
--     , unsafeProveMon
--     )
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

type family ValidMon a :: Constraint where
    ValidMon a = Ord a
--     ValidMon (MonT (->) b c) = (ValidMon b, ValidMon c)
--     ValidMon a = Ord a

data MonT cat a b where 
--     MonT :: (Ord a, Ord b) => cat a b -> MonT cat a b
    MonT :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b

unsafeProveMon :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b
unsafeProveMon = MonT

-------------------

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a = (ValidCategory cat a, ValidMon a)
    id = MonT id
    (MonT f).(MonT g) = MonT $ f.g

instance SubCategory subcat cat => SubCategory (MonT subcat) cat where
    embed (MonT f) = embed f

instance (ValidMon (TUnit cat), Monoidal cat) => Monoidal (MonT cat) where
    type Tensor (MonT cat) = Tensor cat
    tensor = error "FIXME: need to add a Hask Functor instance for this to work"

    type TUnit (MonT cat) = TUnit cat
    tunit _ = tunit (Proxy::Proxy cat)

instance (ValidMon (TUnit cat), Braided cat) => Braided (MonT cat) where
    braid   _ = braid   (Proxy :: Proxy cat)
    unbraid _ = unbraid (Proxy :: Proxy cat)

instance (ValidMon (TUnit cat), Symmetric cat) => Symmetric (MonT cat)

instance (ValidMon (TUnit cat), Cartesian cat) => Cartesian (MonT cat) where
    fst = MonT fst
    snd = MonT snd

    terminal a = MonT $ terminal a
    initial a = MonT $ initial a

-------------------------------------------------------------------------------

{-
type Mon = MonT Hask

newtype MonT cat a b = MonT (ConstrainedT '[P.Ord] cat a b)

unsafeProveMon :: 
    ( Ord b
    , Ord a
    , ValidCategory cat a
    , ValidCategory cat b
    ) => cat a b -> MonT (cat) a b
unsafeProveMon f = MonT $ proveConstrained f

-------------------

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a = ValidCategory (ConstrainedT '[P.Ord] cat) a 
    id = MonT id
    (MonT f) . (MonT g) = MonT (f.g)

instance SubCategory subcat cat => SubCategory (MonT subcat) cat where
    embed (MonT f) = embed f

instance (Ord (TUnit cat), Monoidal cat) => Monoidal (MonT cat) where
    type Tensor (MonT cat) = Tensor cat
    tensor = error "FIXME: need to add a Hask Functor instance for this to work"

    type TUnit (MonT cat) = TUnit cat
    tunit _ = tunit (Proxy::Proxy cat)

instance (Ord (TUnit cat), Braided cat) => Braided (MonT cat) where
    braid   _ = braid   (Proxy :: Proxy cat)
    unbraid _ = unbraid (Proxy :: Proxy cat)

instance (Ord (TUnit cat), Symmetric cat) => Symmetric (MonT cat)

instance (Ord (TUnit cat), Cartesian cat) => Cartesian (MonT cat) where
    fst = MonT $ ConstrainedT fst
    snd = MonT $ ConstrainedT snd

    terminal a = MonT $ ConstrainedT $ terminal a
    initial a = MonT $ ConstrainedT $ initial a


-------------------

mon :: Int -> [Int]
mon i = [i,i+1,i+2]

nomon :: Int -> [Int]
nomon i = if i `mod` 2 == 0
    then mon i
    else mon (i*2)

-}
