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
import SubHask.SubType
import SubHask.Category.Trans.Constrained

-------------------------------------------------------------------------------

data family ProofOf (c::k) a

-------------------

-- newtype IncreasingT (cat :: * -> * -> *) a b = IncreasingT (cat a b)
data IncreasingT cat (a :: *) (b :: *) where
    IncreasingT :: (Ord_ a, Ord_ b) => cat a b -> IncreasingT cat a b

instance Category cat => Category (IncreasingT cat) where
    type ValidCategory (IncreasingT cat) a = (ValidCategory cat a, Ord_ a)
    id = IncreasingT id
    (IncreasingT f).(IncreasingT g) = IncreasingT $ f.g

instance Sup a b c => Sup (IncreasingT a) b c
instance Sup b a c => Sup a (IncreasingT b) c
instance (subcat <: cat) => IncreasingT subcat <: cat where
    embedType_ = Embed2 (\ (IncreasingT f) -> embedType2 f)

-------------------

instance Semigroup (cat a b) => Semigroup (IncreasingT cat a b) where
    (IncreasingT f)+(IncreasingT g) = IncreasingT $ f+g

instance (Ord_ a, Ord_ b, Monoid (cat a b)) => Monoid (IncreasingT cat a b) where
    zero = IncreasingT zero

instance Abelian (cat a b) => Abelian (IncreasingT cat a b) where

instance (Ord_ a, Ord_ b, Rg (cat a b)) => Rg (IncreasingT cat a b) where
    (IncreasingT f)*(IncreasingT g) = IncreasingT $ f*g

instance (Ord_ a, Ord_ b, Rig (cat a b)) => Rig (IncreasingT cat a b) where
    one = IncreasingT one

-------------------

newtype instance ProofOf IncreasingT a = ProofOf { unProofOf :: a }

instance Semigroup a => Semigroup (ProofOf IncreasingT a) where
    (ProofOf a1)+(ProofOf a2) = ProofOf (a1+a2)

instance Monoid a => Monoid (ProofOf IncreasingT a) where
    zero = ProofOf zero

instance Abelian a => Abelian (ProofOf IncreasingT a)

-------------------

type Increasing a = Increasing_ a
type family Increasing_ a where
    Increasing_ ( (cat :: * -> * -> *) a b) = IncreasingT cat a b

proveIncreasing ::
    ( Ord_ a
    , Ord_ b
    ) => (ProofOf IncreasingT a -> ProofOf IncreasingT b) -> Increasing (a -> b)
proveIncreasing f = IncreasingT $ \a -> unProofOf $ f $ ProofOf a

unsafeProveIncreasing ::
    ( Ord_ a
    , Ord_ b
    ) => (a -> b) -> Increasing (a -> b)
unsafeProveIncreasing = IncreasingT

-------------------------------------------------------------------------------

type Mon = MonT Hask

-- type family ValidMon a :: Constraint where
--     ValidMon a = Ord_ a
--     ValidMon (MonT (->) b c) = (ValidMon b, ValidMon c)
--     ValidMon a = Ord a
type ValidMon a = Ord a

data MonT cat (a :: *) (b :: *) where
    MonT :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b

unsafeProveMon :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b
-- unsafeProveMon :: (Ord_ a, Ord_ b) => cat a b -> MonT cat a b
unsafeProveMon = MonT

class Category cat => Monotonic cat
instance Category cat => Monotonic (MonT cat)

-------------------

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a = (ValidCategory cat a, ValidMon a)
    id = MonT id
    (MonT f).(MonT g) = MonT $ f.g

instance Sup a b c => Sup (MonT a) b c
instance Sup b a c => Sup a (MonT b) c
instance (subcat <: cat) => MonT subcat <: cat where
    embedType_ = Embed2 (\ (MonT f) -> embedType2 f)

-- instance (ValidMon (TUnit cat), Monoidal cat) => Monoidal (MonT cat) where
--     type Tensor (MonT cat) = Tensor cat
--     tensor = error "FIXME: need to add a Hask Functor instance for this to work"
--
--     type TUnit (MonT cat) = TUnit cat
--     tunit _ = tunit (Proxy::Proxy cat)

-- instance (ValidMon (TUnit cat), Braided cat) => Braided (MonT cat) where
--     braid   _ = braid   (Proxy :: Proxy cat)
--     unbraid _ = unbraid (Proxy :: Proxy cat)
--
-- instance (ValidMon (TUnit cat), Symmetric cat) => Symmetric (MonT cat)
--
-- instance (ValidMon (TUnit cat), Cartesian cat) => Cartesian (MonT cat) where
--     fst = MonT fst
--     snd = MonT snd
--
--     terminal a = MonT $ terminal a
--     initial a = MonT $ initial a

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
