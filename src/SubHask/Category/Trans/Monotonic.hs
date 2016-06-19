module SubHask.Category.Trans.Monotonic
     ( Mon (..)
     , unsafeProveMon

     -- * The MonT transformer
     , MonT (..)
     , unsafeProveMonT

     )
    where

import GHC.Prim
import Data.Proxy
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra
import SubHask.SubType
import SubHask.Category.Trans.Constrained

data IncreasingT cat (a :: *) (b :: *) where
    IncreasingT :: (Ord a, Ord b) => cat a b -> IncreasingT cat a b

mkMutable [t| forall cat a b. IncreasingT cat a b |]

instance Category cat => Category (IncreasingT cat) where
    type ValidCategory (IncreasingT cat) a = (ValidCategory cat a, Ord a)
    id = IncreasingT id
    (IncreasingT f).(IncreasingT g) = IncreasingT $ f.g

instance Sup a b c => Sup (IncreasingT a) b c
instance Sup b a c => Sup a (IncreasingT b) c
instance (subcat <: cat) => IncreasingT subcat <: cat where
    embedType_ = Embed2 (\ (IncreasingT f) -> embedType2 f)

instance Semigroup (cat a b) => Semigroup (IncreasingT cat a b) where
    (IncreasingT f)+(IncreasingT g) = IncreasingT $ f+g

instance Abelian (cat a b) => Abelian (IncreasingT cat a b) where

instance Provable (IncreasingT Hask) where
    f $$ a = ProofOf $ (f $ unProofOf a)

newtype instance ProofOf (IncreasingT cat) a = ProofOf { unProofOf :: ProofOf_ cat a }

mkMutable [t| forall a cat. ProofOf (IncreasingT cat) a |]

instance Semigroup (ProofOf_ cat a) => Semigroup (ProofOf (IncreasingT cat) a) where
    (ProofOf a1)+(ProofOf a2) = ProofOf (a1+a2)

instance Abelian (ProofOf_ cat a) => Abelian (ProofOf (IncreasingT cat) a)

type Increasing a = Increasing_ a
type family Increasing_ a where
    Increasing_ ( (cat :: * -> * -> *) a b) = IncreasingT cat a b

proveIncreasing ::
    ( Ord a
    , Ord b
    ) => (ProofOf (IncreasingT Hask) a -> ProofOf (IncreasingT Hask) b) -> Increasing (a -> b)
proveIncreasing f = unsafeProveIncreasing $ \a -> unProofOf $ f $ ProofOf a

instance (Ord a, Ord b) => Hask (ProofOf (IncreasingT Hask) a) (ProofOf (IncreasingT Hask) b) <: (IncreasingT Hask) a b where
    embedType_ = Embed0 proveIncreasing

unsafeProveIncreasing ::
    ( Ord a
    , Ord b
    ) => (a -> b) -> Increasing (a -> b)
unsafeProveIncreasing = IncreasingT

-- | A convenient specialization of "MonT" and "Hask"
type Mon = MonT Hask

type ValidMon a = Ord a

data MonT cat (a :: *) (b :: *) where
    MonT :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b

unsafeProveMonT :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b
unsafeProveMonT = MonT

unsafeProveMon :: (ValidMon a, ValidMon b) => cat a b -> MonT cat a b
unsafeProveMon = MonT

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a = (ValidCategory cat a, ValidMon a)
    id = MonT id
    (MonT f).(MonT g) = MonT $ f.g

instance Sup a b c => Sup (MonT a) b c
instance Sup b a c => Sup a (MonT b) c
instance (subcat <: cat) => MonT subcat <: cat where
    embedType_ = Embed2 (\ (MonT f) -> embedType2 f)

