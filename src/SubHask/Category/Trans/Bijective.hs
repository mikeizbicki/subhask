-- | Provides transformer categories for injective, surjective, and bijective
-- functions.
--
-- TODO: Add @Epic@, @Monic@, and @Iso@ categories.
module SubHask.Category.Trans.Bijective
    ( Injective
    , InjectiveT
    , unsafeProveInjective
    , Surjective
    , SurjectiveT
    , unsafeProveSurjective
    , Bijective
    , BijectiveT
    , proveBijective
    , unsafeProveBijective
    , unInjectiveT
    , unSurjectiveT
    , unBijectiveT
    )
    where

import SubHask.Category
import SubHask.SubType

-- | Injective (one-to-one) functions map every input to a unique output.  See
-- <https://en.wikipedia.org/wiki/Injective_function wikipedia> for more detail.
class Concrete cat => Injective cat

newtype InjectiveT cat a b = InjectiveT { unInjectiveT :: cat a b }

instance Concrete cat => Injective (InjectiveT cat)

instance Category cat => Category (InjectiveT cat) where
    type ValidCategory (InjectiveT cat) a = (ValidCategory cat a)
    id = InjectiveT id
    (InjectiveT f).(InjectiveT g) = InjectiveT (f.g)

instance Sup a b c => Sup (InjectiveT a) b c
instance Sup b a c => Sup a (InjectiveT b) c
instance (subcat <: cat) => InjectiveT subcat <: cat where
    embedType_ = Embed2 (\ (InjectiveT f) -> embedType2 f)

unsafeProveInjective :: cat a b -> InjectiveT cat a b
unsafeProveInjective = InjectiveT

-- | Surjective (onto) functions can take on every value in the range.  See
-- <https://en.wikipedia.org/wiki/Surjective_function wikipedia> for more detail.
class Concrete cat => Surjective cat

newtype SurjectiveT cat a b = SurjectiveT { unSurjectiveT :: cat a b }

instance Concrete cat => Surjective (SurjectiveT cat)

instance Category cat => Category (SurjectiveT cat) where
    type ValidCategory (SurjectiveT cat) a = (ValidCategory cat a)
    id = SurjectiveT id
    (SurjectiveT f).(SurjectiveT g) = SurjectiveT (f.g)

instance Sup a b c => Sup (SurjectiveT a) b c
instance Sup b a c => Sup a (SurjectiveT b) c
instance (subcat <: cat) => SurjectiveT subcat <: cat where
    embedType_ = Embed2 (\ (SurjectiveT f) -> embedType2 f)

unsafeProveSurjective :: cat a b -> SurjectiveT cat a b
unsafeProveSurjective = SurjectiveT

-- | Bijective functions are both injective and surjective.  See
-- <https://en.wikipedia.org/wiki/Bijective_function wikipedia> for more detail.
class (Injective cat, Surjective cat) => Bijective cat

newtype BijectiveT cat a b = BijectiveT { unBijectiveT :: cat a b }

instance Concrete cat => Surjective (BijectiveT cat)
instance Concrete cat => Injective (BijectiveT cat)
instance Concrete cat => Bijective (BijectiveT cat)

instance Category cat => Category (BijectiveT cat) where
    type ValidCategory (BijectiveT cat) a = (ValidCategory cat a)
    id = BijectiveT id
    (BijectiveT f).(BijectiveT g) = BijectiveT (f.g)

instance Sup a b c => Sup (BijectiveT a) b c
instance Sup b a c => Sup a (BijectiveT b) c
instance (subcat <: cat) => BijectiveT subcat <: cat where
    embedType_ = Embed2 (\ (BijectiveT f) -> embedType2 f)

proveBijective :: (Injective cat, Surjective cat) => cat a b -> BijectiveT cat a b
proveBijective = BijectiveT

unsafeProveBijective :: cat a b -> BijectiveT cat a b
unsafeProveBijective = BijectiveT
