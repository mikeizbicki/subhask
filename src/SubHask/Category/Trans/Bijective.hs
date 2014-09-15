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
    )
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

-- | Injective (one-to-one) functions map every input to a unique output.  See
-- <https://en.wikipedia.org/wiki/Injective_function wikipedia> for more detail.
class Concrete cat => Injective cat

newtype InjectiveT cat a b = InjectiveT (cat a b)

instance Concrete cat => Injective (InjectiveT cat)

instance Category cat => Category (InjectiveT cat) where
    type ValidCategory (InjectiveT cat) a = (ValidCategory cat a) 
    id = InjectiveT id
    (InjectiveT f).(InjectiveT g) = InjectiveT (f.g)

instance SubCategory subcat cat => SubCategory (InjectiveT subcat) cat where
    embed (InjectiveT f) = embed f

unsafeProveInjective :: Concrete cat => cat a b -> InjectiveT cat a b
unsafeProveInjective = InjectiveT

-------------------

-- | Surjective (onto) functions can take on every value in the range.  See 
-- <https://en.wikipedia.org/wiki/Surjective_function wikipedia> for more detail.
class Concrete cat => Surjective cat

newtype SurjectiveT cat a b = SurjectiveT (cat a b)

instance Concrete cat => Surjective (SurjectiveT cat)

instance Category cat => Category (SurjectiveT cat) where
    type ValidCategory (SurjectiveT cat) a = (ValidCategory cat a)
    id = SurjectiveT id
    (SurjectiveT f).(SurjectiveT g) = SurjectiveT (f.g)

instance SubCategory subcat cat => SubCategory (SurjectiveT subcat) cat where
    embed (SurjectiveT f) = embed f

unsafeProveSurjective :: Concrete cat => cat a b -> SurjectiveT cat a b
unsafeProveSurjective = SurjectiveT

-------------------

-- | Bijective functions are both injective and surjective.  See
-- <https://en.wikipedia.org/wiki/Bijective_function wikipedia> for more detail.
class (Injective cat, Surjective cat) => Bijective cat

newtype BijectiveT cat a b = BijectiveT (cat a b)

instance Concrete cat => Surjective (BijectiveT cat)
instance Concrete cat => Injective (BijectiveT cat)
instance Concrete cat => Bijective (BijectiveT cat)

instance Category cat => Category (BijectiveT cat) where
    type ValidCategory (BijectiveT cat) a = (ValidCategory cat a)
    id = BijectiveT id 
    (BijectiveT f).(BijectiveT g) = BijectiveT (f.g)

instance SubCategory subcat cat => SubCategory (BijectiveT subcat) cat where
    embed (BijectiveT f) = embed f

proveBijective :: (Injective cat, Surjective cat) => cat a b -> BijectiveT cat a b
proveBijective = BijectiveT

unsafeProveBijective :: Concrete cat => cat a b -> BijectiveT cat a b
unsafeProveBijective = BijectiveT

{-
data BijectiveT cat a b = BijectiveT (cat a b) (cat b a)

instance SubCategory cat subcat => SubCategory cat (BijectiveT subcat) where
    embed (BijectiveT f fi) = embed f

instance Category cat => Groupoid (BijectiveT cat) where
    inverse (BijectiveT f fi) = BijectiveT fi f

instance Category cat => Category (BijectiveT cat) where
    type ValidCategory (BijectiveT cat) a b = (ValidCategory cat a b, ValidCategory cat b a)
    id = BijectiveT id id
    (BijectiveT f fi).(BijectiveT g gi) = BijectiveT (f.g) (gi.fi)

unsafeProveBijective :: cat a b -> cat b a -> BijectiveT cat a b
unsafeProveBijective f fi = BijectiveT f fi
-}
