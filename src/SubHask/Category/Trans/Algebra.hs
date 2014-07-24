module SubHask.Category.Trans.Algebra
--     (
--     Mon
--     , MonT
--     , unsafeProveMonT
--     )
    where

import Debug.Trace

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra

-------------------------------------------------------------------------------

type Mon a b = MonT (->) a b

class Category cat => MonoidHom cat

newtype MonT cat (a :: *) (b :: *) = MonT (cat a b)

instance Category cat => Category (MonT cat) where
    type ValidCategory (MonT cat) a b = (Monoid a, Monoid b, ValidCategory cat a b)
    id = MonT id
    (MonT f).(MonT g) = MonT (f.g)

instance Category cat => MonoidHom (MonT cat)

instance SubCategory supercat cat => SubCategory supercat (MonT cat) where
    embed (MonT f) = embed f

unsafeProveMonT ::
    ( Monoid a
    , Monoid b
    ) => cat a b -> MonT cat a b
unsafeProveMonT = MonT


-------------------------------------------------------------------------------

type Grp a b = GrpT (->) a b

class MonoidHom cat => GroupHom cat

newtype GrpT cat (a :: *) (b :: *) = GrpT (cat a b)

instance Category cat => Category (GrpT cat) where
    type ValidCategory (GrpT cat) a b = (Group a, Group b, ValidCategory cat a b)
    id = GrpT id
    (GrpT f).(GrpT g) = GrpT (f.g)

instance Category cat => MonoidHom (GrpT cat)
instance Category cat => GroupHom (GrpT cat)

instance SubCategory supercat cat => SubCategory supercat (GrpT cat) where
    embed (GrpT f) = embed f

unsafeProveGrpT ::
    ( Group a
    , Group b
    ) => cat a b -> GrpT cat a b
unsafeProveGrpT = GrpT

-------------------------------------------------------------------------------

data Linear r a b = Linear (a r -> b r)

instance Category (Linear r) where
    type ValidCategory (Linear r) (a :: * -> *) (b :: * -> *) = ()
    id = Linear id
    (Linear f1).(Linear f2) = Linear $ f1.f2

