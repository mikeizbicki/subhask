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
import SubHask.Category.Trans.Common

-------------------------------------------------------------------------------

type Mon a b = MonT (->) a b

class Category cat => MonoidHom cat

newtype MonT cat (a :: *) (b :: *) = MonT (cat a b)

-- mkCatTrans ''MonT ''Monoid


instance Category cat => MonoidHom (MonT cat)

unsafeProveMonoidHom ::
    ( Monoid a
    , Monoid b
    ) => cat a b -> MonT cat a b
unsafeProveMonoidHom = MonT


-------------------------------------------------------------------------------

type Grp a b = GrpT (->) a b

class MonoidHom cat => GroupHom cat

newtype GrpT cat (a :: *) (b :: *) = GrpT (cat a b)

-- mkCatTrans ''GrpT ''Group

instance Category cat => MonoidHom (GrpT cat)
instance Category cat => GroupHom (GrpT cat)

unsafeProveGroupHom ::
    ( Group a
    , Group b
    ) => cat a b -> GrpT cat a b
unsafeProveGroupHom = GrpT

-------------------------------------------------------------------------------

type Mod a b = ModT (->) a b

class GroupHom cat => ModuleHom cat

newtype ModT cat (a :: *) (b :: *) = ModT (cat a b)

-- mkCatTrans ''ModT ''Module

instance Category cat => MonoidHom (ModT cat)
instance Category cat => GroupHom (ModT cat)
instance Category cat => ModuleHom (ModT cat)

unsafeProveModuleHom ::
    ( Module a
    , Module b
    ) => cat a b -> ModT cat a b
unsafeProveModuleHom = ModT

-------------------------------------------------------------------------------

type Vect a b = LinearT (->) a b

class ModuleHom cat => Linear cat

newtype LinearT cat (a :: *) (b :: *) = LinearT (cat a b)

-- mkCatTrans ''LinearT ''VectorSpace

instance Category cat => MonoidHom (LinearT cat)
instance Category cat => GroupHom (LinearT cat)
instance Category cat => ModuleHom (LinearT cat)
instance Category cat => Linear (LinearT cat)

-- newtype Linear r a b = Linear (a r -> b r)
-- 
-- instance Category (Linear r) where
--     type ValidCategory (Linear r) (a :: * -> *) (b :: * -> *) = ()
--     id = Linear id
--     (Linear f1).(Linear f2) = Linear $ f1.f2

-------------------------------------------------------------------------------

type Lip a b = LipT (->) a b

-- | See <http://ncatlab.org/nlab/show/Lipschitz+map ncatlab> for more details.
class Category cat => Lipschitz cat where
    lipschitzModulus :: cat a b -> Scalar b

data LipT cat (a :: *) (b :: *) = LipT !(Scalar b) !(cat a b)

--mkCatTrans ''LipT ''MetricSpace

instance Category cat => Category (LipT cat) where
    type ValidCategory (LipT cat) a = 
        ( MetricSpace a
        , ValidCategory cat a 
        )

    {-# INLINE id #-}
    id = LipT one id

    {-# INLINE (.) #-}
    (LipT m1 f1).(LipT m2 f2) = LipT (m1*m2) (f1.f2)

instance Category cat => Lipschitz (LipT cat) where
    {-# INLINE lipschitzModulus #-}
    lipschitzModulus (LipT m _) = m

-------------------

type Met a b = MetT (->) a b

-- | See <http://ncatlab.org/nlab/show/short+map ncatlab> for more details.
class Category cat => Short cat

newtype MetT cat (a :: *) (b :: *) = MetT (cat a b)

-- mkCatTrans ''MetT ''MetricSpace

instance Category cat => Short (MetT cat)
instance Category cat => Lipschitz (MetT cat) where
    {-# INLINE lipschitzModulus #-}
    lipschitzModulus _ = one
