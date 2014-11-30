module SubHask.Category.Trans.Linear
--     ( LinearT
--     , (+>)
--     , unsafeProveLinear
--     )
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra
import SubHask.SubType
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

data LinearT cat a b = LinearT { unLinearT :: cat a b }

instance Category cat => Category (LinearT cat) where
    type ValidCategory (LinearT cat) a = ( ValidCategory cat a )
    id = LinearT id
    (LinearT f).(LinearT g) = LinearT $ f.g

instance Sup (LinearT cat) cat cat
instance Sup cat (LinearT cat) cat
instance (LinearT cat <: cat) where
    embedType_ = Embed2 unLinearT

type (+>) = LinearT (->)

unsafeProveLinear :: cat a b -> LinearT cat a b
unsafeProveLinear = LinearT
