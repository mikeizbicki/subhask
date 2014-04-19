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

-------------------------------------------------------------------------------

data LinearT cat a b = LinearT (cat a b)

instance Category cat => Category (LinearT cat) where
    type ValidCategory (LinearT cat) a b = ( ValidCategory cat a b )
    id = LinearT id
    (LinearT f).(LinearT g) = LinearT $ f.g

instance SubCategory cat subcat => SubCategory cat (LinearT subcat) where
    embed (LinearT cat) = embed cat

type (+>) = LinearT (->)

unsafeProveLinear :: cat a b -> LinearT cat a b
unsafeProveLinear = LinearT
