module SubHask.Category.Trans.Bijective
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map as Map
import qualified Prelude as P

import SubHask.Category
import SubHask.Algebra

-------------------------------------------------------------------------------

data BijectiveT cat a b = BijectiveT (cat a b) (cat b a)

instance Category cat => Category (BijectiveT cat) where
    type ValidCategory (BijectiveT cat) a b = (ValidCategory cat a b, ValidCategory cat b a)
    id = BijectiveT id id
    (BijectiveT f fi).(BijectiveT g gi) = BijectiveT (f.g) (gi.fi)

instance SubCategory cat subcat => SubCategory cat (BijectiveT subcat) where
    embed (BijectiveT f fi) = embed f

instance Category cat => Groupoid (BijectiveT cat) where
    inverse (BijectiveT f fi) = BijectiveT fi f

---------------------------------------

unsafeProveBijective :: cat a b -> cat b a -> BijectiveT cat a b
unsafeProveBijective f fi = BijectiveT f fi
