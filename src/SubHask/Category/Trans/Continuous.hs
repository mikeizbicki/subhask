module SubHask.Category.Trans.Continuous
    where

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra

data C0T cat a b where
    C0T :: (Topology a, Topology b) => cat a b -> C0T cat a b

instance Category cat => Category (C0T cat) where
    type ValidCategory (C0T cat) a = (Topology a, ValidCategory cat a)
    id = C0T id
    (C0T f).(C0T g) = C0T $ f.g
