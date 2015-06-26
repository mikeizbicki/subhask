-- | This module reexports the modules that every program using SubHask will need.
-- You should import it instead of Prelude.
module SubHask
    ( module SubHask.Algebra
    , module SubHask.Category
    , module SubHask.Compatibility.Base
    , module SubHask.Internal.Prelude
    , module SubHask.Monad
    , module SubHask.SubType
    ) where

import SubHask.Algebra
import SubHask.Category
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.Monad
import SubHask.SubType
