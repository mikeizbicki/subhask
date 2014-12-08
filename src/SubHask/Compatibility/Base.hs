module SubHask.Compatibility.Base
    where

import Data.Typeable
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

fromPreludeEq [t|TypeRep|]

