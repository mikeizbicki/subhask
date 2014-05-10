module SubHask.Category.Linear
    where

import GHC.Prim

import Control.DeepSeq
import Data.Typeable
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS

-- import qualified Numeric.LinearAlgebra as LA

import Data.Params.Vector.Unboxed

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Finite
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

class Category cat => Linear cat where


-------------------------------------------------------------------------------
-- vectors

