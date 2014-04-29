module SubHask.Category.HMatrix
    where

import GHC.Prim

import Control.DeepSeq
import Data.Typeable
import qualified Prelude as P
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as LA

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Trans.Linear
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

data family HMatrix a b
data instance HMatrix (VS.Vector a) (VS.Vector a) = Matrix (LA.Matrix a)

instance Category HMatrix where
    
