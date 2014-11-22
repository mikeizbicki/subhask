module SubHask.Algebra.Trans.EarthMover
    where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Prelude as P
import SubHask
-- import SubHask.Algebra.Vector
-- import SubHask.Algebra.Matrix
import SubHask.TemplateHaskell.Deriving

import Numeric.LinearProgramming

-------------------------------------------------------------------------------
-- EarthMover

newtype EarthMoverLP v = EarthMoverLP { unEarthMover :: v }

deriveHierarchy ''EarthMoverLP
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    , ''Ring
    , ''Indexed
    ]

instance
    ( Indexed v
    , Eq v
    , Ord (Scalar v)
    , Ring (Scalar v)
    ) => MetricSpace (EarthMoverLP v)
        where
--     distance (EarthMoverLP v1) (EarthMoverLP v2) = case lp of
--         Optimal (x,_) -> x
--         Feasible (x,_) -> x
--
--         where
--             lp = simplex
--                 (Minimuze [])
--                 (Sparse [])
--                 []
