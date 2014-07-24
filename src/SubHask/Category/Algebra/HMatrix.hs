module SubHask.Category.Algebra.HMatrix
    where

import qualified Data.Packed.Matrix as HM

instance Monoid r => Monoid (HM.Matrix r) where
