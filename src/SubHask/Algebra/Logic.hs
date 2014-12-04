module SubHask.Algebra.Logic
    where

import Control.Monad
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

--------------------------------------------------------------------------------
-- | A Boolean algebra is a special type of Ring.
-- Their applications (set-like operations) tend to be very different than Rings, so it makes sense for the class hierarchies to be completely unrelated.
-- The "Boolean2Ring" type, however, provides the correct transformation.

newtype Boolean2Ring b = Boolean2Ring b

mkBoolean2Ring :: Boolean b => b -> Boolean2Ring b
mkBoolean2Ring = Boolean2Ring

instance Boolean b => Semigroup (Boolean2Ring b) where
--     (Boolean2Ring b1)+(Boolean2Ring b2) = Boolean2Ring $ (b1 && not b2) || (not b1 && b2)
    (Boolean2Ring b1)+(Boolean2Ring b2) = Boolean2Ring $ (b1 || b2) && not (b1 && b2)

instance Boolean b => Abelian (Boolean2Ring b)

instance Boolean b => Monoid (Boolean2Ring b) where
    zero = Boolean2Ring $ false

instance Boolean b => Cancellative (Boolean2Ring b) where
    (-)=(+)
--     b1-b2 = b1+negate b2

instance Boolean b => Group (Boolean2Ring b) where
    negate = id
--     negate (Boolean2Ring b) = Boolean2Ring $ not b

instance Boolean b => Rg (Boolean2Ring b) where
    (Boolean2Ring b1)*(Boolean2Ring b2) = Boolean2Ring $ b1 && b2

instance Boolean b => Rig (Boolean2Ring b) where
    one = Boolean2Ring $ true

instance Boolean b => Ring (Boolean2Ring b)
