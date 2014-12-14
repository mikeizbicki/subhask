-- | This module contains any objects relating to order theory
module SubHask.Algebra.Ord
    where

-- import Control.Monad
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

import Debug.Trace

-- newtype Swap a = Swap a
--     deriving (Read,Show,P.Eq)
--
-- instance P.Ord a => P.Ord (Swap a) where
--     a <= b = b P.<= a
--
-- newtype With a = With a
--     deriving (Read,Show)

-- instance Show a => Show (With a)
-- instance Read a => Read (With a)
-- instance NFData a => NFData (With a)
-- deriveHierarchy ''With [ ''Enum, ''Boolean, ''Ring, ''MetricSpace ]

-- instance Eq a => P.Eq (With a) where
--     (==) = undefined
--     (/=) = undefined
--
-- instance (P.Eq a, Ord a) => P.Ord (With a) where
-- --     compare = undefined
-- --     (<=) = undefined
--     compare (With a1) (With a2)
--         = trace "compare" $ P.EQ
-- --         = if a1 == a2
-- --             then P.EQ
-- --             else if a1 < a2
-- --                 then P.LT
-- --                 else P.GT
-------------

newtype WithPreludeOrd a = WithPreludeOrd { unWithPreludeOrd :: a }

instance Show a => Show (WithPreludeOrd a) where
    show (WithPreludeOrd a) = show a

-- | FIXME: for some reason, our deriving mechanism doesn't work on Show here;
-- It causes's Set's show to enter an infinite loop
deriveHierarchyFiltered ''WithPreludeOrd [ ''Enum, ''Boolean, ''Ring, ''MetricSpace ] [ ''Show ]

instance Eq a => P.Eq (WithPreludeOrd a) where
    a==b = a==b

instance Ord a => P.Ord (WithPreludeOrd a) where
    a<=b = a<=b
