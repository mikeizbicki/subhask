module SubHask.Instances
    where

import GHC.Prim
import qualified Prelude as P

import qualified Data.Set as Set

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Trans.Constrained
import SubHask.Category.Trans.Monotonic
import SubHask.Functor

-------------------------------------------------------------------------------
-- []

instance Pointed [] where
    point a = [a]

-- instance TypeMonoid [] a where
--     join xss = P.concat xss

-- instance Concrete cat => EndoFunctor cat [] where
--     efmap f = go (embed f) xs

-- instance Concrete cat => Applicative cat [] where
--     ap fs xs = [ f $ x | f <- fs, x <- xs ]
    
{-

-------------------------------------------------------------------------------
-- Set

instance Pointed Set.Set where
    point = Set.singleton

-- instance P.Ord a => TypeMonoid Set.Set a where
--     join set = Set.unions $ Set.toList set

---------------------------------------

instance Concrete cat => EndoFunctor (ConstrainedT '[P.Ord] cat) Set.Set where
    efmap f set =  Set.map (embed f) set

instance Concrete cat => Applicative (ConstrainedT '[P.Ord] cat) Set.Set where
    ap fs xs = Set.unions [ efmap f xs | f <- Set.toList fs ]

instance Concrete cat => Monad (ConstrainedT '[P.Ord] cat) Set.Set where
    join set = Set.unions $ Set.toList set

---------------------------------------

-- notice that this version is asymptotically faster than above
instance EndoFunctor Monotonic Set.Set where
    efmap f set =  Set.mapMonotonic (embed f) set 

instance Applicative Monotonic Set.Set where
    ap fs xs = Set.unions [ efmap f xs | f <- Set.toList fs ]

instance Monad Monotonic Set.Set where

-}
