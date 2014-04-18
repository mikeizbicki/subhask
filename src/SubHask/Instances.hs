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

instance TypeMonoid [] a where
    join xss = P.concat xss

instance ConcreteCategory cat => Functor cat [] where
    fmap f xs = P.map (embed f) xs

instance ConcreteCategory cat => Applicative cat [] where
    fs <*> xs = [ f $ x | f <- fs, x <- xs ]
    
instance ConcreteCategory cat => Monad cat [] where

-------------------------------------------------------------------------------
-- Set

instance Pointed Set.Set where
    point = Set.singleton

instance P.Ord a => TypeMonoid Set.Set a where
    join set = Set.unions $ Set.toList set

---------------------------------------

instance ConcreteCategory cat => Functor (ConstrainedT '[P.Ord] cat) Set.Set where
    fmap f set =  Set.map (embed f) set

instance ConcreteCategory cat => Applicative (ConstrainedT '[P.Ord] cat) Set.Set where
    fs <*> xs = Set.unions [ fmap f xs | f <- Set.toList fs ]

instance ConcreteCategory cat => Monad (ConstrainedT '[P.Ord] cat) Set.Set where

---------------------------------------

-- notice that this version is asymptotically faster than above
instance Functor Monotonic Set.Set where
    fmap f set =  Set.mapMonotonic (embed f) set 

instance Applicative Monotonic Set.Set where
    fs <*> xs = Set.unions [ fmap f xs | f <- Set.toList fs ]

instance Monad Monotonic Set.Set where


