module SubHask.Functor
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

class Functor cat f where
    fmap :: ValidCategory cat a b => cat a b -> f a -> f b

class Natural cat f1 f2 where
    nmap :: cat a b -> f1 a -> f2 b

class Pointed f where
    point :: a -> f a

pure :: Pointed f => a -> f a
pure = point

return :: Pointed f => a -> f a
return = point

class (Functor cat f, Pointed f) => Applicative cat f where
    (<*>) :: ValidCategory cat a b => f (cat a b) -> f a -> f b 

class TypeMonoid m a where
    join :: m (m a) -> m a

class Applicative cat m => Monad cat m where

    (>>=) :: (TypeMonoid m b, ValidCategory cat a (m b)) => m a -> cat a (m b) -> m b
    a >>= f = join $ fmap f a

