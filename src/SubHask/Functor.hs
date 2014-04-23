{-# LANGUAGE RebindableSyntax #-}
module SubHask.Functor
    where

import GHC.Prim
import Data.Proxy
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

class EndoFunctor cat f where
    efmap :: ValidCategory cat a b => cat a b -> cat (f a) (f b)
-- class EndoFunctor (+>) f where
--     efmap :: ValidCategory (+>) a b => (a +> b) -> f a +> f b

type ValidEndoFunctor cat f a b = 
    ( ValidCategory cat a b
    , ValidCategory cat (f a) (f b)
    , EndoFunctor cat f
    , Category cat
    )

fmap :: 
    ( ValidEndoFunctor cat f a b
    , Concrete cat
    ) => cat a b -> f a -> f b
fmap f a = efmap f $ a

(<$>) :: 
    ( ValidEndoFunctor cat f a b
    , Concrete cat
    ) => cat a b -> f a -> f b
(<$>) = fmap


-------------------------------------------------------------------------------
-- Applicatives

class Pointed f where
    point :: a -> f a

pure :: Pointed f => a -> f a
pure = point

return :: Pointed f => a -> f a
return = point

class (EndoFunctor cat f, Pointed f) => Applicative cat f where
    ap :: ValidCategory cat a b => f (cat a b) -> cat (f a) (f b)
        
-- class EndoFunctor cat f => Applicative cat f where
--     point :: cat a (f a)
--     ap :: ValidCategory cat a b => f (cat a b) -> cat (f a) (f b)
-- 
-- pure :: 
--     ( Applicative (->) f
--     , ValidCategory (->) a (f a)
--     ) => a -> f a
-- pure = point
-- 
-- return :: 
--     ( Monad (->) f
--     , ValidCategory (->) a (f a)
--     ) => a -> f a
-- return = point

type ValidApplicative cat f a b = 
    ( ValidEndoFunctor cat f a b 
    , Applicative cat f
    )

(<*>) :: 
    ( ValidApplicative cat f a b
    , Concrete cat
    ) => f (cat a b) -> f a -> f b 
(<*>) = \f -> embed $ ap f

-------------------------------------------------------------------------------
-- Monads

class Applicative cat m => Monad cat m where
    join :: cat (m (m a)) (m a)

(>>=) :: 
    ( Concrete cat
    , ValidMonad cat m a b
    ) => m a -> cat a (m b) -> m b
a >>= f = (join . efmap f) $ a

bind ::
    ( ValidMonad cat m a b
    , Category cat
    ) => cat a (m b) -> cat (m a) (m b)
bind f = join . (efmap f)

(>>) :: ValidMonad (->) m a b => m a -> m b -> m b
a >> f = (join . efmap (\ _ -> f)) $ a


type ValidMonad cat f a b = 
    ( ValidEndoFunctor cat f a b
    , ValidEndoFunctor cat f a (f b)
    , ValidEndoFunctor cat f (f b) b
    , Monad cat f
    )

---------------------------------------

test x = point x

liftM f m1 = do
    x1 <- m1
    point (f x1) 

liftM' :: forall cat m a b.
    ( MonadDo cat m
    , ValidMonad cat m a b
    , Concrete cat
    ) => cat a b -> cat (m a) (m b)
liftM' f = 
    let f1 = letbind (Proxy::Proxy subcat) (\x1 -> point (f $ x1))
    in bind f1 

class Monad cat m => MonadDo cat m where
    fail :: P.String -> m a
    fail str = P.error str

    letbind :: proxy cat -> (a -> m b) -> cat a (m b)

{-
f :: Double +> Double
f x = x+1
f = Linear $ \x -> x+1

g :: SparseFunction (Z 3) (Z 4)
g (Z 0) = Z 1
g (Z 1) = Z 2
g (Z 2) = Z 1
-}

-- liftM2 f m1 m2 = withCategory (Proxy::Proxy (->)) $ do
--     x1 <- m1
--     x2 <- m2
--     return (f x1 x2)

{-
class TypeMonoid m a where
    join :: m (m a) -> m a

class 
    ( Concrete cat
    , Applicative cat m
    ) => Monad cat m 
        where

    (>>=) :: 
        ( ValidMonad cat m a b
        , TypeMonoid m b
        ) => m a -> cat a (m b) -> m b
    a >>= f = join $ fmap f a
-}
