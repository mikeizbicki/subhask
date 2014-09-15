{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}

module SubHask.Functor
    where

import GHC.Prim
import Data.Proxy
import qualified Prelude as P

import SubHask.Category
import GHC.Exts

import SubHask.Category.Trans.Constrained
import SubHask.Category.Trans.Monotonic
import qualified Data.Set as Set

-------------------------------------------------------------------------------

-- data Fun cat f g = forall a b. Fun (cat (f a) (g b))
-- 
-- instance Category cat => Category (Fun cat) where
--     type ValidCategory (Fun cat) f g = forall a b.
--         ( ValidCategory cat (f a) (g b)
--         )
-- 
--     id = Fun id

---------

-- | Type class specifying that the given functor satisfies the appropriate
-- requirements for the given category. 
--
-- FIXME: This is what we really want the @ValidFunctor@ family to look like.  
-- Unfortunately, due to various missing features in GHC (see
-- <https://ghc.haskell.org/trac/ghc/ticket/9195 #9195>,
-- <https://ghc.haskell.org/trac/ghc/ticket/7019 #7019>,
-- <https://ghc.haskell.org/trac/ghc/ticket/5927 #5927>,
-- <https://ghc.haskell.org/trac/ghc/ticket/2893 #2893>, and
-- <https://ghc.haskell.org/trac/ghc/ticket/2456 #2456>)
-- , this does not compile.  
--
-- > type ValidFunctor cat f = forall a b. ValidCategory cat a b => 
-- >     ( ValidCategory cat (f a) b
-- >     , ValidCategory cat a (f b)
-- >     )
--
-- This type family compiles.  But when we try to apply the family, the type
-- checker loops. 
--
-- > type family ValidFunctor (cat :: * -> * -> *) (f :: * -> *) a b :: Constraint
-- > type instance ValidFunctor cat f a b =
-- >     ( ValidCategory cat (f a) b
-- >     , ValidCategory cat a (f b)
-- >     , ValidFunctor cat f (f a) (f b)
-- >     )
--
-- The exact implementation is a slightly more verbose version of the above that
-- performs the recursion in the opposite direction.

-- type family ValidFunctor (cat :: * -> * -> *) (f :: * -> *) (a :: k1) (b :: k2) :: Constraint where
-- --     ValidFunctor cat f (f a) (f b) = 
-- --         ( ValidCategory cat (f a) (f b)
-- --         , ValidFunctor cat f a (f b) 
-- --         , ValidFunctor cat f (f a) b
-- --         )
-- -- 
-- --     ValidFunctor cat f (f a) b = 
-- --         ( ValidCategory cat (f a) b
-- --         , ValidFunctor cat f a b 
-- --         )
-- -- 
--     ValidFunctor cat f a (f b) = 
--         ( ValidCategory cat a (f b)
--         , ValidFunctor cat f a b 
--         )
-- 
--     ValidFunctor cat f a b = 
--         ( ValidCategory cat a b
--         ) 

type ValidFunctor (cat :: * -> * -> *) (f :: * -> *) a b = 
    ( ValidCategory cat a b
    , ValidCategory cat (f a) (f b)
    )

class EndoFunctor cat f where
    efmap :: ValidFunctor cat f a b => cat a b -> cat (f a) (f b)

instance EndoFunctor (->) [] where
    efmap = P.map

instance EndoFunctor (ConstrainedT '[P.Ord] (->)) Set.Set where
    efmap f = constrain $ \set -> Set.map (f$) set
    
instance EndoFunctor Mon Set.Set where
    efmap f = unsafeProveMon $ \set -> Set.mapMonotonic (f$) set

-- class Functor cat1 cat2 f where
--     fmap :: cat1 a b -> cat2 (f a) (f b)
-- 
-- type EndoFunctor cat f = Functor cat cat f
-- 
-- efmap :: EndoFunctor cat f => cat a b -> cat (f a) (f b)
-- efmap = fmap

-- | 
--
-- See <http://ncatlab.org/nlab/show/pointed+endofunctor ncatlab> for more.
class EndoFunctor cat f => Pointed cat f where
    point :: cat a (f a)

class TypeMonoid f where
    join :: f (f a) -> f a

class 
    ( EndoFunctor cat f
    , Concrete cat
    , Pointed cat f
    , TypeMonoid f
    ) => Monad cat f 
        where
    
    type ValidMonad cat f a b :: Constraint
    type ValidMonad cat f a b =
        ( ValidFunctor cat f a b
        , ValidFunctor cat f (f a) (f b)
        , ValidFunctor cat f (f a) (f (f b))
        )

    (>>=) :: 
        ( ValidMonad cat f a b
        ) => f a -> cat a (f b) -> f b

    idKleisli :: 
        ( ValidMonad cat f a a 
        ) => cat a (f a)
    idKleisli = point

    dotKleisli :: 
        ( ValidMonad cat f a b
        , ValidMonad cat f b c
        , ValidMonad cat f a c
        ) => cat b (f c) -> cat a (f b) -> cat a (f c)


defaultBind :: 
    ( Monad cat f
    , ValidCategory cat a (f b)
    , ValidCategory cat (f a) (f (f b))
    ) => f a -> cat a (f b) -> f b
defaultBind a f = join $ efmap f $ a

--     ) => f a -> (a ==cat=> f b) -> f b
--
-- f a -> (a >--c--> f b) -> f b

---------

newtype Kleisli cat f a b = Kleisli (cat a (f b))

instance Monad cat f => Category (Kleisli cat f) where
    type ValidCategory (Kleisli cat f) a b =
        ( ValidMonad cat f a b
        )

    {-# INLINE id #-}
    id = Kleisli $ idKleisli

    {-# INLINE (.) #-}
    (Kleisli a).(Kleisli b) = Kleisli $ dotKleisli a b

{-

class EndoFunctor cat f where
    efmap :: ValidCategory cat a b => cat a b -> cat (f a) (f b)

type ValidEndoFunctor cat f a b = 
    ( ValidCategory cat a b
    , ValidCategory cat (f a) (f b)
    , EndoFunctor cat f
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
    join :: ValidCategory cat a a => cat (m (m a)) (m a)

    {-# INLINE (>>=) #-}
    (>>=) :: ValidMonad cat m a b => m a -> cat a (m b) -> m b
    a >>= f = join . (efmap f) $ a

type ValidMonad cat f a b = 
    ( ValidEndoFunctor cat f a b
    , ValidEndoFunctor cat f a (f b)
    , ValidEndoFunctor cat f (f b) b
    , ValidCategory cat a a
    , ValidCategory cat b b
    , Category cat
    , Concrete cat
    , Monad cat f
    )

-- newtype Kleisli cat m a b = Kleisli (cat a (m b))
-- 
-- instance Concrete cat => Category (Kleisli cat m) where
--     type ValidCategory (Kleisli cat m) a b =
--         ( ValidCategory cat a (m b)
--         , ValidMonad cat m a b
--         )
--     id = Kleisli $ id


-- (>>=) :: 
--     ( Concrete cat
--     , ValidMonad cat m a b
--     ) => m a -> cat a (m b) -> m b
-- a >>= f = (join . efmap f) $ a
-- 
-- bind ::
--     ( ValidMonad cat m a b
--     , Category cat
--     ) => cat a (m b) -> cat (m a) (m b)
-- bind f = join . (efmap f)
-- 
-- (>>) :: ValidMonad (->) m a b => m a -> m b -> m b
-- a >> f = (join . efmap (\ _ -> f)) $ a

---------------------------------------

-- test x = point x
-- 
-- liftM f m1 = do
--     x1 <- m1
--     point (f x1) 
-- 
-- liftM' :: forall cat m a b.
--     ( MonadDo cat m
--     , ValidMonad cat m a b
--     , Concrete cat
--     ) => cat a b -> cat (m a) (m b)
-- liftM' f = 
--     let f1 = letbind (Proxy::Proxy subcat) (\x1 -> point (f $ x1))
--     in bind f1 

class Monad cat m => MonadDo cat m where
    fail :: P.String -> m a
    fail str = P.error str

    letbind :: proxy cat -> (a -> m b) -> cat a (m b)



-------------------------------------------------------------------------------

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

-}
