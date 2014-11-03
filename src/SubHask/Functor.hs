{-# LANGUAGE ImplicitParams #-}
-- |
--
-- ---
--
-- NOTE: The current Functor instances require that we expose the constructors of our category GADTs.
-- This makes our code much less modular than we would like.
-- We could fix this problem by using a helper function like:
--
-- > mapWithMon :: (forall f c. Ord c => Ord (f c))
-- >     => ( ( Ord a
-- >          , Ord b
-- >          ) => (a -> b) -> f a -> f b
-- >        )
-- >     -> Mon a b -> Mon (f a) (f b)
-- > mapWithMon f (MonT (ConstrainedT g)) = unsafeProveMon $ f g
-- >
-- > instance Functor Mon Set.Set where
-- >     fmap = mapWithMon Set.mapMonotonic
--
-- But GHC is not powerful enough to handle the complex type of 'mapWithMon'.
--
module SubHask.Functor
    where

import qualified Prelude as P
import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra

import SubHask.Category.Trans.Constrained
import SubHask.Category.Trans.Monotonic
import qualified Data.Set as Set

-------------------------------------------------------------------------------

class Category cat => Functor cat f where
    fmap :: cat a b -> cat (f a) (f b)

class Functor cat f => Applicative cat f where
    pure :: ValidCategory cat a => cat a (f a)
    (<*>) :: f (cat a b) -> cat (f a) (f b)

class Functor cat f => Monad cat f where
    return :: ValidCategory cat a => cat a (f a)

    join :: ValidCategory cat a => cat (f (f a)) (f a)

    (>>=) ::
        ( ValidCategory cat b
        , Concrete cat
        ) => f a -> (cat a (f b)) -> f b
    (>>=) a f = (join . fmap f) $ a

--     (>>=) ::
--         ( ValidCategory cat b
--         , Concrete cat
--         ) => f a -> (cat a (f b)) -> proxy cat -> f b
--     (>>=) a f _ = (join . fmap f) $ a
--
--     (>>) ::
--         ( ValidCategory cat b
--         , ValidCategory cat a
--         , ValidCategory cat (f b)
--         , Concrete cat
--         , Cartesian cat
--         ) => f a -> f b -> proxy cat -> f b
--     (>>) a b p = (a >>= const b) p

class Functor cat f => Comonad cat f where
    coreturn :: ValidCategory cat a => cat (f a) a
    cojoin :: ValidCategory cat a => cat (f a) (f (f a))

    extend ::
        ( ValidCategory cat a
        , Concrete cat
        ) => cat (f a) b -> f a -> f b
    extend f a = (fmap f . cojoin) $ a

-------------------------------------------------------------------------------

instance Functor Hask [] where
    fmap = P.map

instance Monad Hask [] where
    return a = [a]
    join = P.concat

---------

-- FIXME: MonT instances broken due to standard library using a different Ord than subhask.
-- I need to add a compatibility layer converting standard library functions to use subhask.
--
-- instance Functor Mon [] where
--     fmap (MonT f) = unsafeProveMon $ map f
--
-- instance Monad Mon [] where
--     return = unsafeProveMon $ \a -> [a]
--     join = unsafeProveMon P.concat

---------------------------------------

instance Functor Hask (Hask a) where
    fmap f g = f.g

instance Monad Hask (Hask a) where
    return b = \_ -> b
    join f = \s -> f s $ s

-------------------------------------------------------------------------------

-- instance Functor Mon Set.Set where
--     fmap (MonT f) = unsafeProveMon $ Set.mapMonotonic f
--
-- instance Monad Mon Set.Set where
--     return = unsafeProveMon $ Set.singleton
--     join = unsafeProveMon $ Set.unions . Set.toList

---------------------------------------

-- instance Ord a => Functor Mon ((,) a) where
--     fmap (MonT f) = unsafeProveMon $ \(a,b) -> (a, f b)
--
-- instance Ord a => Comonad Mon ((,) a) where
--     coreturn = snd
--     cojoin = unsafeProveMon $ \a -> (fst a , a)

-------------------------------------------------------------------------------

-- return' ::
--     ( Monad cat f
--     , ValidCategory cat a
--     ) => proxy cat -> cat a (f a)
-- return' _ = return
--
-- glurgList :: Mon String [String]
-- glurgList = unsafeProveMon $ \x -> [x,x++" glurg!"]
--
-- glurgSet :: Mon String (Set.Set String)
-- glurgSet = unsafeProveMon $ \x -> Set.fromList[x,x++" glurg!"]
--
-- testList :: [String]
-- testList = (return' (Proxy::Proxy Mon) $ "test")
--     >>= glurgList
--     >>= glurgList
--     >>= glurgList
--     >>= glurgList
--
-- testSet :: Set.Set String
-- testSet = (return' (Proxy::Proxy Mon) $ "test")
--     >>= glurgSet
--     >>= glurgSet
--     >>= glurgSet
--     >>= glurgSet

-------------------------------------------------------------------------------

{-
instance Functor Mon Set.Set where
    fmap (MonT (ConstrainedT f)) = unsafeProveMon $ Set.mapMonotonic f

instance Monad Mon Set.Set where
    return = unsafeProveMon $ Set.singleton
    join = unsafeProveMon $ Set.unions . Set.toList

---------------------------------------

instance Ord a => Functor Mon ((,) a) where
    fmap (MonT (ConstrainedT f)) = MonT $ ConstrainedT $ \(a,b) -> (a, f b)

instance Ord a => Comonad Mon ((,) a) where
    coreturn = snd
    cojoin = MonT $ ConstrainedT $ \a -> (fst a , a)

test :: Mon () Char
test = initial 'a'

----------------------------------------

instance Functor (->) (Mon a) where
--     fmap f (MonT (ConstrainedT g)) = (MonT (ConstrainedT $ f.g))

mapper :: Ord c => (b -> c) -> Mon a b -> Mon a c
mapper f (MonT (ConstrainedT g)) = (MonT (ConstrainedT $ f.g))

instance Functor Mon (Mon a) where
    fmap (MonT (ConstrainedT f)) = undefined -- MonT $ ConstrainedT $ undefined

readmap :: Mon b c -> Mon (Mon a b) (Mon a c)
-- readmap (MonT (ConstrainedT f)) = (MonT (ConstrainedT $ \g -> f.g))
readmap f = (MonT (ConstrainedT $ \g -> f.g))

-------------------------------------------------------------------------------

glurg :: Mon String (Set.Set String)
glurg = unsafeProveMon $ \x -> Set.fromList [x,x++" glurg!"]

withMon :: (Proxy Mon -> a) -> a
withMon f = f (Proxy::Proxy Mon)

settest :: Set.Set String
-- settest = (return' (Proxy::Proxy Mon) $ "hi") >>= return' (Proxy :: Proxy Mon)
settest = withMon $ do
--     withMon return' $ "hi"
--     withMon return' $ "bye"
    return' (Proxy::Proxy Mon ) $ "hi"
--     glurg
    return' (Proxy::Proxy Mon ) $ "bye"

-- -- settest = (return' (Proxy::Proxy Mon) $ "hi") >>= glurg >>= glurg
-- --     glurg "test"
-- --     return' (Proxy::Proxy Mon) $ "bye"

-}

fail = error
