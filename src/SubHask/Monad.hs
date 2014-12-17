-- | This module contains the Monad hierarchy of classes.
module SubHask.Monad
    where

import qualified Prelude as P

import SubHask.Category
import SubHask.Internal.Prelude

--------------------------------------------------------------------------------

class Category cat => Functor cat f where
    fmap :: cat a b -> cat (f a) (f b)

-- |
--
-- FIXME: Not all monads can be made instances of Applicative in certain subcategories of hask.
-- For example, the "OrdHask" instance of "Set" requires an Ord constraint and a classical logic.
-- This means that we can't support @Set (a -> b)@, which means no applicative instance.
--
-- There are reasonable solutions to this problem for Set (by storing functions differently), but are there other instances where Applicative is not a monad?
class Functor cat f => Applicative cat f where
    pure :: cat a (f a)
    (<*>) :: f (cat a b) -> cat (f a) (f b)

-- | This class is a hack.
-- We can't include the @(>>)@ operator in the @Monad@ class because it doesn't depend on the underlying category.
class Then m where
    infixl 1 >>
    (>>) :: m a -> m b -> m b

-- | A default implementation
haskThen :: Monad Hask m => m a -> m b -> m b
haskThen xs ys = xs >>= \_ -> ys

-- | This is the only current alternative to the @Then@ class for supporting @(>>)@.
-- The problems with this implementation are:
-- 1. All those ValidCategory constraints are ugly!
-- 2. We've changed the signature of @(>>)@ in a way that's incompatible with do notation.
mkThen :: forall proxy cat m a b.
    ( Monad cat m
    , Cartesian cat
    , Concrete cat
    , ValidCategory cat a
    , ValidCategory cat (m b)
    ) => proxy cat -> m a -> m b -> m b
mkThen _ xs ys = xs >>= (const ys :: cat a (m b))

return :: Monad Hask m => a -> m a
return = return_

-- |
--
-- FIXME: right now, we're including any possibly relevant operator in this class;
-- the main reason is that I don't know if there will be more efficient implementations for these in different categories
--
-- FIXME: think about do notation again
class (Then m, Functor cat m) => Monad cat m where
    return_ :: ValidCategory cat a => cat a (m a)

    -- | join ought to have a default implementation of:
    --
    -- > join = (>>= id)
    --
    -- but "id" requires a "ValidCategory" constraint, so we can't use this default implementation.
    join :: cat (m (m a)) (m a)

    -- | In Hask, most people think of monads in terms of the @>>=@ operator;
    -- for our purposes, the reverse operator is more fundamental because it does not require the @Concrete@ constraint
    infixr 1 =<<
    (=<<) :: cat a (m b) -> cat (m a) (m b)
    (=<<) f = join . fmap f

    -- | The bind operator is used in desguaring do notation;
    -- unlike all the other operators, we're explicitly applying values to the arrows passed in;
    -- that's why we need the "Concrete" constraint
    infixl 1 >>=
    (>>=) :: Concrete cat => m a -> cat a (m b) -> m b
    (>>=) a f = join . fmap f $ a

    -- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
    infixr 1 <=<
    (<=<) :: cat b (m c) -> cat a (m b) -> cat a (m c)
    f<=<g = ((=<<) f) . g

    -- | Left-to-right Kleisli composition of monads.
    infixl 1 >=>
    (>=>) :: cat a (m b) -> cat b (m c) -> cat a (m c)
    (>=>) = flip (<=<)

fail = error

--------------------------------------------------------------------------------

-- | Every Monad has a unique Kleisli category
--
-- FIXME: should this be a GADT?
newtype Kleisli cat f a b = Kleisli (cat a (f b))

instance Monad cat f => Category (Kleisli cat f) where
    type ValidCategory (Kleisli cat f) a = ValidCategory cat a
    id = Kleisli return_
    (Kleisli f).(Kleisli g) = Kleisli (f<=<g)

