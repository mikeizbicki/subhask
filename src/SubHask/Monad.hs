-- | This module contains the Monad hierarchy of classes.
module SubHask.Monad
    where

import qualified Prelude as P
import Prelude (replicate, zipWith, unzip)

import SubHask.Algebra
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

--------------------------------------------------------------------------------
-- everything below here is a cut/paste from GHC's Control.Monad

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence       :: Monad Hask m => [m a] -> m [a]
{-# INLINE sequence #-}
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }

-- | Evaluate each action in the sequence from left to right,
-- and ignore the results.
sequence_        :: Monad Hask m => [m a] -> m ()
{-# INLINE sequence_ #-}
sequence_ ms     =  foldr (>>) (return ()) ms

-- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
mapM            :: Monad Hask m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM #-}
mapM f as       =  sequence (map f as)

-- | @'mapM_' f@ is equivalent to @'sequence_' . 'map' f@.
mapM_           :: Monad Hask m => (a -> m b) -> [a] -> m ()
{-# INLINE mapM_ #-}
mapM_ f as      =  sequence_ (map f as)

-- | This generalizes the list-based 'filter' function.
filterM          :: (Monad Hask m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     =  return []
filterM p (x:xs) =  do
   flg <- p x
   ys  <- filterM p xs
   return (if flg then x:ys else ys)

-- | 'forM' is 'mapM' with its arguments flipped
forM            :: Monad Hask m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM #-}
forM            = flip mapM


-- | 'forM_' is 'mapM_' with its arguments flipped
forM_           :: Monad Hask m => [a] -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_           = flip mapM_

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad Hask m) => m a -> m b
{-# INLINE forever #-}
forever a   = let a' = a >> a' in a'
-- Use explicit sharing here, as it is prevents a space leak regardless of
-- optimizations.

-- | @'void' value@ discards or ignores the result of evaluation, such as the return value of an 'IO' action.
void :: Functor Hask f => f a -> f ()
void = fmap (const ())

-- -----------------------------------------------------------------------------
-- Other monad functions

-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM      :: (Monad Hask m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs =  sequence (map f xs) >>= return . unzip

-- | The 'zipWithM' function generalizes 'zipWith' to arbitrary monads.
zipWithM          :: (Monad Hask m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys  =  sequence (zipWith f xs ys)

-- | 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_         :: (Monad Hask m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

{- | The 'foldM' function is analogous to 'foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


>       foldM f a1 [x1, x2, ..., xm]

==

>       do
>         a2 <- f a1 x1
>         a3 <- f a2 x2
>         ...
>         f am xm

If right-to-left evaluation is required, the input list should be reversed.
-}

foldM             :: (Monad Hask m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a []      =  return a
foldM f a (x:xs)  =  f a x >>= \fax -> foldM f fax xs

-- | Like 'foldM', but discards the result.
foldM_            :: (Monad Hask m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f a xs     = foldM f a xs >> return ()

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad Hask m) => Int -> m a -> m [a]
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad Hask m) => Int -> m a -> m ()
replicateM_ n x   = sequence_ (replicate n x)

{- | Conditional execution of monadic expressions. For example,

>       when debug (putStr "Debugging\n")

will output the string @Debugging\\n@ if the Boolean value @debug@ is 'True',
and otherwise do nothing.
-}

when              :: (Monad Hask m) => Bool -> m () -> m ()
when p s          =  if p then s else return ()

-- | The reverse of 'when'.

unless            :: (Monad Hask m) => Bool -> m () -> m ()
unless p s        =  if p then return () else s

-- | Promote a function to a monad.
liftM   :: (Monad Hask m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- >    liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- >    liftM2 (+) (Just 1) Nothing = Nothing
--
liftM2  :: (Monad Hask m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM3  :: (Monad Hask m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM4  :: (Monad Hask m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM5  :: (Monad Hask m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application.

>       return f `ap` x1 `ap` ... `ap` xn

is equivalent to

>       liftMn f x1 x2 ... xn

-}

ap                :: (Monad Hask m) => m (a -> b) -> m a -> m b
ap                =  liftM2 id


