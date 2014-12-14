-- | Bindings to make the popular containers library compatible with subhask
module SubHask.Compatibility.Containers
    where

-- import Control.Monad
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as Set
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Algebra.Container
import SubHask.Algebra.Ord
import SubHask.Category
import SubHask.Category.Trans.Monotonic
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Map

newtype Map k v = Map (M.Map (WithPreludeOrd k) (WithPreludeOrd v))
--     deriving (Read,Show,NFData)

-- deriveHierarchy ''Map []

type instance Scalar (Map k v) = Int
type instance Logic (Map k v) = Bool
type instance Elem (Map k v) = (k,v)

instance (Eq v, Ord k, Semigroup v, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Map k v) where
    abs (Map m) = M.size m

instance (Eq k, Eq v) => Eq_ (Map k v) where
    (Map m1)==(Map m2) = m1 P.== m2

instance (Ord k, Eq v) => POrd_ (Map k v) where
    inf (Map m1) (Map m2) = Map $ M.differenceWith go (M.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance Ord k => Semigroup (Map k v) where
    (Map m1)+(Map m2) = Map $ M.union m1 m2

instance Ord k => Monoid (Map k v) where
    zero = Map $ M.empty

instance (Ord k, Eq v) => Container (Map k v) where
    elem (k,v) (Map m) = case M.lookup (WithPreludeOrd k) m of
        Nothing -> False
        Just (WithPreludeOrd v') -> v==v'

    notElem (k,v) (Map m) = case M.lookup (WithPreludeOrd k) m of
        Nothing -> True
        Just (WithPreludeOrd v') -> v/=v'

instance (Ord k, Eq v) => Indexed (Map k v) where
    (Map m) !! k = P.fmap unWithPreludeOrd $ M.lookup (WithPreludeOrd k) m
    hasIndex k (Map m) = M.member (WithPreludeOrd k) m
--     indices (Map m) = map unWithPreludeOrd $ M.keys m
--     values (Map m) = map unWithPreludeOrd $ M.elems m

instance (Ord k, Eq v) => Unfoldable (Map k v) where
    singleton (k,v) = Map $ M.singleton (WithPreludeOrd k) (WithPreludeOrd v)

----------------------------------------
-- | This is a thin wrapper around Data.Map.Strict

newtype Map' k v = Map' (MS.Map (WithPreludeOrd k) (WithPreludeOrd v))
--     deriving (Read,Show,NFData)

type instance Scalar (Map' k v) = Int
type instance Logic (Map' k v) = Bool
type instance Elem (Map' k v) = (k,v)

instance (Eq v, Ord k, Semigroup v, Arbitrary k, Arbitrary v) => Arbitrary (Map' k v) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Map' k v) where
    abs (Map' m) = MS.size m

instance (Eq k, Eq v) => Eq_ (Map' k v) where
    (Map' m1)==(Map' m2) = m1 P.== m2

instance (Ord k, Eq v) => POrd_ (Map' k v) where
    inf (Map' m1) (Map' m2) = Map' $ MS.differenceWith go (MS.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance Ord k => Semigroup (Map' k v) where
    (Map' m1)+(Map' m2) = Map' $ MS.union m1 m2

instance Ord k => Monoid (Map' k v) where
    zero = Map' $ MS.empty

instance (Ord k, Eq v) => Container (Map' k v) where
    elem (k,v) (Map' m) = case MS.lookup (WithPreludeOrd k) m of
        Nothing -> False
        Just (WithPreludeOrd v') -> v==v'

    notElem (k,v) (Map' m) = case MS.lookup (WithPreludeOrd k) m of
        Nothing -> True
        Just (WithPreludeOrd v') -> v/=v'

instance (Ord k, Eq v) => Indexed (Map' k v) where
    (Map' m) !! k = P.fmap unWithPreludeOrd $ MS.lookup (WithPreludeOrd k) m
    hasIndex k (Map' m) = MS.member (WithPreludeOrd k) m

    indices (Map' m) = map unWithPreludeOrd $ MS.keys m
    values (Map' m) = map unWithPreludeOrd $ MS.elems m

mapIndices :: (Ord k1, Ord k2) => (k1 -> k2) -> Map' k1 v -> Map' k2 v
mapIndices f (Map' m) = Map' $ MS.mapKeys (\(WithPreludeOrd k) -> WithPreludeOrd $ f k) m

mapValues :: (Ord k, Eq v1, Eq v2) => (v1 -> v2) -> Map' k v1 -> Map' k v2
mapValues f (Map' m) = Map' $ MS.map (\(WithPreludeOrd v) -> WithPreludeOrd $ f v) m

instance (Ord k, Eq v) => Unfoldable (Map' k v) where
    singleton (k,v) = Map' $ MS.singleton (WithPreludeOrd k) (WithPreludeOrd v)
    fromList xs = Map' $ MS.fromList $ map (\(k,v) -> (WithPreludeOrd k,WithPreludeOrd v)) $ P.reverse xs

instance (Ord k, Eq v) => Foldable (Map' k v) where
    toList (Map' m) = map (\(WithPreludeOrd k,WithPreludeOrd v) -> (k,v))
                    $ MS.toList m

-------------------------------------------------------------------------------
-- | This is a thin wrapper around the container's set type

newtype Set a = Set (Set.Set (WithPreludeOrd a))
--     deriving (Read,Show,NFData)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = P.fmap fromList arbitrary

type instance Scalar (Set a) = Int
type instance Logic (Set a) = Logic a
type instance Elem (Set a) = a

instance Normed (Set a) where
    abs (Set s) = Set.size s

instance Eq a => Eq_ (Set a) where
    (Set s1)==(Set s2) = s1'==s2'
        where
            s1' = removeWithPreludeOrd $ Set.toList s1
            s2' = removeWithPreludeOrd $ Set.toList s2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd (WithPreludeOrd x:xs) = x:removeWithPreludeOrd xs

instance Ord a => POrd_ (Set a) where
    inf (Set s1) (Set s2) = Set $ Set.intersection s1 s2

instance Ord a => MinBound_ (Set a) where
    minBound = Set $ Set.empty

instance Ord a => Lattice_ (Set a) where
    sup (Set s1) (Set s2) = Set $ Set.union s1 s2

instance Ord a => Semigroup (Set a) where
    (Set s1)+(Set s2) = Set $ Set.union s1 s2

instance Ord a => Monoid (Set a) where
    zero = Set $ Set.empty

instance Ord a => Abelian (Set a)

instance Ord a => Container (Set a) where
    elem a (Set s) = Set.member (WithPreludeOrd a) s
    notElem a (Set s) = not $ Set.member (WithPreludeOrd a) s

instance Ord a => Unfoldable (Set a) where
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

    fromList as = Set $ Set.fromList $ map WithPreludeOrd as

instance Ord a => Foldable (Set a) where
    foldl  f a (Set s) = Set.foldl  (\a (WithPreludeOrd e) -> f a e) a s
    foldl' f a (Set s) = Set.foldl' (\a (WithPreludeOrd e) -> f a e) a s
    foldr  f a (Set s) = Set.foldr  (\(WithPreludeOrd e) a -> f e a) a s
    foldr' f a (Set s) = Set.foldr' (\(WithPreludeOrd e) a -> f e a) a s

-------------------

liftWithPreludeOrd :: (a -> b) -> WithPreludeOrd a -> WithPreludeOrd b
liftWithPreludeOrd f (WithPreludeOrd a) = WithPreludeOrd $ f a

newtype Lexical_ s a = Lexical_ (s a)

-- deriveHierarchy ''Lexical_ [ ]
type instance Logic (Lexical_ s a) = Bool -- Logic (s a)
type instance Elem (Lexical_ s a) = Elem (s a)

instance Eq_ (Lexical_ s a)
instance POrd_ (Lexical_ s a)
instance Lattice_ (Lexical_ s a)
instance Ord_ (Lexical_ s a)
instance Semigroup (s a) => Semigroup (Lexical_ s a)
instance Monoid (s a) => Monoid (Lexical_ s a)
instance Container (s a) => Container (Lexical_ s a)
instance Unfoldable (s a) => Unfoldable (Lexical_ s a)
instance Foldable (s a) => Foldable (Lexical_ s a)

instance Functor Mon (Lexical_ Set) where
    fmap (MonT f) = unsafeProveMon
        $ \(Lexical_ (Set s)) -> Lexical_ $ Set $ Set.mapMonotonic (liftWithPreludeOrd f) s

instance Monad Mon (Lexical_ Set) where
    return = unsafeProveMon go
        where
            go :: a -> Lexical_ Set a
            go a = Lexical_ $ Set $ Set.singleton $ WithPreludeOrd a

--     join = unsafeProveMon go
--         where
--             go :: Ord a => Lexical_ Set (Lexical_ Set a) -> Lexical_ Set a
--             go a = foldl' (+) zero a
--
-- qq :: Ord a => Lexical_ Set (Lexical_ Set a) -> Lexical_ Set a
-- qq a = foldl' (+) zero a

----


class Category cat => Functor cat f where
    fmap :: cat a b -> cat (f a) (f b)

instance Functor (->) ((->) a) where
    fmap f g = f . g

class Functor cat f => Applicative cat f where
    pure :: cat a (f a)
    (<*>) :: f (cat a b) -> cat (f a) (f b)

instance Applicative (->) ((->) a) where
    pure b = \_ -> b
    f<*>g = \a -> f a (g a)

-- |
--
-- FIXME: right now, we're including any possibly relevant operator in this class;
-- the main reason is that I don't know if there will be more efficient implementations for these in different categories
--
-- FIXME: think about do notation again
class Functor cat m => Monad cat m where
    return :: ValidCategory cat a => cat a (m a)

    -- | join ought to have a default implementation of:
    --
    -- > join = (>>= id)
    --
    -- but "id" requires a "ValidCategory" constraint, so we can't use this default implementation.
    join :: cat (m (m a)) (m a)

    -- | In Hask, most people think of monads in terms of the @>>=@ operator;
    -- for our purposes, the reverse operator is more fundamental because it does not require the @Concrete@ constraint
    (=<<) :: cat a (m b) -> cat (m a) (m b)
    (=<<) f = join . fmap f

    -- | The bind operator is used in desguaring do notation;
    -- unlike all the other operators, we're explicitly applying values to the arrows passed in;
    -- that's why we need the "Concrete" constraint
    (>>=) :: Concrete cat => m a -> cat a (m b) -> m b
    (>>=) a f = join . fmap f $ a

    -- | Left-to-right Kleisli composition of monads.
    (>=>) :: cat a (m b) -> cat b (m c) -> cat a (m c)
    (>=>) = flip (<=<)

    -- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
    (<=<) :: cat b (m c) -> cat a (m b) -> cat a (m c)
    f<=<g = ((=<<) f) . g

instance Monad (->) ((->) a) where
    return b = \_ -> b
    join f = \a -> f a a

-- | Every Monad has a unique Kleisli category
--
-- FIXME: should this be a GADT?
newtype Kleisli cat f a b = Kleisli (cat a (f b))

instance Monad cat f => Category (Kleisli cat f) where
    type ValidCategory (Kleisli cat f) a = ValidCategory cat a
    id = Kleisli return
    (Kleisli f).(Kleisli g) = Kleisli (f<=<g)

------

-- FIXME: implement this in terms of @Lexical@ and @Set@
--
-- FIXME: add the @Constrained@ Monad
data LexSet a where
    LexSet :: Ord a => Set.Set (WithPreludeOrd a) -> LexSet a

type instance Logic (LexSet a) = Bool
type instance Elem (LexSet a) = a

instance Eq_ (LexSet a)
instance POrd_ (LexSet a)
instance Lattice_ (LexSet a)
instance Ord_ (LexSet a)
instance Semigroup (LexSet a)
instance Monoid (LexSet a)
instance (Eq_ a ) => Container (LexSet a)
instance (Eq_ a ) => Unfoldable (LexSet a)
instance (Eq_ a ) => Foldable (LexSet a)

instance Functor Mon LexSet where
    fmap (MonT f) = unsafeProveMon (go f)
        where
            go :: (Ord a, Ord b) => (a -> b) -> LexSet a -> LexSet b
            go f (LexSet s) = LexSet $ Set.mapMonotonic (liftWithPreludeOrd f) s

instance Monad Mon LexSet where
    return = unsafeProveMon go
        where
            go :: Ord a => a -> LexSet a
            go a = LexSet $ Set.singleton $ WithPreludeOrd a

    -- | FIXME: is there a more efficient implementation?
    join = unsafeProveMon go
        where
            go :: LexSet (LexSet a) -> LexSet a
            go (LexSet s) = unWithPreludeOrd $ foldl1' (+) (Set.toList s)

