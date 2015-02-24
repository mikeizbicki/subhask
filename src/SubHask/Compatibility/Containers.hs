{-# LANGUAGE RebindableSyntax #-}
-- | Bindings to make the popular containers library compatible with subhask
module SubHask.Compatibility.Containers
    where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Algebra.Container
import SubHask.Algebra.Ord
import SubHask.Algebra.Parallel
import SubHask.Category
import SubHask.Category.Trans.Constrained
import SubHask.Category.Trans.Monotonic
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Sequence

newtype Seq a = Seq (Seq.Seq a)
    deriving (Read,Show,NFData)

type instance Scalar (Seq a) = Int
type instance Logic (Seq a) = Bool
type instance Elem (Seq a) = a

instance (Eq a, Arbitrary a) => Arbitrary (Seq a) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Seq a) where
    size (Seq s) = Seq.length s

instance Eq a => Eq_ (Seq a) where
    (Seq a1)==(Seq a2) = F.toList a1==F.toList a2

instance POrd a => POrd_ (Seq a) where
    inf a1 a2 = fromList $ inf (toList a1) (toList a2)

instance POrd a => MinBound_ (Seq a) where
    minBound = empty

instance Semigroup (Seq a) where
    (Seq a1)+(Seq a2) = Seq $ a1 Seq.>< a2

instance Monoid (Seq a) where
    zero = Seq $ Seq.empty

instance Eq a => Container (Seq a) where
    elem e (Seq a) = elem e $ F.toList a
    notElem = not elem

instance Constructible (Seq a) where
    cons e (Seq a) = Seq $ e Seq.<| a
    snoc (Seq a) e = Seq $ a Seq.|> e
    singleton e = Seq $ Seq.singleton e

    fromList1 x xs = Seq $ Seq.fromList (x:xs)

instance ValidEq a => Foldable (Seq a) where

    toList (Seq a) = F.toList a

    {-# INLINE uncons #-}
    uncons (Seq a) = if Seq.null a
        then Nothing
        else Just (Seq.index a 0, Seq $ Seq.drop 1 a)

    {-# INLINE unsnoc #-}
    unsnoc (Seq v) = if Seq.null v
        then Nothing
        else Just (Seq $ Seq.take (Seq.length v-1) v, Seq.index v 0)

--     foldMap f   (Seq a) = F.foldMap f   a

    foldr   f e (Seq a) = F.foldr   f e a
    foldr'  f e (Seq a) = F.foldr'  f e a
    foldr1  f   (Seq a) = F.foldr1  f   a
--     foldr1' f   (Seq a) = F.foldr1' f   a

    foldl   f e (Seq a) = F.foldl   f e a
    foldl'  f e (Seq a) = F.foldl'  f e a
    foldl1  f   (Seq a) = F.foldl1  f   a
--     foldl1' f   (Seq a) = F.foldl1' f   a

instance (ValidEq a) => Partitionable (Seq a) where
    partition n (Seq xs) = go xs
        where
            go :: Seq.Seq a -> [Seq a]
            go xs = if Seq.null xs
                then []
                else Seq a:go b
                where
                    (a,b) = Seq.splitAt len xs

            size = Seq.length xs
            len = size `div` n
                + if size `rem` n == 0 then 0 else 1

    partitionInterleaved n xs = foldl' go (P.replicate n empty) xs
        where
            go (r:rs) x = rs+[r`snoc`x]

-- partitionInterleaved_list :: Int -> [a] -> [[a]]
-- partitionInterleaved_list n xs = [map snd $ P.filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
--     where
--         ixs = addIndex 0 xs
--         addIndex i [] = []
--         addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)


-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Map

newtype Map k v = Map (M.Map (WithPreludeOrd k) (WithPreludeOrd v))
    deriving (Read,Show,NFData)

-- deriveHierarchy ''Map []

type instance Scalar (Map k v) = Int
type instance Logic (Map k v) = Bool
type instance Value (Map k v) = v
type instance Index (Map k v) = k
type instance Elem (Map k v) = (k,v)

instance (Eq v, Ord k, Semigroup v, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Map k v) where
    size (Map m) = M.size m

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

instance (Ord k, Eq v) => Constructible (Map k v) where
    singleton (k,v) = Map $ M.singleton (WithPreludeOrd k) (WithPreludeOrd v)

instance (Ord k, POrd v) => MinBound_ (Map k v) where
    minBound = zero

----------

instance (Ord k, Eq v) => IxConstructible (Map k v) where
   singletonAt k v = Map $ M.singleton (WithPreludeOrd k) (WithPreludeOrd v)
   insertAt k v (Map m) = Map $ M.insert (WithPreludeOrd k) (WithPreludeOrd v) m

instance (Ord k, Eq v) => IxContainer (Map k v) where
    lookup k (Map m) = P.fmap unWithPreludeOrd $ M.lookup (WithPreludeOrd k) m
    hasIndex k (Map m) = M.member (WithPreludeOrd k) m
    indices (Map m) = map unWithPreludeOrd $ M.keys m
    values (Map m) = map unWithPreludeOrd $ M.elems m

----------------------------------------
-- | This is a thin wrapper around Data.Map.Strict

newtype Map' k v = Map' (MS.Map (WithPreludeOrd k) (WithPreludeOrd v))
    deriving (Read,Show,NFData)

type instance Scalar (Map' k v) = Int
type instance Logic (Map' k v) = Bool
type instance Elem (Map' k v) = (k,v)

type instance Index (Map' k v) = k
type instance Value (Map' k v) = v
type instance SetValue (Map' k v) v' = Map' k v'

instance (Eq v, Ord k, Semigroup v, Arbitrary k, Arbitrary v) => Arbitrary (Map' k v) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Map' k v) where
    size (Map' m) = MS.size m

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

instance (Ord k, Eq v) => Constructible (Map' k v) where
    singleton (k,v) = Map' $ MS.singleton (WithPreludeOrd k) (WithPreludeOrd v)
    fromList1 x xs = Map' $ MS.fromList $ map (\(k,v) -> (WithPreludeOrd k,WithPreludeOrd v)) $ P.reverse (x:xs)

instance (Ord k, POrd v) => MinBound_ (Map' k v) where
    minBound = zero

instance (Ord k, Eq v) => Foldable (Map' k v) where
    toList (Map' m) = map (\(WithPreludeOrd k,WithPreludeOrd v) -> (k,v))
                    $ MS.toList m

----------

instance (Ord k, Eq v) => IxConstructible (Map' k v) where
   singletonAt k v = Map' $ MS.singleton (WithPreludeOrd k) (WithPreludeOrd v)
   insertAt k v (Map' m) = Map' $ MS.insert (WithPreludeOrd k) (WithPreludeOrd v) m

instance (Ord k, Eq v) => IxContainer (Map' k v) where
    lookup k (Map' m) = P.fmap unWithPreludeOrd $ MS.lookup (WithPreludeOrd k) m
    hasIndex k (Map' m) = MS.member (WithPreludeOrd k) m

    indices (Map' m) = map unWithPreludeOrd $ MS.keys m
    values (Map' m) = map unWithPreludeOrd $ MS.elems m
    imap f (Map' m) = Map' $ MS.mapWithKey go m
        where
            go (WithPreludeOrd k) (WithPreludeOrd v) = WithPreludeOrd $ f k v

-------------------------------------------------------------------------------
-- | This is a thin wrapper around the container's set type

newtype Set a = Set (Set.Set (WithPreludeOrd a))
    deriving (Read,Show,NFData)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = P.fmap fromList arbitrary

type instance Scalar (Set a) = Int
type instance Logic (Set a) = Logic a
type instance Elem (Set a) = a

instance Normed (Set a) where
    size (Set s) = Set.size s

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

instance Ord a => Constructible (Set a) where
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

    fromList1 a as = Set $ Set.fromList $ map WithPreludeOrd (a:as)

instance Ord a => Foldable (Set a) where
    foldl   f a (Set s) = Set.foldl   (\a (WithPreludeOrd e) -> f a e) a s
    foldl'  f a (Set s) = Set.foldl'  (\a (WithPreludeOrd e) -> f a e) a s
    foldr  f a (Set s) = Set.foldr  (\(WithPreludeOrd e) a -> f e a) a s
    foldr' f a (Set s) = Set.foldr' (\(WithPreludeOrd e) a -> f e a) a s

-------------------

-- |
--
-- FIXME: implement this in terms of @Lexical@ and @Set@
--
-- FIXME: add the @Constrained@ Monad
data LexSet a where
    LexSet :: Ord a => Set a -> LexSet a

type instance Scalar (LexSet a) = Int
type instance Logic (LexSet a) = Bool
type instance Elem (LexSet a) = a

instance Show a => Show (LexSet a) where
    show (LexSet s) = "LexSet "++show (toList s)

instance Eq_ (LexSet a) where
    (LexSet a1)==(LexSet a2) = Lexical a1==Lexical a2

instance POrd_ (LexSet a) where
    inf (LexSet a1) (LexSet a2) = LexSet $ unLexical $ inf (Lexical a1) (Lexical a2)
    (LexSet a1) <  (LexSet a2) = Lexical a1 <  Lexical a2
    (LexSet a1) <= (LexSet a2) = Lexical a1 <= Lexical a2

instance Lattice_ (LexSet a) where
    sup (LexSet a1) (LexSet a2) = LexSet $ unLexical $ sup (Lexical a1) (Lexical a2)
    (LexSet a1) >  (LexSet a2) = Lexical a1 >  Lexical a2
    (LexSet a1) >= (LexSet a2) = Lexical a1 >= Lexical a2

instance Ord_ (LexSet a)

instance Semigroup (LexSet a) where
    (LexSet a1)+(LexSet a2) = LexSet $ a1+a2

instance Ord a => Monoid (LexSet a) where
    zero = LexSet zero

instance (Ord a ) => Container (LexSet a) where
    elem x (LexSet s) = elem x s

instance (Ord a ) => Constructible (LexSet a) where
    fromList1 a as = LexSet $ fromList1 a as

instance (Ord a ) => Normed (LexSet a) where
    size (LexSet s) = size s

instance (Ord a ) => MinBound_ (LexSet a) where
    minBound = zero

instance (Ord a ) => Foldable (LexSet a) where
    foldl   f a (LexSet s) = foldl   f a s
    foldl'  f a (LexSet s) = foldl'  f a s
    foldl1  f   (LexSet s) = foldl1  f   s
    foldl1' f   (LexSet s) = foldl1' f   s
    foldr   f a (LexSet s) = foldr   f a s
    foldr'  f a (LexSet s) = foldr'  f a s
    foldr1  f   (LexSet s) = foldr1  f   s
    foldr1' f   (LexSet s) = foldr1' f   s

liftPreludeOrd :: (a -> b) -> WithPreludeOrd a -> WithPreludeOrd b
liftPreludeOrd f (WithPreludeOrd a) = WithPreludeOrd $ f a

instance Functor OrdHask LexSet where
    fmap (ConstrainedT f) = proveConstrained go
        where
            go (LexSet (Set s)) = LexSet $ Set $ Set.map (liftPreludeOrd f) s

instance Monad OrdHask LexSet where
    return_ = proveConstrained singleton
    join    = proveConstrained $ \(LexSet s) -> foldl1' (+) s

instance Functor Mon LexSet where
    fmap (MonT f) = unsafeProveMon go
        where
            go (LexSet (Set s)) = LexSet $ Set $ Set.mapMonotonic (liftPreludeOrd f) s

-- | FIXME: is there a more efficient implementation?
instance Monad Mon LexSet where
    return_ = unsafeProveMon singleton
    join    = unsafeProveMon $ \(LexSet s) -> foldl1' (+) s

instance Then LexSet where
    (LexSet a)>>(LexSet b) = LexSet b

dup :: (Ord a, Ring a) => Mon a (LexSet a)
dup = unsafeProveMon $ \a -> fromList [a,2*a,3*a]

pud ::  OrdHask Int (LexSet Int)
pud = proveConstrained go
    where
        go i = if i `mod` 2 == 0
            then fromList [1,2,3]
            else fromList [2,4]

qwe :: (Ord a, Ring a, a~Int) => LexSet a -> LexSet a
-- qwe s = s >>= dup >>= dup >>= (pud ) >>= dup
qwe s = s >>= unsafeProveMon (\x -> (dup $ x) >>= dup >> (pud $ x) >>= dup)

proveOrd :: (Ord a, Ord b) => (a -> b) -> OrdHask a b
proveOrd = proveConstrained

test = fromList [1,10,100] :: LexSet Int

pp :: [(Int,Int)]
pp = do
    a <- [1,2]
    b <- [10,11]
    return (a,b)

