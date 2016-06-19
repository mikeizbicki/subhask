{-# LANGUAGE RebindableSyntax #-}
-- | Bindings to make the popular containers library compatible with subhask
module SubHask.Compatibility.Containers
    where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as MS
import qualified Data.IntMap.Strict as IMS
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
import SubHask.Compatibility.Base()
import SubHask.Internal.Prelude
import SubHask.Monad

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Sequence

newtype Seq a = Seq (Seq.Seq a)
    deriving (Read,Show,NFData)

mkMutable [t| forall a. Seq a |]

type instance Scalar (Seq a) = Int
type instance Logic (Seq a) = Bool
type instance Elem (Seq a) = a
type instance SetElem (Seq a) b = Seq b

instance (Eq a, Arbitrary a) => Arbitrary (Seq a) where
    arbitrary = P.fmap fromList arbitrary

instance Normed (Seq a) where
    {-# INLINE size #-}
    size (Seq s) = Seq.length s

instance (Eq a, ClassicalLogic a) => Eq (Seq a) where
    {-# INLINE (==) #-}
    (Seq a1)==(Seq a2) = F.toList a1==F.toList a2

instance (POrd a, ClassicalLogic a) => POrd (Seq a) where
    {-# INLINE inf #-}
    inf a1 a2 = fromList $ inf (toList a1) (toList a2)

instance (POrd a, ClassicalLogic a) => MinBound (Seq a) where
    {-# INLINE minBound #-}
    minBound = empty

instance Semigroup (Seq a) where
    {-# INLINE (+) #-}
    (Seq a1)+(Seq a2) = Seq $ a1 Seq.>< a2

instance Monoid (Seq a) where
    {-# INLINE zero #-}
    zero = Seq $ Seq.empty

instance (Eq a, ClassicalLogic a) => Container (Seq a) where
    {-# INLINE elem #-}
    elem a (Seq s) = elem a $ F.toList s

    {-# INLINE notElem #-}
    notElem = not elem

instance Constructible (Seq a) where
    {-# INLINE cons #-}
    {-# INLINE snoc #-}
    {-# INLINE singleton #-}
    {-# INLINE fromList1 #-}
    cons a (Seq s) = Seq $ a Seq.<| s
    snoc (Seq s) a = Seq $ s Seq.|> a
    singleton a = Seq $ Seq.singleton a

    fromList1 x xs = Seq $ Seq.fromList (x:xs)

instance (Eq a, ClassicalLogic a) => Foldable (Seq a) where

    {-# INLINE toList #-}
    toList (Seq a) = F.toList a

    {-# INLINE uncons #-}
    uncons (Seq a) = if Seq.null a
        then Nothing
        else Just (Seq.index a 0, Seq $ Seq.drop 1 a)

    {-# INLINE unsnoc #-}
    unsnoc (Seq a) = if Seq.null a
        then Nothing
        else Just (Seq $ Seq.take (Seq.length a-1) a, Seq.index a 0)

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    foldr   f a (Seq s) = F.foldr   f a s
    foldr'  f a (Seq s) = F.foldr'  f a s
    foldr1  f   (Seq s) = F.foldr1  f   s

    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    foldl   f a (Seq s) = F.foldl   f a s
    foldl'  f a (Seq s) = F.foldl'  f a s
    foldl1  f   (Seq s) = F.foldl1  f   s

instance (Eq a, ClassicalLogic a) => Partitionable (Seq a) where
    {-# INLINABLE partition #-}
    partition n (Seq xs) = go xs
        where
            go :: Seq.Seq a -> [Seq a]
            go xs' = if Seq.null xs'
                then []
                else Seq a:go b
                where
                    (a,b) = Seq.splitAt len xs'

            size' = Seq.length xs
            len = size' `div` n
                + if size' `rem` n == 0 then 0 else 1

    {-# INLINABLE partitionInterleaved #-}
    partitionInterleaved n xs = foldl' go (P.replicate n empty) xs
        where
            go (r:rs) x = rs+[r`snoc`x]
            go [] _ = undefined

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Map

newtype Map b a = Map (M.Map (WithPreludeOrd b) (WithPreludeOrd a))
    deriving (Show,NFData)

mkMutable [t| forall b a. Map b a |]

type instance Scalar (Map b a) = Int
type instance Logic (Map b a) = Bool
type instance Index (Map b a) = b
type instance SetIndex (Map b a) b' = Map b' a
type instance Elem (Map b a) = a
type instance SetElem (Map b a) a' = Map b a'

-- misc classes

instance (Eq a, Ord b, Semigroup a, Arbitrary b, Arbitrary a, ClassicalLogic a, ClassicalLogic b) => Arbitrary (Map b a) where
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq b, Eq a, ClassicalLogic b, ClassicalLogic a) => Eq (Map b a) where
    {-# INLINE (==) #-}
    (Map m1)==(Map m2) = m1 P.== m2

instance (Ord b, Eq a, ClassicalLogic b, ClassicalLogic a) => POrd (Map b a) where
    {-# INLINE inf #-}
    inf (Map m1) (Map m2) = Map $ M.differenceWith go (M.intersection m1 m2) m2
        where
            go :: forall b. Eq b => b -> b -> Maybe b
            go a1 a2 = if a1==a2 then Just a1 else Nothing

instance (Ord b, POrd a, ClassicalLogic b, ClassicalLogic a) => MinBound (Map b a) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance (Ord b, ClassicalLogic b) => Semigroup (Map b a) where
    {-# INLINE (+) #-}
    (Map m1)+(Map m2) = Map $ M.union m1 m2

instance (Ord b, ClassicalLogic b) => Monoid (Map b a) where
    {-# INLINE zero #-}
    zero = Map $ M.empty

instance Normed (Map b a) where
    {-# INLINE size #-}
    size (Map m) = M.size m

-- indexed containers

instance (Ord b, Eq a, ClassicalLogic b, ClassicalLogic a) => IxContainer (Map b a) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup b (Map m) = P.fmap unWithPreludeOrd $ M.lookup (WithPreludeOrd b) m
    hasIndex (Map m) b = M.member (WithPreludeOrd b) m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (Map m) = map (\(WithPreludeOrd b,WithPreludeOrd a)->(b,a)) $ M.assocs m
    indices (Map m) = map unWithPreludeOrd $ M.keys m
    values (Map m) = map unWithPreludeOrd $ M.elems m
    imap f (Map m) = Map $ M.mapWithKey (\(WithPreludeOrd b) (WithPreludeOrd a) -> WithPreludeOrd $ f b a) m

instance (Ord b, Eq a, ClassicalLogic b, ClassicalLogic a) => IxConstructible (Map b a) where
    {-# INLINE singletonAt #-}
    singletonAt b a = Map $ M.singleton (WithPreludeOrd b) (WithPreludeOrd a)

    {-# INLINE consAt #-}
    consAt b a (Map m) = Map $ M.insert (WithPreludeOrd b) (WithPreludeOrd a) m

----------------------------------------
-- | This is a thin wrapper around Data.Map.Strict

newtype Map' b a = Map' (MS.Map (WithPreludeOrd b) (WithPreludeOrd a))
    deriving (Show,NFData)

mkMutable [t| forall b a. Map' b a |]

type instance Scalar (Map' b a) = Int
type instance Logic (Map' b a) = Bool
type instance Index (Map' b a) = b
type instance SetIndex (Map' b a) b' = Map' b' a
type instance Elem (Map' b a) = a
type instance SetElem (Map' b a) a' = Map' b a'

-- misc classes

instance (Eq a, Ord b, Semigroup a, Arbitrary b, Arbitrary a, ClassicalLogic a, ClassicalLogic b) => Arbitrary (Map' b a) where
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq b, Eq a, ClassicalLogic a, ClassicalLogic b) => Eq (Map' b a) where
    {-# INLINE (==) #-}
    (Map' m1)==(Map' m2) = m1 P.== m2

instance (Ord b, Eq a, ClassicalLogic a, ClassicalLogic b) => POrd (Map' b a) where
    {-# INLINE inf #-}
    inf (Map' m1) (Map' m2) = Map' $ MS.differenceWith go (MS.intersection m1 m2) m2
        where
            go :: forall b. Eq b => b -> b -> Maybe b
            go a1 a2 = if a1==a2 then Just a1 else Nothing

instance (Ord b, POrd a, ClassicalLogic a, ClassicalLogic b) => MinBound (Map' b a) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance (Ord b, ClassicalLogic b) => Semigroup (Map' b a) where
    {-# INLINE (+) #-}
    (Map' m1)+(Map' m2) = Map' $ MS.union m1 m2

instance (Ord b, ClassicalLogic b) => Monoid (Map' b a) where
    {-# INLINE zero #-}
    zero = Map' $ MS.empty

instance Normed (Map' b a) where
    {-# INLINE size #-}
    size (Map' m) = MS.size m

-- indexed containers

instance (Ord b, Eq a, ClassicalLogic a, ClassicalLogic b) => IxContainer (Map' b a) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup b (Map' m) = P.fmap unWithPreludeOrd $ MS.lookup (WithPreludeOrd b) m
    hasIndex (Map' m) b = MS.member (WithPreludeOrd b) m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (Map' m) = map (\(WithPreludeOrd b,WithPreludeOrd a)->(b,a)) $ MS.assocs m
    indices (Map' m) = map unWithPreludeOrd $ MS.keys m
    values (Map' m) = map unWithPreludeOrd $ MS.elems m
    imap f (Map' m) = Map' $ MS.mapWithKey (\(WithPreludeOrd b) (WithPreludeOrd a) -> WithPreludeOrd $ f b a) m

instance (Ord b, Eq a, ClassicalLogic a, ClassicalLogic b) => IxConstructible (Map' b a) where
    {-# INLINE singletonAt #-}
    singletonAt b a = Map' $ MS.singleton (WithPreludeOrd b) (WithPreludeOrd a)

    {-# INLINE consAt #-}
    consAt b a (Map' m) = Map' $ MS.insert (WithPreludeOrd b) (WithPreludeOrd a) m

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.IntMap

newtype IntMap a = IntMap (IM.IntMap (WithPreludeOrd a))
    deriving (Read,Show,NFData)

mkMutable [t| forall a. IntMap a |]

type instance Scalar (IntMap a) = Int
type instance Logic (IntMap a) = Bool
type instance Index (IntMap a) = IM.Key
type instance Elem (IntMap a) = a
type instance SetElem (IntMap a) a' = IntMap a'

-- misc classes

instance (Eq a, Semigroup a, Arbitrary a, ClassicalLogic a) => Arbitrary (IntMap a) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq a, ClassicalLogic a) => Eq (IntMap a) where
    {-# INLINE (==) #-}
    (IntMap m1)==(IntMap m2) = m1 P.== m2

instance (Eq a, ClassicalLogic a) => POrd (IntMap a) where
    {-# INLINE inf #-}
    inf (IntMap m1) (IntMap m2) = IntMap $ IM.differenceWith go (IM.intersection m1 m2) m2
        where
            go :: forall b. Eq b => b -> b -> Maybe b
            go a1 a2 = if a1==a2 then Just a1 else Nothing

instance (POrd a, ClassicalLogic a) => MinBound (IntMap a) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Semigroup (IntMap a) where
    {-# INLINE (+) #-}
    (IntMap m1)+(IntMap m2) = IntMap $ IM.union m1 m2

instance Monoid (IntMap a) where
    {-# INLINE zero #-}
    zero = IntMap $ IM.empty

instance Normed (IntMap a) where
    {-# INLINE size #-}
    size (IntMap m) = IM.size m

-- indexed container

instance (Eq a, ClassicalLogic a) => IxConstructible (IntMap a) where
    {-# INLINE singletonAt #-}
    {-# INLINE consAt #-}
    singletonAt b a = IntMap $ IM.singleton b (WithPreludeOrd a)
    consAt b a (IntMap m) = IntMap $ IM.insert b (WithPreludeOrd a) m

instance (Eq a, ClassicalLogic a) => IxContainer (IntMap a) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup b (IntMap m) = P.fmap unWithPreludeOrd $ IM.lookup b m
    hasIndex (IntMap m) b = IM.member b m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (IntMap m) = map (\(b,WithPreludeOrd a)->(b,a)) $ IM.assocs m
    indices (IntMap m) = IM.keys m
    values (IntMap m) = map unWithPreludeOrd $ IM.elems m
    imap f (IntMap m) = IntMap $ IM.mapWithKey (\b (WithPreludeOrd a) -> WithPreludeOrd $ f b a) m

----------------------------------------
-- | This is a thin wrapper around Data.IntMap.Strict

newtype IntMap' a = IntMap' (IMS.IntMap (WithPreludeOrd a))
    deriving (Read,Show,NFData)

mkMutable [t| forall a. IntMap' a |]

type instance Scalar (IntMap' a) = Int
type instance Logic (IntMap' a) = Bool
type instance Index (IntMap' a) = IMS.Key
type instance Elem (IntMap' a) = a
type instance SetElem (IntMap' a) a' = IntMap' a'

-- misc classes

instance (Eq a, Semigroup a, Arbitrary a, ClassicalLogic a) => Arbitrary (IntMap' a) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq a, ClassicalLogic a) => Eq (IntMap' a) where
    {-# INLINE (==) #-}
    (IntMap' m1)==(IntMap' m2) = m1 P.== m2

instance (Eq a, ClassicalLogic a) => POrd (IntMap' a) where
    {-# INLINE inf #-}
    inf (IntMap' m1) (IntMap' m2) = IntMap' $ IMS.differenceWith go (IMS.intersection m1 m2) m2
        where
            go :: forall b. Eq b => b -> b -> Maybe b
            go a1 a2 = if a1==a2 then Just a1 else Nothing

instance (POrd a, ClassicalLogic a) => MinBound (IntMap' a) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Semigroup (IntMap' a) where
    {-# INLINE (+) #-}
    (IntMap' m1)+(IntMap' m2) = IntMap' $ IMS.union m1 m2

instance Monoid (IntMap' a) where
    {-# INLINE zero #-}
    zero = IntMap' $ IMS.empty

instance Normed (IntMap' a) where
    {-# INLINE size #-}
    size (IntMap' m) = IMS.size m

-- container

instance (Eq a, ClassicalLogic a) => IxConstructible (IntMap' a) where
    {-# INLINABLE singletonAt #-}
    {-# INLINABLE consAt #-}
    singletonAt b a = IntMap' $ IMS.singleton b (WithPreludeOrd a)
    consAt b a (IntMap' m) = IntMap' $ IMS.insert b (WithPreludeOrd a) m

instance (Eq a, ClassicalLogic a) => IxContainer (IntMap' a) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup b (IntMap' m) = P.fmap unWithPreludeOrd $ IMS.lookup b m
    hasIndex (IntMap' m) b = IMS.member b m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (IntMap' m) = map (\(b,WithPreludeOrd a)->(b,a)) $ IMS.assocs m
    indices (IntMap' m) = IMS.keys m
    values (IntMap' m) = map unWithPreludeOrd $ IMS.elems m
    imap f (IntMap' m) = IntMap' $ IMS.mapWithKey (\b (WithPreludeOrd a) -> WithPreludeOrd $ f b a) m

-------------------------------------------------------------------------------
-- | This is a thin wrapper around the container's set type

newtype Set a = Set (Set.Set (WithPreludeOrd a))
    deriving (Show,NFData)

mkMutable [t| forall a. Set a |]

instance (Ord a, Arbitrary a, ClassicalLogic a) => Arbitrary (Set a) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromList arbitrary

type instance Scalar (Set a) = Int
type instance Logic (Set a) = Logic a
type instance Elem (Set a) = a
type instance SetElem (Set a) b = Set b

instance Normed (Set a) where
    {-# INLINE size #-}
    size (Set s) = Set.size s

instance (Eq a, ClassicalLogic a) => Eq (Set a) where
    {-# INLINE (==) #-}
    (Set s1)==(Set s2) = s1'==s2'
        where
            s1' = removeWithPreludeOrd $ Set.toList s1
            s2' = removeWithPreludeOrd $ Set.toList s2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd (WithPreludeOrd x:xs) = x:removeWithPreludeOrd xs

instance (Ord a, ClassicalLogic a) => POrd (Set a) where
    {-# INLINE inf #-}
    inf (Set s1) (Set s2) = Set $ Set.intersection s1 s2

instance (Ord a, ClassicalLogic a) => MinBound (Set a) where
    {-# INLINE minBound #-}
    minBound = Set $ Set.empty

instance (Ord a, ClassicalLogic a) => Lattice (Set a) where
    {-# INLINE sup #-}
    sup (Set s1) (Set s2) = Set $ Set.union s1 s2

instance (Ord a, ClassicalLogic a) => Semigroup (Set a) where
    {-# INLINE (+) #-}
    (Set s1)+(Set s2) = Set $ Set.union s1 s2

instance (Ord a, ClassicalLogic a) => Monoid (Set a) where
    {-# INLINE zero #-}
    zero = Set $ Set.empty

instance (Ord a, ClassicalLogic a) => Abelian (Set a)

instance (Ord a, ClassicalLogic a) => Container (Set a) where
    {-# INLINE elem #-}
    {-# INLINE notElem #-}
    elem a (Set s) = Set.member (WithPreludeOrd a) s
    notElem a (Set s) = not $ Set.member (WithPreludeOrd a) s

instance (Ord a, ClassicalLogic a) => Constructible (Set a) where
    {-# INLINE singleton #-}
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

    {-# INLINE fromList1 #-}
    fromList1 a as = Set $ Set.fromList $ map WithPreludeOrd (a:as)

instance (Ord a, ClassicalLogic a) => Foldable (Set a) where
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    foldl   f a (Set s) = Set.foldl   (\a' (WithPreludeOrd a) -> f a' a) a s
    foldl'  f a (Set s) = Set.foldl'  (\a' (WithPreludeOrd a) -> f a' a) a s
    foldr  f a (Set s) = Set.foldr  (\(WithPreludeOrd a) a' -> f a a') a s
    foldr' f a (Set s) = Set.foldr' (\(WithPreludeOrd a) a' -> f a a') a s

-- |
--
-- FIXME: implement this in terms of @Lexical@ and @Set@
--
-- FIXME: add the @Constrained@ Monad
data LexSet a where
    LexSet :: (Ord a, ClassicalLogic a) => Set a -> LexSet a

mkMutable [t| forall a. LexSet a |]

type instance Scalar (LexSet a) = Int
type instance Logic (LexSet a) = Bool
type instance Elem (LexSet a) = a
type instance SetElem (LexSet a) b = LexSet b

instance Show a => Show (LexSet a) where
    show (LexSet s) = "LexSet "++show (toList s)

instance Eq (LexSet a) where
    (LexSet a1)==(LexSet a2) = Lexical a1==Lexical a2

instance POrd (LexSet a) where
    inf (LexSet a1) (LexSet a2) = LexSet $ unLexical $ inf (Lexical a1) (Lexical a2)
    (LexSet a1) <  (LexSet a2) = Lexical a1 <  Lexical a2
    (LexSet a1) <= (LexSet a2) = Lexical a1 <= Lexical a2

instance Lattice (LexSet a) where
    sup (LexSet a1) (LexSet a2) = LexSet $ unLexical $ sup (Lexical a1) (Lexical a2)
    (LexSet a1) >  (LexSet a2) = Lexical a1 >  Lexical a2
    (LexSet a1) >= (LexSet a2) = Lexical a1 >= Lexical a2

instance Ord (LexSet a)

instance Semigroup (LexSet a) where
    (LexSet a1)+(LexSet a2) = LexSet $ a1+a2

instance (Ord a, ClassicalLogic a) => Monoid (LexSet a) where
    zero = LexSet zero

instance (Ord a, ClassicalLogic a) => Container (LexSet a) where
    elem x (LexSet s) = elem x s

instance (Ord a, ClassicalLogic a) => Constructible (LexSet a) where
    fromList1 a as = LexSet $ fromList1 a as

instance (Ord a, ClassicalLogic a) => Normed (LexSet a) where
    size (LexSet s) = size s

instance (Ord a, ClassicalLogic a) => MinBound (LexSet a) where
    minBound = zero

instance (Ord a, ClassicalLogic a) => Foldable (LexSet a) where
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
    (LexSet _)>>(LexSet b) = LexSet b
