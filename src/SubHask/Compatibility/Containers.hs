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
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import SubHask.Monad
import SubHask.TemplateHaskell.Deriving

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

instance Eq a => Eq_ (Seq a) where
    {-# INLINE (==) #-}
    (Seq a1)==(Seq a2) = F.toList a1==F.toList a2

instance POrd a => POrd_ (Seq a) where
    {-# INLINE inf #-}
    inf a1 a2 = fromList $ inf (toList a1) (toList a2)

instance POrd a => MinBound_ (Seq a) where
    {-# INLINE minBound #-}
    minBound = empty

instance Semigroup (Seq a) where
    {-# INLINE (+) #-}
    (Seq a1)+(Seq a2) = Seq $ a1 Seq.>< a2

instance Monoid (Seq a) where
    {-# INLINE zero #-}
    zero = Seq $ Seq.empty

instance Eq a => Container (Seq a) where
    {-# INLINE elem #-}
    elem e (Seq a) = elem e $ F.toList a

    {-# INLINE notElem #-}
    notElem = not elem

instance Constructible (Seq a) where
    {-# INLINE cons #-}
    {-# INLINE snoc #-}
    {-# INLINE singleton #-}
    {-# INLINE fromList1 #-}
    cons e (Seq a) = Seq $ e Seq.<| a
    snoc (Seq a) e = Seq $ a Seq.|> e
    singleton e = Seq $ Seq.singleton e

    fromList1 x xs = Seq $ Seq.fromList (x:xs)

instance Eq_ a => Foldable (Seq a) where

    {-# INLINE toList #-}
    toList (Seq a) = F.toList a

    {-# INLINE uncons #-}
    uncons (Seq a) = if Seq.null a
        then Nothing
        else Just (Seq.index a 0, Seq $ Seq.drop 1 a)

    {-# INLINE unsnoc #-}
    unsnoc (Seq e) = if Seq.null e
        then Nothing
        else Just (Seq $ Seq.take (Seq.length e-1) e, Seq.index e 0)

    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    {-# INLINE foldr1 #-}
    foldr   f e (Seq a) = F.foldr   f e a
    foldr'  f e (Seq a) = F.foldr'  f e a
    foldr1  f   (Seq a) = F.foldr1  f   a

    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldl1 #-}
    foldl   f e (Seq a) = F.foldl   f e a
    foldl'  f e (Seq a) = F.foldl'  f e a
    foldl1  f   (Seq a) = F.foldl1  f   a

instance (Eq_ a) => Partitionable (Seq a) where
    {-# INLINABLE partition #-}
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

    {-# INLINABLE partitionInterleaved #-}
    partitionInterleaved n xs = foldl' go (P.replicate n empty) xs
        where
            go (r:rs) x = rs+[r`snoc`x]

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.Map

newtype Map i e = Map (M.Map (WithPreludeOrd i) (WithPreludeOrd e))
    deriving (Show,NFData)

mkMutable [t| forall i e. Map i e |]

type instance Scalar (Map i e) = Int
type instance Logic (Map i e) = Bool
type instance Index (Map i e) = i
type instance SetIndex (Map i e) i' = Map i' e
type instance Elem (Map i e) = e
type instance SetElem (Map i e) e' = Map i e'

-- misc classes

instance (Eq e, Ord i, Semigroup e, Arbitrary i, Arbitrary e) => Arbitrary (Map i e) where
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq i, Eq e) => Eq_ (Map i e) where
    {-# INLINE (==) #-}
    (Map m1)==(Map m2) = m1 P.== m2

instance (Ord i, Eq e) => POrd_ (Map i e) where
    {-# INLINE inf #-}
    inf (Map m1) (Map m2) = Map $ M.differenceWith go (M.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance (Ord i, POrd e) => MinBound_ (Map i e) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Ord i => Semigroup (Map i e) where
    {-# INLINE (+) #-}
    (Map m1)+(Map m2) = Map $ M.union m1 m2

instance Ord i => Monoid (Map i e) where
    {-# INLINE zero #-}
    zero = Map $ M.empty

instance Normed (Map i e) where
    {-# INLINE size #-}
    size (Map m) = M.size m

-- indexed containers

instance (Ord i, Eq e) => IxContainer (Map i e) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup i (Map m) = P.fmap unWithPreludeOrd $ M.lookup (WithPreludeOrd i) m
    hasIndex (Map m) i = M.member (WithPreludeOrd i) m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (Map m) = map (\(WithPreludeOrd i,WithPreludeOrd e)->(i,e)) $ M.assocs m
    indices (Map m) = map unWithPreludeOrd $ M.keys m
    values (Map m) = map unWithPreludeOrd $ M.elems m
    imap f (Map m) = Map $ M.mapWithKey (\(WithPreludeOrd i) (WithPreludeOrd e) -> WithPreludeOrd $ f i e) m

instance (Ord i, Eq e) => IxConstructible (Map i e) where
    {-# INLINE singletonAt #-}
    singletonAt i e = Map $ M.singleton (WithPreludeOrd i) (WithPreludeOrd e)

    {-# INLINE consAt #-}
    consAt i e (Map m) = Map $ M.insert (WithPreludeOrd i) (WithPreludeOrd e) m

----------------------------------------
-- | This is a thin wrapper around Data.Map.Strict

newtype Map' i e = Map' (MS.Map (WithPreludeOrd i) (WithPreludeOrd e))
    deriving (Show,NFData)

mkMutable [t| forall i e. Map' i e |]

type instance Scalar (Map' i e) = Int
type instance Logic (Map' i e) = Bool
type instance Index (Map' i e) = i
type instance SetIndex (Map' i e) i' = Map' i' e
type instance Elem (Map' i e) = e
type instance SetElem (Map' i e) e' = Map' i e'

-- misc classes

instance (Eq e, Ord i, Semigroup e, Arbitrary i, Arbitrary e) => Arbitrary (Map' i e) where
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq i, Eq e) => Eq_ (Map' i e) where
    {-# INLINE (==) #-}
    (Map' m1)==(Map' m2) = m1 P.== m2

instance (Ord i, Eq e) => POrd_ (Map' i e) where
    {-# INLINE inf #-}
    inf (Map' m1) (Map' m2) = Map' $ MS.differenceWith go (MS.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance (Ord i, POrd e) => MinBound_ (Map' i e) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Ord i => Semigroup (Map' i e) where
    {-# INLINE (+) #-}
    (Map' m1)+(Map' m2) = Map' $ MS.union m1 m2

instance Ord i => Monoid (Map' i e) where
    {-# INLINE zero #-}
    zero = Map' $ MS.empty

instance Normed (Map' i e) where
    {-# INLINE size #-}
    size (Map' m) = MS.size m

-- indexed containers

instance (Ord i, Eq e) => IxContainer (Map' i e) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup i (Map' m) = P.fmap unWithPreludeOrd $ MS.lookup (WithPreludeOrd i) m
    hasIndex (Map' m) i = MS.member (WithPreludeOrd i) m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (Map' m) = map (\(WithPreludeOrd i,WithPreludeOrd e)->(i,e)) $ MS.assocs m
    indices (Map' m) = map unWithPreludeOrd $ MS.keys m
    values (Map' m) = map unWithPreludeOrd $ MS.elems m
    imap f (Map' m) = Map' $ MS.mapWithKey (\(WithPreludeOrd i) (WithPreludeOrd e) -> WithPreludeOrd $ f i e) m

instance (Ord i, Eq e) => IxConstructible (Map' i e) where
    {-# INLINE singletonAt #-}
    singletonAt i e = Map' $ MS.singleton (WithPreludeOrd i) (WithPreludeOrd e)

    {-# INLINE consAt #-}
    consAt i e (Map' m) = Map' $ MS.insert (WithPreludeOrd i) (WithPreludeOrd e) m

-------------------------------------------------------------------------------
-- | This is a thin wrapper around Data.IntMap

newtype IntMap e = IntMap (IM.IntMap (WithPreludeOrd e))
    deriving (Read,Show,NFData)

mkMutable [t| forall a. IntMap a |]

type instance Scalar (IntMap e) = Int
type instance Logic (IntMap e) = Bool
type instance Index (IntMap e) = IM.Key
type instance Elem (IntMap e) = e
type instance SetElem (IntMap e) e' = IntMap e'

-- misc classes

instance (Eq e, Semigroup e, Arbitrary e) => Arbitrary (IntMap e) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq e) => Eq_ (IntMap e) where
    {-# INLINE (==) #-}
    (IntMap m1)==(IntMap m2) = m1 P.== m2

instance (Eq e) => POrd_ (IntMap e) where
    {-# INLINE inf #-}
    inf (IntMap m1) (IntMap m2) = IntMap $ IM.differenceWith go (IM.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance (POrd e) => MinBound_ (IntMap e) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Semigroup (IntMap e) where
    {-# INLINE (+) #-}
    (IntMap m1)+(IntMap m2) = IntMap $ IM.union m1 m2

instance Monoid (IntMap e) where
    {-# INLINE zero #-}
    zero = IntMap $ IM.empty

instance Normed (IntMap e) where
    {-# INLINE size #-}
    size (IntMap m) = IM.size m

-- indexed container

instance (Eq e) => IxConstructible (IntMap e) where
    {-# INLINE singletonAt #-}
    {-# INLINE consAt #-}
    singletonAt i e = IntMap $ IM.singleton i (WithPreludeOrd e)
    consAt i e (IntMap m) = IntMap $ IM.insert i (WithPreludeOrd e) m

instance (Eq e) => IxContainer (IntMap e) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup i (IntMap m) = P.fmap unWithPreludeOrd $ IM.lookup i m
    hasIndex (IntMap m) i = IM.member i m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (IntMap m) = map (\(i,WithPreludeOrd e)->(i,e)) $ IM.assocs m
    indices (IntMap m) = IM.keys m
    values (IntMap m) = map unWithPreludeOrd $ IM.elems m
    imap f (IntMap m) = IntMap $ IM.mapWithKey (\i (WithPreludeOrd e) -> WithPreludeOrd $ f i e) m

----------------------------------------
-- | This is a thin wrapper around Data.IntMap.Strict

newtype IntMap' e = IntMap' (IMS.IntMap (WithPreludeOrd e))
    deriving (Read,Show,NFData)

mkMutable [t| forall a. IntMap' a |]

type instance Scalar (IntMap' e) = Int
type instance Logic (IntMap' e) = Bool
type instance Index (IntMap' e) = IMS.Key
type instance Elem (IntMap' e) = e
type instance SetElem (IntMap' e) e' = IntMap' e'

-- misc classes

instance (Eq e, Semigroup e, Arbitrary e) => Arbitrary (IntMap' e) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromIxList arbitrary

-- comparisons

instance (Eq e) => Eq_ (IntMap' e) where
    {-# INLINE (==) #-}
    (IntMap' m1)==(IntMap' m2) = m1 P.== m2

instance (Eq e) => POrd_ (IntMap' e) where
    {-# INLINE inf #-}
    inf (IntMap' m1) (IntMap' m2) = IntMap' $ IMS.differenceWith go (IMS.intersection m1 m2) m2
        where
            go v1 v2 = if v1==v2 then Just v1 else Nothing

instance (POrd e) => MinBound_ (IntMap' e) where
    {-# INLINE minBound #-}
    minBound = zero

-- algebra

instance Semigroup (IntMap' e) where
    {-# INLINE (+) #-}
    (IntMap' m1)+(IntMap' m2) = IntMap' $ IMS.union m1 m2

instance Monoid (IntMap' e) where
    {-# INLINE zero #-}
    zero = IntMap' $ IMS.empty

instance Normed (IntMap' e) where
    {-# INLINE size #-}
    size (IntMap' m) = IMS.size m

-- container

instance (Eq e) => IxConstructible (IntMap' e) where
    {-# INLINABLE singletonAt #-}
    {-# INLINABLE consAt #-}
    singletonAt i e = IntMap' $ IMS.singleton i (WithPreludeOrd e)
    consAt i e (IntMap' m) = IntMap' $ IMS.insert i (WithPreludeOrd e) m

instance (Eq e) => IxContainer (IntMap' e) where
    {-# INLINE lookup #-}
    {-# INLINE hasIndex #-}
    lookup i (IntMap' m) = P.fmap unWithPreludeOrd $ IMS.lookup i m
    hasIndex (IntMap' m) i = IMS.member i m

    {-# INLINE toIxList #-}
    {-# INLINE indices #-}
    {-# INLINE values #-}
    {-# INLINE imap #-}
    toIxList (IntMap' m) = map (\(i,WithPreludeOrd e)->(i,e)) $ IMS.assocs m
    indices (IntMap' m) = IMS.keys m
    values (IntMap' m) = map unWithPreludeOrd $ IMS.elems m
    imap f (IntMap' m) = IntMap' $ IMS.mapWithKey (\i (WithPreludeOrd e) -> WithPreludeOrd $ f i e) m

-------------------------------------------------------------------------------
-- | This is a thin wrapper around the container's set type

newtype Set a = Set (Set.Set (WithPreludeOrd a))
    deriving (Show,NFData)

mkMutable [t| forall a. Set a |]

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    {-# INLINABLE arbitrary #-}
    arbitrary = P.fmap fromList arbitrary

type instance Scalar (Set a) = Int
type instance Logic (Set a) = Logic a
type instance Elem (Set a) = a
type instance SetElem (Set a) b = Set b

instance Normed (Set a) where
    {-# INLINE size #-}
    size (Set s) = Set.size s

instance Eq a => Eq_ (Set a) where
    {-# INLINE (==) #-}
    (Set s1)==(Set s2) = s1'==s2'
        where
            s1' = removeWithPreludeOrd $ Set.toList s1
            s2' = removeWithPreludeOrd $ Set.toList s2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd (WithPreludeOrd x:xs) = x:removeWithPreludeOrd xs

instance Ord a => POrd_ (Set a) where
    {-# INLINE inf #-}
    inf (Set s1) (Set s2) = Set $ Set.intersection s1 s2

instance Ord a => MinBound_ (Set a) where
    {-# INLINE minBound #-}
    minBound = Set $ Set.empty

instance Ord a => Lattice_ (Set a) where
    {-# INLINE sup #-}
    sup (Set s1) (Set s2) = Set $ Set.union s1 s2

instance Ord a => Semigroup (Set a) where
    {-# INLINE (+) #-}
    (Set s1)+(Set s2) = Set $ Set.union s1 s2

instance Ord a => Monoid (Set a) where
    {-# INLINE zero #-}
    zero = Set $ Set.empty

instance Ord a => Abelian (Set a)

instance Ord a => Container (Set a) where
    {-# INLINE elem #-}
    {-# INLINE notElem #-}
    elem a (Set s) = Set.member (WithPreludeOrd a) s
    notElem a (Set s) = not $ Set.member (WithPreludeOrd a) s

instance Ord a => Constructible (Set a) where
    {-# INLINE singleton #-}
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

    {-# INLINE fromList1 #-}
    fromList1 a as = Set $ Set.fromList $ map WithPreludeOrd (a:as)

instance Ord a => Foldable (Set a) where
    {-# INLINE foldl #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldr #-}
    {-# INLINE foldr' #-}
    foldl   f a (Set s) = Set.foldl   (\a (WithPreludeOrd e) -> f a e) a s
    foldl'  f a (Set s) = Set.foldl'  (\a (WithPreludeOrd e) -> f a e) a s
    foldr  f a (Set s) = Set.foldr  (\(WithPreludeOrd e) a -> f e a) a s
    foldr' f a (Set s) = Set.foldr' (\(WithPreludeOrd e) a -> f e a) a s

-- |
--
-- FIXME: implement this in terms of @Lexical@ and @Set@
--
-- FIXME: add the @Constrained@ Monad
data LexSet a where
    LexSet :: Ord a => Set a -> LexSet a

mkMutable [t| forall a. LexSet a |]

type instance Scalar (LexSet a) = Int
type instance Logic (LexSet a) = Bool
type instance Elem (LexSet a) = a
type instance SetElem (LexSet a) b = LexSet b

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

