{-# LANGUAGE RebindableSyntax,QuasiQuotes #-}

-- | This module contains the container algebras
module SubHask.Algebra.Container
    where

import Control.Monad
import GHC.Prim
import Control.Monad
import GHC.TypeLits
import qualified Prelude as P

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import SubHask.Algebra
import SubHask.Algebra.Ord
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------

-- | The Jaccard distance.
--
-- See <https://en.wikipedia.org/wiki/Jaccard_index wikipedia> for more detail.
newtype Jaccard a = Jaccard a

deriveHierarchy ''Jaccard
    [ ''Ord
    , ''Boolean
    , ''Ring
    , ''FreeMonoid
    ]

instance
    ( Lattice a
    , Field (Scalar a)
    , Normed a
    , Eq a
    ) => MetricSpace (Jaccard a)
        where
    distance (Jaccard xs) (Jaccard ys) = 1 - abs (xs && ys) / abs (xs || ys)

----------------------------------------

-- | The Hamming distance.
--
-- See <https://en.wikipedia.org/wiki/Hamming_distance wikipedia> for more detail.
newtype Hamming a = Hamming a

deriveHierarchy ''Hamming
    [ ''Ord
    , ''Boolean
    , ''Ring
    , ''FreeMonoid
    ]

instance
    ( Foldable a
    , Eq (Elem a)
    , Eq a
    , Ord (Scalar a)
    , Ring (Scalar a)
    ) => MetricSpace (Hamming a)
        where

    {-# INLINE distance #-}
    distance (Hamming xs) (Hamming ys) =
        {-# SCC distance_Hamming #-}
        go (toList xs) (toList ys) 0
        where
            go [] [] i = i
            go xs [] i = i + fromIntegral (abs xs)
            go [] ys i = i + fromIntegral (abs ys)
            go (x:xs) (y:ys) i = go xs ys $ i + if x==y
                then 0
                else 1

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError (Hamming xs) (Hamming ys) dist =
        {-# SCC isFartherThanWithDistance_Hamming #-}
        go (toList xs) (toList ys) 0
        where
            go xs ys tot = if tot > dist
                then errorVal
                else go_ xs ys tot
                where
                    go_ (x:xs) (y:ys) i = go xs ys $ i + if x==y
                        then 0
                        else 1
                    go_ [] [] i = i
                    go_ xs [] i = i + fromIntegral (abs xs)
                    go_ [] ys i = i + fromIntegral (abs ys)

----------------------------------------

-- | The Levenshtein distance is a type of edit distance, but it is often referred to as THE edit distance.
--
-- FIXME: The implementation could be made faster in a number of ways;
-- for example, the Hamming distance is a lower bound on the Levenshtein distance
--
-- See <https://en.wikipedia.org/wiki/Levenshtein_distance wikipedia> for more detail.
newtype Levenshtein a = Levenshtein a

deriveHierarchy ''Levenshtein
    [ ''Ord
    , ''Boolean
    , ''Ring
    , ''FreeMonoid
    ]

instance
    ( Foldable a
    , Memoizable (Elem a)
    , Eq (Elem a)
    , Eq a
    , Show a
    , Ord (Scalar a)
    , Ring (Scalar a)
    , MaxBound (Scalar a)
    ) => MetricSpace (Levenshtein a)
        where

    {-# INLINE distance #-}
    distance (Levenshtein xs) (Levenshtein ys) =
        {-# SCC distance_Levenshtein #-}
        fromIntegral $ dist (toList xs) (toList ys)

-- | this function stolen from
-- https://www.haskell.org/haskellwiki/Edit_distance
dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0
        then mainDiag
	    else if lab > 0
            then lowers P.!! (lab - 1)
	        else{- < 0 -}   uppers P.!! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z

----------------------------------------

-- | Compensated sums are more accurate for floating point math
--
-- FIXME: There are many different types of compensated sums, they should be implemented too.
--
-- FIXME: Is this the best representation for compensated sums?
-- The advantage is that we can make any algorithm work in a compensated or uncompensated manner by just changing the types.
-- This is closely related to the measure theory containers work.
--
-- See, e.g. <https://en.wikipedia.org/wiki/Kahan_summation_algorithm kahan summation> for more detail.
newtype Uncompensated s = Uncompensated s

deriveHierarchy ''Uncompensated
    [ ''Ord
    , ''Boolean
    , ''InnerProductSpace
    , ''Ring
    , ''Unfoldable
    ]

instance Foldable s => Foldable (Uncompensated s) where
    unCons (Uncompensated s) = case unCons s of
        Nothing -> Nothing
        Just (x,xs) -> Just (x, Uncompensated xs)

    unSnoc (Uncompensated s) = case unSnoc s of
        Nothing -> Nothing
        Just (xs,x) -> Just (Uncompensated xs,x)

    foldMap f   (Uncompensated s) = foldMap f   s
    foldr   f a (Uncompensated s) = foldr   f a s
    foldr'  f a (Uncompensated s) = foldr'  f a s
    foldr1  f   (Uncompensated s) = foldr1  f   s
    foldr1' f   (Uncompensated s) = foldr1' f   s
    foldl   f a (Uncompensated s) = foldl   f a s
    foldl'  f a (Uncompensated s) = foldl'  f a s
    foldl1  f   (Uncompensated s) = foldl1  f   s
    foldl1' f   (Uncompensated s) = foldl1' f   s

    sum = foldl' (+) zero


----------------------------------------

-- | Lexical ordering of foldable types.
--
-- NOTE: The default ordering for containers is the partial ordering by inclusion.
-- In most cases this makes more sense intuitively.
-- But this is NOT the ordering in the Prelude, because the Prelude does not have partial orders.
-- Therefore, in the prelude, @@"abc" < "def"@@, but for us, "abc" and "def" are incomparable "PNA".
-- The Lexical newtype gives us the total ordering provided by the Prelude.
--
-- FIXME: there are more container orderings that probably deserve implementation
newtype Lexical a = Lexical a

deriveHierarchy ''Lexical [ ''Foldable, ''Unfoldable, ''Monoid ]

class (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a
instance (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a

instance (SimpleContainerLogic a, Eq_ (Elem a), Foldable a) => Eq_ (Lexical a) where
    (Lexical a1)==(Lexical a2) = toList a1==toList a2

instance (SimpleContainerLogic a, POrd_ (Elem a), Foldable a, Unfoldable a) => POrd_ (Lexical a) where
    inf (Lexical a1) (Lexical a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = inf x y:go xs ys
            go _ _ = []

instance (SimpleContainerLogic a, POrd_ (Elem a), Foldable a, Unfoldable a) => MinBound_ (Lexical a) where
    minBound = Lexical zero

instance (SimpleContainerLogic a, Lattice_ (Elem a), Foldable a, Unfoldable a) => Lattice_ (Lexical a) where
    sup (Lexical a1) (Lexical a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = sup x y:go xs ys
            go xs [] = xs
            go [] ys = ys

-------------------------------------------------------------------------------
-- compatibility layer types

-- | An indexed vector can be thought of as a sparse vector.
--
-- FIXME: this had a problem with deriveHierarchy; not sure why
--
-- FIXME: figure out the semantics of how we want to represent maps;
-- there's lots more possible implementations of IndexedVector;
-- pretty sure they can be derived from this one using the correct monoid, but we need the implementation
newtype IndexedVector k v = IndexedVector { unsafeGetMap :: Map.Map (WithPreludeOrd k) v }
    deriving (Read,Show)

instance (Eq v, Ord k, Semigroup v, Arbitrary k, Arbitrary v) => Arbitrary (IndexedVector k v) where
    arbitrary = liftM fromIndexedList arbitrary

type instance Scalar (IndexedVector k v) = Scalar v
type instance Logic (IndexedVector k v) = Logic v
type instance Elem (IndexedVector k v) = v

-- | This is the L2 norm of the vector.
instance (Floating (Scalar v), IsScalar v, Ord v) => Normed (IndexedVector k v) where
    {-# INLINE abs #-}
    abs (IndexedVector m) =
        {-# SCC abs_IndexedVector #-}
        sqrt $ sum $ map (**2) $ Map.elems m

instance (Floating (Scalar v), IsScalar v, Ord v, Ord k) => MetricSpace (IndexedVector k v) where
    {-# INLINE distance #-}
    distance (IndexedVector m1) (IndexedVector m2) =
        {-# SCC distance_IndexedVector #-}
        sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
        where
            go tot [] [] = tot
            go tot [] ((k,v):xs) = go (tot+v*v) [] xs
            go tot ((k,v):xs) [] = go (tot+v*v) [] xs

            go tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
                EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
                LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
                GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1

    isFartherThanWithDistanceCanError (IndexedVector m1) (IndexedVector m2) dist =
        {-# SCC isFartherThanWithDistanceCanError_IndexedVector #-}
        sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
        where
            dist2 = dist*dist

            go tot [] [] = tot
            go tot xs ys = if tot > dist2
                then errorVal
                else go' tot xs ys

            go' tot [] ((k,v):xs) = go (tot+v*v) [] xs
            go' tot ((k,v):xs) [] = go (tot+v*v) [] xs

            go' tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
                EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
                LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
                GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1

instance (Eq k, Eq v) => Eq_ (IndexedVector k v) where
    (IndexedVector m1)==(IndexedVector m2) = m1' == m2'
        where
            m1' = removeWithPreludeOrd $ Map.toList m1
            m2' = removeWithPreludeOrd $ Map.toList m2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd ((WithPreludeOrd k,v):xs) = (k,v):removeWithPreludeOrd xs

instance (Eq v, Ord k, Lattice_ v) => POrd_ (IndexedVector k v) where
    inf (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.intersectionWith (inf) m1 m2

instance (Eq v, Ord k, Lattice_ v) => Lattice_ (IndexedVector k v) where
    sup (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.unionWith (sup) m1 m2

--     | FIXME: is this correct?
--     pcompare (IndexedVector m1) (IndexedVector m2) = go (Map.assocs m1) (Map.assocs m2)
--         where
--             go [] [] = PEQ
--             go [] _  = PLT
--             go _  [] = PGT
--             go ((k1,v1):xs1) ((k2,v2):xs2) = case pcompare k1 k2 of
--                 PNA -> PNA
--                 PLT -> PLT
--                 PGT -> PGT
--                 PEQ -> case pcompare v1 v2 of
--                     PNA -> PNA
--                     PLT -> PLT
--                     PGT -> PGT
--                     PEQ -> go xs1 xs2


instance (Eq v, Ord k, Semigroup v) => Semigroup (IndexedVector k v) where
    (IndexedVector m1)+(IndexedVector m2) = IndexedVector $ Map.unionWith (+) m1 m2

instance (Eq v, Ord k, Semigroup v) => Monoid (IndexedVector k v) where
    zero = IndexedVector $ Map.empty

instance (Eq v, Ord k, Abelian v) => Abelian (IndexedVector k v)

instance (Eq v, Ord k, Semigroup v, Eq_ v) => Container (IndexedVector k v) where
    elem x (IndexedVector m) = elem x $ P.map snd $ Map.toList m

type instance Index (IndexedVector k v) = k

instance (Eq v, Ord k, Semigroup v) => Indexed (IndexedVector k v) where
    (IndexedVector m) !! k = Map.lookup (WithPreludeOrd k) m

instance (Eq v, Ord k, Semigroup v) => IndexedUnfoldable (IndexedVector k v) where
    singletonAt i e = IndexedVector $ Map.singleton (WithPreludeOrd i) e

    consAt i e (IndexedVector s) = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    snocAt (IndexedVector s) i e = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    fromIndexedList xs = IndexedVector $ Map.fromList $ map (\(i,e) -> (WithPreludeOrd i,e)) xs

instance (Eq v, Ord k, Semigroup v) => IndexedFoldable (IndexedVector k v) where
    toIndexedList (IndexedVector s) = map (\(WithPreludeOrd i,e) -> (i,e)) $ Map.assocs s

instance (Eq v, Ord k, Semigroup v) => IndexedDeletable (IndexedVector k v) where
    deleteAt k (IndexedVector s) = IndexedVector $ Map.delete (WithPreludeOrd k) s

---------------------------------------

-- | This is a thin wrapper around the container's set type
newtype Set a = Set (Set.Set (WithPreludeOrd a))
    deriving (Read,Show,NFData)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = liftM fromList arbitrary

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
    elem a (Set s) = Set.member (WithPreludeOrd a)s

instance Ord a => Unfoldable (Set a) where
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

    fromList as = Set $ Set.fromList $ map WithPreludeOrd as

