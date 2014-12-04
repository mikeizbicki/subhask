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

-- | Lexical ordering of foldable types
newtype Lexical a = Lexical a
    deriving (Read,Show,Arbitrary,NFData,Semigroup,Monoid)

deriveHierarchy ''Lexical [ ''Foldable, ''Unfoldable ]

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

{-
newtype IndexedVector k v = IndexedVector { unsafeGetMap :: Map.Map (WithPreludeOrd k) v }

type instance Scalar (IndexedVector k v) = Scalar v
type instance Elem (IndexedVector k v) = v

-- | This is the L2 norm of the vector.
instance (Floating (Scalar v), IsScalar v, Ord v) => Normed (IndexedVector k v) where
    {-# INLINE abs #-}
    abs (IndexedVector m) =
        {-# SCC abs_IndexedVector #-}
        sqrt $ sum $ map (**2) $ Map.elems m

-- instance (Floating (Scalar v), IsScalar v, Ord v, Ord k) => MetricSpace (IndexedVector k v) where
--     {-# INLINE distance #-}
--     distance (IndexedVector m1) (IndexedVector m2) =
--         {-# SCC distance_IndexedVector #-}
--         sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
--         where
--             go tot [] [] = tot
--             go tot [] ((k,v):xs) = go (tot+v*v) [] xs
--             go tot ((k,v):xs) [] = go (tot+v*v) [] xs
--
--             go tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
--                 EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
--                 LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
--                 GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1
--
--     isFartherThanWithDistanceCanError (IndexedVector m1) (IndexedVector m2) dist =
--         {-# SCC isFartherThanWithDistanceCanError_IndexedVector #-}
--         sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
--         where
--             dist2 = dist*dist
--
--             go tot [] [] = tot
--             go tot xs ys = if tot > dist2
--                 then errorVal
--                 else go' tot xs ys
--
--             go' tot [] ((k,v):xs) = go (tot+v*v) [] xs
--             go' tot ((k,v):xs) [] = go (tot+v*v) [] xs
--
--             go' tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
--                 EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
--                 LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
--                 GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1

instance (Eq_ k, Eq_ v) => Eq_ (IndexedVector k v) where
    (IndexedVector m1)==(IndexedVector m2) = m1' == m2'
        where
            m1' = removeWithPreludeOrd $ Map.toList m1
            m2' = removeWithPreludeOrd $ Map.toList m2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd ((WithPreludeOrd k,v):xs) = (k,v):removeWithPreludeOrd xs

{-
instance (Ord k, POrd v, Semigroup v) => POrd (IndexedVector k v) where
    pcompare (IndexedVector m1) (IndexedVector m2) = go (Map.assocs m1) (Map.assocs m2)
        where
            go [] [] = PEQ
            go [] _  = PLT
            go _  [] = PGT
            go ((k1,v1):xs1) ((k2,v2):xs2) = case pcompare k1 k2 of
                PNA -> PNA
                PLT -> PLT
                PGT -> PGT
                PEQ -> case pcompare v1 v2 of
                    PNA -> PNA
                    PLT -> PLT
                    PGT -> PGT
                    PEQ -> go xs1 xs2

instance (Ord k, POrd v, Semigroup v) => Ord (IndexedVector k v) where

instance (Ord k, Semigroup v) => InfSemilattice (IndexedVector k v) where
    inf (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.unionWith (+) m1 m2

instance (Ord k, Semigroup v) => SupSemilattice (IndexedVector k v) where
    sup (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.intersectionWith (+) m1 m2

instance (Ord k, Semigroup v) => Lattice (IndexedVector k v) where

instance (Ord k, Semigroup v) => Semigroup (IndexedVector k v) where
    (+) = inf

instance (Ord k, Semigroup v) => Monoid (IndexedVector k v) where
    zero = IndexedVector $ Map.empty

instance (Ord k, Abelian v) => Abelian (IndexedVector k v)

instance (Ord k, Semigroup v, Eq_ v) => Container (IndexedVector k v) where
    elem x (IndexedVector m) = elem x $ P.map snd $ Map.toList m

type instance Index (IndexedVector k v) = k

instance (Ord k, Semigroup v) => Indexed (IndexedVector k v) where
    (IndexedVector m) !! k = Map.lookup (WithPreludeOrd k) m

instance (Ord k, Semigroup v) => IndexedUnfoldable (IndexedVector k v) where
    singletonAt i e = IndexedVector $ Map.singleton (WithPreludeOrd i) e

    consAt i e (IndexedVector s) = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    snocAt (IndexedVector s) i e = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    fromIndexedList xs = IndexedVector $ Map.fromList $ map (\(i,e) -> (WithPreludeOrd i,e)) xs

instance (Ord k, Semigroup v) => IndexedFoldable (IndexedVector k v) where
    toIndexedList (IndexedVector s) = map (\(WithPreludeOrd i,e) -> (i,e)) $ Map.assocs s

instance (Ord k, Semigroup v) => IndexedDeletable (IndexedVector k v) where
    deleteAt k (IndexedVector s) = IndexedVector $ Map.delete (WithPreludeOrd k) s
-}

---------------------------------------
-}

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

