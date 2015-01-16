{-# LANGUAGE RebindableSyntax,QuasiQuotes #-}

-- | This module contains the container algebras
module SubHask.Algebra.Container
    where

import Control.Monad
import GHC.Prim
import Control.Monad
import GHC.TypeLits
import qualified Prelude as P
import Prelude (tail,head,last)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import SubHask.Algebra
import SubHask.Algebra.Ord
import SubHask.Category
import SubHask.Compatibility.Base
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
    ( Lattice_ a
    , Field (Scalar a)
    , Normed a
    , Logic (Scalar a) ~ Logic a
    , Boolean (Logic a)
    ) => MetricSpace (Jaccard a)
        where
    distance (Jaccard xs) (Jaccard ys) = 1 - size (xs && ys) / size (xs || ys)

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
            go xs [] i = i + fromIntegral (size xs)
            go [] ys i = i + fromIntegral (size ys)
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
                    go_ xs [] i = i + fromIntegral (size xs)
                    go_ [] ys i = i + fromIntegral (size ys)

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
    , Eq (Elem a)
    , Eq a
    , Show a
    , Ord (Scalar a)
    , Ring (Scalar a)
    , Bounded (Scalar a)
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
	  lab = size a - size b
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
newtype Lexical a = Lexical { unLexical :: a }

deriveHierarchy ''Lexical [ ''Eq_, ''Foldable, ''Unfoldable, ''Monoid ]
-- deriveHierarchy ''Lexical [ ''Eq_, ''Monoid ]

instance
    (Logic a~Bool
    , Ord (Elem a)
    , Foldable a
    , Unfoldable a
    , Eq_ a
    ) => POrd_ (Lexical a)
        where
    inf a1 a2 = if a1<a2 then a1 else a2

    (Lexical a1)<(Lexical a2) = go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = if x<y
                then True
                else if x>y
                    then False
                    else go xs ys
            go [] [] = False
            go [] _  = True
            go _  [] = False

instance (Logic a~Bool, Ord (Elem a), Foldable a, Unfoldable a, Eq_ a) => MinBound_ (Lexical a) where
    minBound = Lexical zero

instance (Logic a~Bool, Ord (Elem a), Foldable a, Unfoldable a, Eq_ a) => Lattice_ (Lexical a) where
    sup a1 a2 = if a1>a2 then a1 else a2

    (Lexical a1)>(Lexical a2) = go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = if x>y
                then True
                else if x<y
                    then False
                    else go xs ys
            go [] [] = False
            go [] _  = False
            go _  [] = True

instance (Logic a~Bool, Ord (Elem a), Foldable a, Unfoldable a, Eq_ a) => Ord_ (Lexical a) where

---------------------------------------

newtype ComponentWise a = ComponentWise { unComponentWise :: a }

deriveHierarchy ''ComponentWise [ ''Eq_, ''Foldable, ''Unfoldable, ''Monoid ]
-- deriveHierarchy ''ComponentWise [ ''Monoid ]

class (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a
instance (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a

-- instance (SimpleContainerLogic a, Eq_ (Elem a), Foldable a) => Eq_ (ComponentWise a) where
--     (ComponentWise a1)==(ComponentWise a2) = toList a1==toList a2

instance (SimpleContainerLogic a, Eq_ a, POrd_ (Elem a), Foldable a, Unfoldable a) => POrd_ (ComponentWise a) where
    inf (ComponentWise a1) (ComponentWise a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = inf x y:go xs ys
            go _ _ = []

instance (SimpleContainerLogic a, Eq_ a, POrd_ (Elem a), Foldable a, Unfoldable a) => MinBound_ (ComponentWise a) where
    minBound = ComponentWise zero

instance (SimpleContainerLogic a, Eq_ a, Lattice_ (Elem a), Foldable a, Unfoldable a) => Lattice_ (ComponentWise a) where
    sup (ComponentWise a1) (ComponentWise a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = sup x y:go xs ys
            go xs [] = xs
            go [] ys = ys

