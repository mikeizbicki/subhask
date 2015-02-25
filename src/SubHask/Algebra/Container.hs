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

--------------------------------------------------------------------------------
-- | A 'Box' is a generalization of an interval from the real numbers into an arbitrary lattice.
-- Boxes are closed in the sense that the end points of the boxes are also contained within the box.
--
-- See <http://en.wikipedia.org/wiki/Partially_ordered_set#Interval wikipedia> for more details.
data Box v = Box
    { smallest :: !v
    , largest :: !v
    }
    deriving (Read,Show)

invar_Box_ordered :: (Lattice v, HasScalar v) => Box v -> Logic v
invar_Box_ordered b = largest b >= smallest b

type instance Scalar (Box v) = Scalar v
type instance Logic (Box v) = Logic v
type instance Elem (Box v) = v
type instance SetElem (Box v) v' = Box v'

-- misc classes

instance (Lattice v, Arbitrary v) => Arbitrary (Box v) where
    arbitrary = do
        v1 <- arbitrary
        v2 <- arbitrary
        return $ Box (inf v1 v2) (sup v1 v2)

-- comparison

instance (Eq v, HasScalar v) => Eq_ (Box v) where
    b1==b2 = smallest b1 == smallest b2
          && largest  b1 == largest  b2

-- FIXME:
-- the following instances are "almost" valid
-- POrd_, however, can't be correct without adding an empty element to the Box
-- should we do this?  Would it hurt efficiency?
--
-- instance (Lattice v, HasScalar v) => POrd_ (Box v) where
--     inf b1 b2 = Box
--         { smallest = sup (smallest b1) (smallest b2)
--         , largest = inf (largest b1) (largest b2)
--         }
--
-- instance (Lattice v, HasScalar v) => Lattice_ (Box v) where
--     sup = (+)

-- algebra

instance (Lattice v, HasScalar v) => Semigroup (Box v) where
    b1+b2 = Box
        { smallest = inf (smallest b1) (smallest b2)
        , largest  = sup (largest b1)  (largest b2)
        }

-- container

instance (Lattice v, HasScalar v) => Constructible (Box v) where
    singleton v = Box v v

instance (Lattice v, HasScalar v) => Container (Box v) where
    elem a (Box lo hi) = a >= lo && a <= hi

-------------------------------------------------------------------------------

-- | The Jaccard distance.
--
-- See <https://en.wikipedia.org/wiki/Jaccard_index wikipedia> for more detail.
newtype Jaccard a = Jaccard a

deriveHierarchy ''Jaccard
    [ ''Ord
    , ''Boolean
    , ''Ring
    , ''Foldable
    ]

instance
    ( Lattice_ a
    , Field (Scalar a)
    , Normed a
    , Logic (Scalar a) ~ Logic a
    , Boolean (Logic a)
    , HasScalar a
    ) => Metric (Jaccard a)
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
    , ''Foldable
    ]

instance
    ( Foldable a
    , Eq (Elem a)
    , Eq a
    , ClassicalLogic (Scalar a)
    , HasScalar a
    ) => Metric (Hamming a)
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

    {-# INLINE distanceUB #-}
    distanceUB (Hamming xs) (Hamming ys) dist =
        {-# SCC distanceUB_Hamming #-}
        go (toList xs) (toList ys) 0
        where
            go xs ys tot = if tot > dist
                then tot
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
    , ''Foldable
    ]

instance
    ( Foldable a
    , Eq (Elem a)
    , Eq a
    , Show a
    , HasScalar a
    , ClassicalLogic (Scalar a)
    , Bounded (Scalar a)
    ) => Metric (Levenshtein a)
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
    , ''Hilbert
    , ''Ring
    , ''Constructible
    ]

instance Foldable s => Foldable (Uncompensated s) where
    uncons (Uncompensated s) = case uncons s of
        Nothing -> Nothing
        Just (x,xs) -> Just (x, Uncompensated xs)

    unsnoc (Uncompensated s) = case unsnoc s of
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

deriveHierarchy ''Lexical [ ''Eq_, ''Foldable, ''Constructible, ''Monoid ]
-- deriveHierarchy ''Lexical [ ''Eq_, ''Monoid ]

instance
    (Logic a~Bool
    , Ord (Elem a)
    , Foldable a
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

instance (Logic a~Bool, Ord (Elem a), Foldable a, Eq_ a) => MinBound_ (Lexical a) where
    minBound = Lexical zero

instance (Logic a~Bool, Ord (Elem a), Foldable a, Eq_ a) => Lattice_ (Lexical a) where
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

instance (Logic a~Bool, Ord (Elem a), Foldable a, Eq_ a) => Ord_ (Lexical a) where

---------------------------------------

newtype ComponentWise a = ComponentWise { unComponentWise :: a }

deriveHierarchy ''ComponentWise [ ''Eq_, ''Foldable, ''Monoid ]
-- deriveHierarchy ''ComponentWise [ ''Monoid ]

class (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a
instance (Boolean (Logic a), Logic (Elem a) ~ Logic a) => SimpleContainerLogic a

-- instance (SimpleContainerLogic a, Eq_ (Elem a), Foldable a) => Eq_ (ComponentWise a) where
--     (ComponentWise a1)==(ComponentWise a2) = toList a1==toList a2

instance (SimpleContainerLogic a, Eq_ a, POrd_ (Elem a), Foldable a) => POrd_ (ComponentWise a) where
    inf (ComponentWise a1) (ComponentWise a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = inf x y:go xs ys
            go _ _ = []

instance (SimpleContainerLogic a, Eq_ a, POrd_ (Elem a), Foldable a) => MinBound_ (ComponentWise a) where
    minBound = ComponentWise zero

instance (SimpleContainerLogic a, Eq_ a, Lattice_ (Elem a), Foldable a) => Lattice_ (ComponentWise a) where
    sup (ComponentWise a1) (ComponentWise a2) = fromList $ go (toList a1) (toList a2)
        where
            go (x:xs) (y:ys) = sup x y:go xs ys
            go xs [] = xs
            go [] ys = ys

