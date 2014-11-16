module SubHask.Algebra.Trans.StringMetrics
    where

import Prelude (last,head,tail)
import qualified Prelude as P

import SubHask
import SubHask.TemplateHaskell.Deriving

import Data.Function.Memoize
import Debug.Trace

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

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
