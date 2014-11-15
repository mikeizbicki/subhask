module SubHask.Algebra.Trans.StringMetrics
    where

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
        go (toList xs) (toList ys)
        where
            go = memoize2 go'
                where
                    go' [] [] = 0
                    go' xs [] = fromIntegral $ length xs
                    go' [] ys = fromIntegral $ length ys
                    go' (x:xs) (y:ys) = minimum
                        [ go xs (y:ys) + 1
                        , go (x:xs) ys + 1
                        , go xs ys + if (x/=y) then 1 else 0
                        ]

--     {-# INLINE isFartherThanWithDistanceCanError #-}
--     isFartherThanWithDistanceCanError (Levenshtein xs) (Levenshtein ys) dist =
--         {-# SCC isFartherThanWithDistanceCanError #-}
--         go (toList xs) (toList ys)
--         where
--             go = memoize2 go_
--                 where
--                     go_
