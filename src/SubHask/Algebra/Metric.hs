-- | This module defines the algebra over various types of balls in metric spaces
module SubHask.Algebra.Metric
    where

import SubHask.Category
import SubHask.Algebra
import SubHask.Algebra.Ord
import SubHask.Monad
import SubHask.Compatibility.Base
import SubHask.Internal.Prelude

import Data.List (nubBy,permutations,sort)
import System.IO
-- |
--
-- Useful for identifying tree metrics.
printTriDistances :: (Show (Scalar m), MetricSpace m) => m -> m -> m -> IO ()
printTriDistances m1 m2 m3 = do
    putStrLn $ show (distance m1 m2) ++ " <= " + show (distance m2 m3 + distance m1 m3)
    putStrLn $ show (distance m1 m3) ++ " <= " + show (distance m2 m3 + distance m1 m2)
    putStrLn $ show (distance m2 m3) ++ " <= " + show (distance m1 m2 + distance m1 m3)

-- | There are three distinct perfect matchings in every complete 4 node graph.
-- A metric is a tree metric iff two of these perfect matchings have the same weight.
-- This is called the 4 points condition.
-- printQuadDistances :: (Ord (Scalar m), Show (Scalar m), MetricSpace m) => m -> m -> m -> m -> IO ()
printQuadDistances m1 m2 m3 m4 = do
    forM_ xs $ \(match,dist) -> do
        putStrLn $ match ++ " = " ++ show dist

    where
        xs = nubBy (\(x,_) (y,_) -> x==y)
           $ sort
           $ map mkMatching
           $ permutations [('1',m1),('2',m2),('3',m3),('4',m4)]

        mkMatching [(i1,n1),(i2,n2),(i3,n3),(i4,n4)] =
            ( (\[x,y] -> x++":"++y) $ sort
                [ sort (i1:i2:[])
                , sort (i3:i4:[])
                ]
            , distance n1 n2 + distance n3 n4
            )

-------------------

-- |
--
-- FIXME: think carefully about whether these should all be open or closed
--
-- FIXME: think carefully about how the logic of the interval should relate to the logic of the metric space
data Box v = Box
    { smallest :: !v
    , largest :: !v
    }
    deriving (Read,Show)

instance Arbitrary v => Arbitrary (Box v) where
    arbitrary = do
        v1 <- arbitrary
        v2 <- arbitrary
        return $ Box v1 v2

type instance Scalar (Box v) = Scalar v
type instance Logic (Box v) = Logic v
type instance Elem (Box v) = v

instance (Eq v, Eq (Scalar v)) => Eq_ (Box v) where
    b1==b2 = smallest b1 == smallest b2 && largest b1 == largest b2

instance (Lattice v, MetricSpace v) => POrd_ (Box v) where
    inf b1 b2 = Box
        { smallest = sup (smallest b1) (smallest b2)
        , largest = inf (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => Lattice_ (Box v) where
    sup b1 b2 = Box
        { smallest = inf (smallest b1) (smallest b2)
        , largest = sup (largest b1) (largest b2)
        }

instance (Lattice v, MetricSpace v) => Semigroup (Box v) where
    (+) = inf

-- FIXME: This can't be a PreContainer because of the Normed requirement
--
-- instance (Lattice v, MetricSpace v) => PreContainer (Box v) where
--     elem e b = e >= smallest b && e <= largest b
--     notElem = not elem

-------------------

-- | The open ball of a metric space assuming no special structure
--
-- FIXME: not implemented
data OpenBall v = OpenBall
    { radius :: !(Scalar v)
    , center :: !v
    }

type instance Scalar (OpenBall v) = Scalar v
type instance Logic (OpenBall v) = Logic v

instance (Eq v, Eq (Scalar v)) => Eq_ (OpenBall v) where
    b1 == b2 = radius b1 == radius b2 && center b1 == center b2

instance (Logic v~Bool, MetricSpace v) => Semigroup (OpenBall v) where
    b1+b2 = b1' { radius = distance (center b1') (center b2') + radius b2' }
        where
            (b1',b2') = if radius b1 > radius b2
                then (b1,b2)
                else (b2,b1)

-------------------

-- | The open ball of a metric space made more efficient relying on extra structure
--
-- FIXME: not implemented
newtype VectorBall v = VectorBall (OpenBall v)

type instance Scalar (VectorBall v) = Scalar v
