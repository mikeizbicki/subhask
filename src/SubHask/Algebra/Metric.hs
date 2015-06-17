-- | This module defines the algebra over various types of balls in metric spaces
module SubHask.Algebra.Metric
    where

import SubHask.Category
import SubHask.Algebra
import SubHask.Algebra.Ord
-- import SubHask.Monad
-- import SubHask.Compatibility.Base
import SubHask.Internal.Prelude
import Control.Monad

import Data.List (nubBy,permutations,sort)
import System.IO

--------------------------------------------------------------------------------

-- | Useful for identifying tree metrics.
printTriDistances :: (Show (Scalar m), Metric m) => m -> m -> m -> IO ()
printTriDistances m1 m2 m3 = do
    putStrLn $ show (distance m1 m2) ++ " <= " + show (distance m2 m3 + distance m1 m3)
    putStrLn $ show (distance m1 m3) ++ " <= " + show (distance m2 m3 + distance m1 m2)
    putStrLn $ show (distance m2 m3) ++ " <= " + show (distance m1 m2 + distance m1 m3)

-- | There are three distinct perfect matchings in every complete 4 node graph.
-- A metric is a tree metric iff two of these perfect matchings have the same weight.
-- This is called the 4 points condition.
-- printQuadDistances :: (Ord (Scalar m), Show (Scalar m), Metric m) => m -> m -> m -> m -> IO ()
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

--------------------------------------------------------------------------------

-- | The closed balls in metric space.
-- Note that since we are not assuming any special structure, addition is rather inefficient.
--
-- FIXME:
-- There are several valid ways to perform the addition; which should we use?
-- We could add Lattice instances in a similar way as we could with Box if we added an empty element; should we do this?

data Ball v = Ball
    { radius :: !(Scalar v)
    , center :: !v
    }

mkMutable [t| forall b. Ball b |]

invar_Ball_radius :: (HasScalar v) => Ball v -> Logic (Scalar v)
invar_Ball_radius b = radius b >= 0

type instance Scalar (Ball v) = Scalar v
type instance Logic (Ball v) = Logic v
type instance Elem (Ball v) = v
type instance SetElem (Ball v) v' = Ball v'

-- misc classes

deriving instance (Read v, Read (Scalar v)) => Read (Ball v)
deriving instance (Show v, Show (Scalar v)) => Show (Ball v)

instance (Arbitrary v, Arbitrary (Scalar v), HasScalar v) => Arbitrary (Ball v) where
    arbitrary = do
        r <- arbitrary
        c <- arbitrary
        return $ Ball (abs r) c

instance (NFData v, NFData (Scalar v)) => NFData (Ball v) where
    rnf b = deepseq (center b)
          $ rnf (radius b)

-- comparison

instance (Eq v, HasScalar v) => Eq_ (Ball v) where
    b1 == b2 = radius b1 == radius b2
            && center b1 == center b2

-- algebra

instance (Metric v, HasScalar v, ClassicalLogic v) => Semigroup (Ball v) where
    b1+b2 = b1 { radius = radius b2 + radius b1 + distance (center b1) (center b2) }
--     b1+b2 = b1 { radius = radius b2 + max (radius b1) (distance (center b1) (center b2)) }

--     b1+b2 = b1' { radius = max (radius b1') (radius b2' + distance (center b1') (center b2')) }
--         where
--             (b1',b2') = if radius b1 > radius b2
--                 then (b1,b2)
--                 else (b2,b1)

-- container

instance (Metric v, HasScalar v, ClassicalLogic v) => Constructible (Ball v) where
    singleton v = Ball 0 v

instance (Metric v, HasScalar v, ClassicalLogic v) => Container (Ball v) where
    elem v b = not $ isFartherThan v (center b) (radius b)

--------------------------------------------------------------------------------

-- | FIXME: In a Banach space we can make Ball addition more efficient by moving the center to an optimal location.
newtype BanachBall v = BanachBall (Ball v)
