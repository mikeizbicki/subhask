-- | This module contains any objects relating to order theory
module SubHask.Algebra.Ord
    where

import qualified Prelude as P
import qualified Data.List as L

import qualified GHC.Arr as Arr
import Data.Array.ST hiding (freeze,thaw)
import Control.Monad
import Control.Monad.Random
import Prelude (take)

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

--------------------------------------------------------------------------------

instance {-#OVERLAPS#-} Classical Eq a => P.Eq a where
    (==) = (==)

instance {-#OVERLAPS#-} Classical Ord a => P.Ord a where
    compare = compare

-- | This wrapper let's us convert between SubHask's Ord type and the Prelude's.
-- See the "sort" function below for an example.
--
-- FIXME:
-- This should be removed.
-- The overlapping instances above are easier to use.
newtype WithPreludeOrd a = WithPreludeOrd { unWithPreludeOrd :: a }
    deriving Storable

instance Show a => Show (WithPreludeOrd a) where
    show (WithPreludeOrd a) = show a

-- | FIXME: for some reason, our deriving mechanism doesn't work on Show here;
-- It causes's Set's show to enter an infinite loop
deriveHierarchyFiltered ''WithPreludeOrd [ ''Eq, ''Enum, ''Boolean, ''Ring, ''Metric] [ ''Show ]

instance (Eq a, ClassicalLogic a) => P.Eq (WithPreludeOrd a) where
    {-# INLINE (==) #-}
    a==b = a==b

instance (Ord a, ClassicalLogic a) => P.Ord (WithPreludeOrd a) where
    {-# INLINE (<=) #-}
    a<=b = a<=b

-- | A wrapper around the Prelude's sort function.
--
-- FIXME:
-- We should put this in the container hierarchy so we can sort any data type
-- sort :: (Ord a, ClassicalLogic a) => [a] -> [a]
-- sort = map unWithPreludeOrd . L.sort . map WithPreludeOrd

-- | Randomly shuffles a list in time O(n log n).
-- See http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
    let l = length xs
    rands <- take l `liftM` getRandomRs (0, l-1)
    let ar = runSTArray ( do
            ar' <- Arr.thawSTArray (Arr.listArray (0, l-1) xs)
            forM_ (L.zip [0..(l-1)] rands) $ \(i, j) -> do
                vi <- Arr.readSTArray ar' i
                vj <- Arr.readSTArray ar' j
                Arr.writeSTArray ar' j vi
                Arr.writeSTArray ar' i vj
            return ar'
            )
    return (Arr.elems ar)
