-- | This module contains any objects relating to order theory
module SubHask.Algebra.Ord
    where

import qualified Prelude as P
import qualified Data.List as L

import qualified GHC.Arr as Arr
import Data.Array.ST hiding (freeze,thaw)
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Prelude (take)

import SubHask.Algebra
import SubHask.Category
import SubHask.Mutable
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

--------------------------------------------------------------------------------

-- | This wrapper let's us convert between SubHask's Ord type and the Prelude's.
-- See the "sort" function below for an example.
newtype WithPreludeOrd a = WithPreludeOrd { unWithPreludeOrd :: a }
    deriving Storable

instance Show a => Show (WithPreludeOrd a) where
    show (WithPreludeOrd a) = show a

-- | FIXME: for some reason, our deriving mechanism doesn't work on Show here;
-- It causes's Set's show to enter an infinite loop
deriveHierarchyFiltered ''WithPreludeOrd [ ''Eq_, ''Enum, ''Boolean, ''Ring, ''Metric ] [ ''Show ]

instance Eq a => P.Eq (WithPreludeOrd a) where
    {-# INLINE (==) #-}
    a==b = a==b

instance Ord a => P.Ord (WithPreludeOrd a) where
    {-# INLINE (<=) #-}
    a<=b = a<=b


-- | A wrapper around the Prelude's sort function.
--
-- FIXME:
-- We should put this in the container hierarchy so we can sort any data type
sort :: Ord a => [a] -> [a]
sort = map unWithPreludeOrd . L.sort . map WithPreludeOrd

-- | Randomly shuffles a list in time O(n log n); see http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: (Eq a, MonadRandom m) => [a] -> m [a]
shuffle xs = do
    let l = length xs
    rands <- take l `liftM` getRandomRs (0, l-1)
    let ar = runSTArray ( do
            ar <- Arr.thawSTArray (Arr.listArray (0, l-1) xs)
            forM_ (L.zip [0..(l-1)] rands) $ \(i, j) -> do
                vi <- Arr.readSTArray ar i
                vj <- Arr.readSTArray ar j
                Arr.writeSTArray ar j vi
                Arr.writeSTArray ar i vj
            return ar
            )
    return (Arr.elems ar)
