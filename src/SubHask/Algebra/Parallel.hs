-- | Every monoid homomorphism from a Container can be parallelized.
-- And if you believe that @NC /= P@, then every parallel algorithm is induced by a monoid in this manner.
module SubHask.Algebra.Parallel
    ( parallel
    , parallelN
    , disableMultithreading
    , Partitionable (..)
    , law_Partitionable_length
    , law_Partitionable_monoid

    -- * parallel helpers
    , parallelBlockedN
    , parallelBlocked
    , unsafeParallelInterleavedN
    , unsafeParallelInterleaved
    , parallelInterleaved
    )
    where

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

import Control.Monad

import qualified Prelude as P
import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | Converts any monoid homomorphism into an efficient parallelized function.
-- This is the only function you should have to care about.
-- It uses rewrite rules to select the most cache-efficient parallelization method for the particular data types called.
{-# INLINABLE parallel #-}
parallel ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
parallel = parallelBlocked

parallelN ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => Int -- ^ number of parallel threads
      -> (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
parallelN=parallelBlockedN

-- | Let's you specify the exact number of threads to parallelize over.
{-# INLINE [2] parallelBlockedN #-}
parallelBlockedN ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => Int -- ^ number of parallel threads
      -> (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
parallelBlockedN n f = parfoldtree1 . parMap rdeepseq f . partition n

-- The function automatically detects the number of available processors and parallelizes the function accordingly.
{-# INLINE [2] parallelBlocked #-}
parallelBlocked ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
parallelBlocked = if dopar
    then parallelBlockedN numproc
    else id
    where
        numproc = unsafePerformIO getNumCapabilities
        dopar = numproc > 1

-- | Let's you specify the exact number of threads to parallelize over.
-- This function is unsafe because if our @range@ is not "Abelian", this function changes the results.
{-# INLINE [2] unsafeParallelInterleavedN #-}
unsafeParallelInterleavedN ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => Int -- ^ number of parallel threads
      -> (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
unsafeParallelInterleavedN n f = parfoldtree1 . parMap rdeepseq f . partitionInterleaved n

-- | This function automatically detects the number of available processors and parallelizes the function accordingly.
-- This function is unsafe because if our @range@ is not "Abelian", this function changes the results.
{-# INLINE [2] unsafeParallelInterleaved #-}
unsafeParallelInterleaved ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
unsafeParallelInterleaved = if dopar
    then unsafeParallelInterleavedN numproc
    else id
    where
        numproc = unsafePerformIO getNumCapabilities
        dopar = numproc > 1

-- | This function automatically detects the number of available processors and parallelizes the function accordingly.
-- This function is safe (i.e. it won't affect the output) because it requires the "Abelian" constraint.
{-# INLINE [2] parallelInterleaved #-}
parallelInterleaved ::
    ( Partitionable domain
    , Abelian range
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential monoid homomorphism
      -> (domain -> range) -- ^ parallel monoid homomorphism
parallelInterleaved = unsafeParallelInterleaved

-- | This forces a function to be run with only a single thread.
-- That is, the function is executed as if @-N1@ was passed into the program rather than whatever value was actually used.
-- Subsequent functions are not affected.
--
-- Why is this useful?
-- The GHC runtime system can make non-threaded code run really slow when many threads are enabled.
-- For example, I have found instances of sequential code taking twice as long when the @-N16@ flag is passed to the run time system.
-- By wrapping those function calls in "disableMultithreading", we restore the original performance.
{-# INLINABLE disableMultithreading #-}
disableMultithreading :: IO a -> IO a
disableMultithreading a = do
    n <- getNumCapabilities
    setNumCapabilities 1
    a' <- a
    setNumCapabilities n
    return a'

--------------------------------------------------------------------------------

-- | A Partitionable container can be split up into an arbitrary number of subcontainers of roughly equal size.
class (Monoid t, Foldable t, Constructible t) => Partitionable t where

    -- | The Int must be >0
    {-# INLINABLE partition #-}
    partition :: Int -> t -> [t]
    partition i t = map (\(x:xs) -> fromList1 x xs) $ partitionBlocked_list i $ toList t

    {-# INLINABLE partitionInterleaved #-}
    partitionInterleaved :: Int -> t -> [t]
    partitionInterleaved i t = map (\(x:xs) -> fromList1 x xs) $ partitionInterleaved_list i $ toList t

law_Partitionable_length :: (ClassicalLogic t, Partitionable t) => Int -> t -> Bool
law_Partitionable_length n t
    | n > 0 = length (partition n t) <= n
    | otherwise = True

law_Partitionable_monoid :: (ClassicalLogic t, Eq t, Partitionable t) => Int -> t -> Bool
law_Partitionable_monoid n t
    | n > 0 = sum (partition n t) == t
    | otherwise = True

-- | Like foldtree1, but parallel
{-# INLINABLE parfoldtree1 #-}
parfoldtree1 :: Monoid a => [a] -> a
parfoldtree1 as = case go as of
    []  -> zero
    [a] -> a
    as'  -> parfoldtree1 as'
    where
        go []  = []
        go [a] = [a]
        go (a1:a2:as'') = par a12 $ a12:go as''
            where
                a12=a1+a2

instance Partitionable [a] where
    {-# INLINABLE partition #-}
    partition = partitionBlocked_list

    {-# INLINABLE partitionInterleaved #-}
    partitionInterleaved = partitionInterleaved_list

{-# INLINABLE partitionBlocked_list #-}
partitionBlocked_list :: Int -> [a] -> [[a]]
partitionBlocked_list n xs = go xs
    where
        go [] = []
        go xs' =  a:go b
            where
                (a,b) = P.splitAt len xs'

        size' = length xs
        len = size' `div` n
            + if size' `rem` n == 0 then 0 else 1

-- | This is an alternative definition for list partitioning.
-- It should be faster on large lists because it only requires one traversal.
-- But it also breaks parallelism for non-commutative operations.
{-# INLINABLE partitionInterleaved_list #-}
partitionInterleaved_list :: Int -> [a] -> [[a]]
partitionInterleaved_list n xs = [map snd $ P.filter (\(i,_)->i `mod` n==j) ixs | j<-[0..n-1]]
    where
        ixs = addIndex 0 xs
        addIndex _ [] = []
        addIndex i (x:xs') = (i,x):(addIndex (i+1) xs')

