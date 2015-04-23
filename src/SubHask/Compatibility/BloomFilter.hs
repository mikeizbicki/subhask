module SubHask.Compatibility.BloomFilter
    ( BloomFilter
    )
    where

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

import qualified Data.BloomFilter as BF

--------------------------------------------------------------------------------

newtype BloomFilter (n::Nat) a = BloomFilter (BF.Bloom a)

mkMutable [t| forall n a. BloomFilter n a |]

type instance Scalar (BloomFilter n a) = Int
type instance Logic (BloomFilter n a) = Bool
type instance Elem (BloomFilter n a) = a

hash = undefined

instance KnownNat n => Semigroup (BloomFilter n a)
    -- FIXME: need access to the underlying representation of BF.Bloom to implement

instance KnownNat n => Monoid (BloomFilter n a) where
    zero = BloomFilter (BF.empty hash n)
        where
            n = fromInteger $ natVal (Proxy::Proxy n)

instance KnownNat n => Constructible (BloomFilter n a)
    -- FIXME: need a good way to handle the hash generically

instance KnownNat n => Container (BloomFilter n a) where
    elem a (BloomFilter b) = BF.elem a b

instance KnownNat n => Normed (BloomFilter n a) where
    size (BloomFilter b) = BF.length b
    -- formula for number of elements in a bloom filter
    -- http://stackoverflow.com/questions/6099562/combining-bloom-filters
    -- c = log(z / N) / ((h * log(1 - 1 / N))

