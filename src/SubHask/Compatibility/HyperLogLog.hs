module SubHask.Compatibility.HyperLogLog
    where

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

import qualified Data.HyperLogLog as H
import qualified Data.Reflection as R
import qualified Data.Semigroup as S
import qualified Prelude as P

-- FIXME: move the below imports to separate compatibility layers
import qualified Data.Bytes.Serial as S
import qualified Data.Approximate as A
import qualified Control.Lens as L

type instance Scalar Int64 = Int64

newtype HyperLogLog p a = H (H.HyperLogLog p)

mkMutable [t| forall p a. HyperLogLog p a |]

type instance Scalar (HyperLogLog p a) = Integer -- FIXME: make Int64
type instance Logic (HyperLogLog p a) = Bool
type instance Elem (HyperLogLog p a) = a

instance Semigroup (HyperLogLog p a) where
    (H h1)+(H h2) = H $ h1 S.<> h2

instance Abelian (HyperLogLog p a)

instance
    ( R.Reifies p Integer
    ) => Normed (HyperLogLog p a)
        where
    size (H h) = P.fromIntegral $ L.view A.estimate (H.size h)

instance
    ( R.Reifies p Integer
    , S.Serial a
    ) => Constructible (HyperLogLog p a)
        where
    cons a (H h) = H $ H.insert a h

