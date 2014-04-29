module SubHask.Category.Linear
    where

import GHC.Prim

import Control.DeepSeq
import Data.Typeable
import qualified Prelude as P
import qualified Data.Map as Map
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS

-- import qualified Numeric.LinearAlgebra as LA

import Data.Params.Vector.Unboxed

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Finite
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

class Category cat => Linear cat where


-------------------------------------------------------------------------------
-- vectors

-------------------------------------------------------------------------------
-- free vector space

newtype SparseFreeVector r a = SparseFreeVector (Map.Map a r)
    deriving (Read,Show,Typeable)

instance (NFData r, NFData a) => NFData (SparseFreeVector r a) where
    rnf (SparseFreeVector m) = rnf m

mkSparseFreeVector :: (Ring r, Ord a) => [a] -> SparseFreeVector r a
mkSparseFreeVector xs = SparseFreeVector $ Map.fromList $ map (,one) xs

-------------------

type instance Scalar (SparseFreeVector r a) = Scalar r

instance (Ord a, Abelian r) => Abelian (SparseFreeVector r a)
instance (Ord a, Monoid r) => Monoid (SparseFreeVector r a) where
    zero = SparseFreeVector $ Map.empty
    (SparseFreeVector m1)+(SparseFreeVector m2) = SparseFreeVector $ Map.unionWith (+) m1 m2

instance (Ord a, Group r) => Group (SparseFreeVector r a) where
    negate (SparseFreeVector m1) = SparseFreeVector $ Map.map negate m1

instance (Ord a, Ring r) => Module r (SparseFreeVector r a) where
    r .* (SparseFreeVector m) = SparseFreeVector $ Map.map (r*) m

instance (Ord a, Field r) => VectorSpace r (SparseFreeVector r a)

instance (Ord a, Field r, IsScalar r) => InnerProductSpace (SparseFreeVector r a) where
    (SparseFreeVector m1)<>(SparseFreeVector m2) = Map.foldr (+) zero $ Map.intersectionWith (*) m1 m2

instance (Ord a, Field r, IsScalar r) => OuterProduct (SparseFreeVector r a) where
    type Outer (SparseFreeVector r a) = SparseFreeVector r (a,a)
    (SparseFreeVector m1)><(SparseFreeVector m2) = SparseFreeVector $ Map.fromList
        [ ((k1,k2),v1*v2)
        | (k1,v1) <- Map.toList m1
        , (k2,v2) <- Map.toList m2
        ]

instance (Ord a, Floating r, IsScalar r) => MetricSpace (SparseFreeVector r a) where
    distance (SparseFreeVector v1) (SparseFreeVector v2) = 
        sqrt $ Map.foldl (+) zero $ Map.map (\a -> a*a) $ Map.unionWith (-) v1 v2

instance (Ord a, Floating r, IsScalar r) => HilbertSpace (SparseFreeVector r a)

x = mkSparseFreeVector $ words "this is a test" :: SparseFreeVector Double String
y = mkSparseFreeVector $ words "this is not" :: SparseFreeVector Double String
