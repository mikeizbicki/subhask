module SubHask.Category.Algebra.TypeParams
    where

import Data.Params
import Data.Params.PseudoPrim
import qualified Data.Params.Vector.Unboxed as PVU
import qualified Data.Vector.Generic as VG

import SubHask.Internal.Prelude
import SubHask.Algebra

-------------------------------------------------------------------------------

instance 
    ( Monoid r
    , KnownNat len
    , PseudoPrim r
    ) => Semigroup (PVU.Vector (Static len) r) 
        where
    v1 + v2 = VG.zipWith (+) v1 v2

instance 
    ( Monoid r
    , KnownNat len
    , PseudoPrim r
    ) => Monoid (PVU.Vector (Static len) r) 
        where
    zero = VG.replicate (viewParam PVU._len (undefined :: PVU.Vector (Static len) r))  zero

instance 
    ( Abelian r
    , KnownNat len
    , PseudoPrim r
    ) => Abelian (PVU.Vector (Static len) r) 

instance 
    ( Group r
    , KnownNat len
    , PseudoPrim r
    ) => Group (PVU.Vector (Static len) r)  
        where
    negate v = VG.map negate v

type instance Scalar (PVU.Vector (Static len) r) = Scalar r

instance 
    ( Module r
    , KnownNat len
    , PseudoPrim r
    ) => Module (PVU.Vector (Static len) r) 
        where
    r .* v = VG.map (r.*) v 
    v *. r = VG.map (*.r) v 

instance
    ( VectorSpace r
    , KnownNat len
    , PseudoPrim r
    ) => VectorSpace (PVU.Vector (Static len) r)
        where
    v /. r = VG.map (/.r) v
