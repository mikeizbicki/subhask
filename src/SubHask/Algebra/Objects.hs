module SubHask.Algebra.Objects
    where

import GHC.Prim
import GHC.TypeLits
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.SparseFunction
import SubHask.Category.Trans.Bijective

-------------------------------------------------------------------------------
-- the symmetric group

-- The symmetric group is one of the simplest and best studied finite groups.
-- It is efficiently implemented as a "BijectiveT SparseFunction (Z n) (Z n)".
-- See <https://en.wikipedia.org/wiki/Symmetric_group>
newtype Sym (n::Nat) = Sym (BijectiveT SparseFunction (Z n) (Z n))

instance KnownNat n => Monoid (Sym n) where
    zero = Sym id
    (Sym s1)+(Sym s2) = Sym $ s1.s2

instance KnownNat n => Group (Sym n) where
    negate (Sym s) = Sym $ inverse s

-------------------------------------------------------------------------------
-- the vedic square

-- | The Vedic Square always forms a monoid, and sometimes forms a group 
-- depending on the value of "n".  (The type system isn't powerful enough to
-- encode these special cases.)  See Wikipedia for more details
-- <https://en.wikipedia.org/wiki/Vedic_square>
newtype VedicSquare (n::Nat) = VedicSquare (Z n)

instance KnownNat n => Monoid (VedicSquare n) where
    zero = VedicSquare one
    (VedicSquare v1)+(VedicSquare v2) = VedicSquare $ v1*v2 

-------------------------------------------------------------------------------

instance Monoid b => Monoid (a -> b) where
    zero = \a -> zero
    f+g = \a -> f a + g a

instance Group b => Group (a -> b) where
    negate f = negate . f
