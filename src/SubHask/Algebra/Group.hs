{-# LANGUAGE RebindableSyntax,QuasiQuotes #-}

-- | This module contains most of the math types not directly related to linear algebra
--
-- FIXME: there is probably a better name for this
module SubHask.Algebra.Group
    where

import Control.Monad
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------
-- non-negative objects

newtype NonNegative t = NonNegative { unNonNegative :: t }

deriveHierarchy ''NonNegative [ ''Enum, ''Boolean, ''Rig, ''Metric ]

instance (Ord t, ClassicalLogic t, Group t) => Cancellative (NonNegative t) where
    (NonNegative t1)-(NonNegative t2) = if diff>zero
        then NonNegative diff
        else NonNegative zero
        where
            diff=t1-t2

-------------------------------------------------------------------------------
-- integers modulo n

-- | Maps members of an equivalence class into the "canonical" element.
class Quotient a (b::k) where
    mkQuotient :: a -> a/b

-- | The type of equivalence classes created by a mod b.
newtype (/) (a :: *) (b :: k) = Mod a

instance (Quotient a b, Arbitrary a) => Arbitrary (a/b) where
    arbitrary = liftM mkQuotient arbitrary

deriveHierarchyFiltered ''(/) [ ''Eq, ''P.Ord ] [''Arbitrary]

instance (Semigroup a, Quotient a b) => Semigroup (a/b) where
    (Mod z1) + (Mod z2) = mkQuotient $ z1 + z2

instance (Abelian a, Quotient a b) => Abelian (a/b)

instance (Monoid a, Quotient a b) => Monoid (a/b)
    where zero = Mod zero

instance (Cancellative a, Quotient a b) => Cancellative (a/b) where
    (Mod i1)-(Mod i2) = mkQuotient $ i1-i2

instance (Group a, Quotient a b) => Group (a/b) where
    negate (Mod i) = mkQuotient $ negate i

instance (Rg a, Quotient a b) => Rg (a/b) where
    (Mod z1)*(Mod z2) = mkQuotient $ z1 * z2

instance (Rig a, Quotient a b) => Rig (a/b) where
    one = Mod one

instance (Ring a, Quotient a b) => Ring (a/b) where
    fromInteger i = mkQuotient $ fromInteger i

type instance ((a/b)><c) = (a><c)/b

instance (Module a, Quotient a b) => Module (a/b) where
    (Mod a) .*  r       = mkQuotient $ a .*  r

-- | The type of integers modulo n
type Z (n::Nat) = Integer/n

instance KnownNat n => Quotient Int n
    where
        mkQuotient i = Mod $ i `P.mod` (fromIntegral $ natVal (Proxy::Proxy n))

instance KnownNat n => Quotient Integer n
    where
        mkQuotient i = Mod $ i `P.mod` (natVal (Proxy::Proxy n))

-- | Extended Euclid's algorithm is used to calculate inverses in modular arithmetic
extendedEuclid :: (Eq t, ClassicalLogic t, Integral t) => t -> t -> (t,t,t,t,t,t)
extendedEuclid a b = go zero one one zero b a
    where
        go s1 s0 t1 t0 r1 r0 = if r1==zero
            then (s1,s0,t1,t0,undefined,r0)
            else go s1' s0' t1' t0' r1' r0'
            where
                q = r0 `div` r1
                (r0', r1') = (r1,r0-q*r1)
                (s0', s1') = (s1,s0-q*s1)
                (t0', t1') = (t1,t0-q*t1)

-------------------------------------------------------------------------------
-- example: Galois field

-- | @Galois p k@ is the type of integers modulo p^k, where p is prime.
-- All finite fields have this form.
--
-- See wikipedia <https://en.wikipedia.org/wiki/Finite_field> for more details.
--
-- FIXME: Many arithmetic operations over Galois Fields can be implemented more efficiently than the standard operations.
-- See <http://en.wikipedia.org/wiki/Finite_field_arithmetic>.
newtype Galois (p::Nat) (k::Nat) = Galois (Z (p^k))

type instance Galois p k >< Integer = Galois p k

deriveHierarchy ''Galois [''Eq,''Ring]

instance KnownNat (p^k) => Module  (Galois p k) where
    z  .*   i = Galois (Mod i) * z

instance (Prime p, KnownNat (p^k)) => Field (Galois p k) where
    reciprocal (Galois (Mod i)) = Galois $ mkQuotient $ t
        where
            (_,_,_,t,_,_) = extendedEuclid n i
            n = natVal (Proxy::Proxy (p^k))

-------------------

class Prime (n::Nat)
instance Prime 1
instance Prime 2
instance Prime 3
instance Prime 5
instance Prime 7
instance Prime 11
instance Prime 13
instance Prime 17
instance Prime 19
instance Prime 23

-------------------------------------------------------------------------------
-- the symmetric group

-- | The symmetric group is one of the simplest and best studied finite groups.
-- It is efficiently implemented as a "BijectiveT SparseFunction (Z n) (Z n)".
-- See <https://en.wikipedia.org/wiki/Symmetric_group>

-- newtype Sym (n::Nat) = Sym (BijectiveT SparseFunction (Z n) (Z n))
--
-- instance KnownNat n => Monoid (Sym n) where
--     zero = Sym id
--     (Sym s1)+(Sym s2) = Sym $ s1.s2
--
-- instance KnownNat n => Group (Sym n) where
--     negate (Sym s) = Sym $ inverse s

-------------------------------------------------------------------------------
-- | The GrothendieckGroup is a general way to construct groups from cancellative semigroups.
--
-- FIXME: How should this be related to the Ratio type?
--
-- See <http://en.wikipedia.org/wiki/Grothendieck_group wikipedia> for more details.
data GrothendieckGroup g where
    GrotheindieckGroup :: Cancellative g => g -> GrothendieckGroup g

-------------------------------------------------------------------------------
-- the vedic square

-- | The Vedic Square always forms a monoid,
-- and sometimes forms a group depending on the value of "n".
-- (The type system isn't powerful enough to encode these special cases.)
--
-- See <https://en.wikipedia.org/wiki/Vedic_square wikipedia> for more detail.
newtype VedicSquare (n::Nat) = VedicSquare (Z n)

deriveHierarchy ''VedicSquare [''Eq]

instance KnownNat n => Semigroup (VedicSquare n) where
    (VedicSquare v1)+(VedicSquare v2) = VedicSquare $ v1*v2

instance KnownNat n => Monoid (VedicSquare n) where
    zero = VedicSquare one

------------------------------------------------------------------------------
-- Minkowski addition

-- | TODO: implement
-- More details available at <https://en.wikipedia.org/wiki/Minkowski_addition wikipedia>.



