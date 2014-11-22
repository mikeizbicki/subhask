{-# LANGUAGE RebindableSyntax #-}

module SubHask.Algebra.Objects
    where

import Control.Monad
import GHC.Prim
import Control.Monad
import GHC.TypeLits
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Quotient
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving
-- import SubHask.Category.Finite
-- import SubHask.Category.Trans.Bijective

-------------------------------------------------------------------------------
-- non-negative objects

newtype NonNegative t = NonNegative t

deriveHierarchy ''NonNegative [ ''Ord, ''Boolean, ''Rig, ''MetricSpace ]

instance (Ord t, Group t) => Cancellative (NonNegative t) where
    (NonNegative t1)-(NonNegative t2) = if diff>zero
        then NonNegative diff
        else NonNegative zero
        where
            diff=t1-t2

-------------------------------------------------------------------------------
-- integers modulo n

-- | The type of integers modulo n
type Z (n::Nat) = Integer/n

instance KnownNat n => Arbitrary (Integer / n) where
    arbitrary = liftM mkZ arbitrary

-- newtype Z (n::Nat) = Z Integer
--     deriving (Read,Show,Eq,Ord)

-- | safe constructor that takes the mod of the input
mkZ :: forall n. KnownNat n => Integer -> Z n
mkZ i = Mod $ i `mod` n
    where
        n = natVal (Proxy :: Proxy n)


instance KnownNat n => CanonicalEq (Int/n)
    where
        canonicalize (Mod i) = Mod $ i `P.mod` (fromIntegral $ natVal (Proxy::Proxy n))

instance KnownNat n => CanonicalEq (Integer/n)
    where
        canonicalize (Mod i) = Mod $ i `P.mod` (natVal (Proxy::Proxy n))

-------------------
-- algebra

instance KnownNat n => Semigroup (Int    /n) where (Mod z1) + (Mod z2) = quotient $ z1 + z2
instance KnownNat n => Semigroup (Integer/n) where (Mod z1) + (Mod z2) = quotient $ z1 + z2

instance KnownNat n => Monoid (Int    /n) where zero = Mod 0
instance KnownNat n => Monoid (Integer/n) where zero = Mod 0

instance KnownNat n => Cancellative (Int    /n) where (Mod i1)-(Mod i2) = quotient $ i1-i2
instance KnownNat n => Cancellative (Integer/n) where (Mod i1)-(Mod i2) = quotient $ i1-i2

instance KnownNat n => Group (Int    /n) where negate (Mod i) = quotient $ negate i
instance KnownNat n => Group (Integer/n) where negate (Mod i) = quotient $ negate i

instance KnownNat n => Abelian (Int    /n)
instance KnownNat n => Abelian (Integer/n)

instance KnownNat n => Rg (Int    /n) where (Mod z1)*(Mod z2) = quotient $ z1 * z2
instance KnownNat n => Rg (Integer/n) where (Mod z1)*(Mod z2) = quotient $ z1 * z2

instance KnownNat n => Rig (Int    /n) where one = Mod 1
instance KnownNat n => Rig (Integer/n) where one = Mod 1

instance KnownNat n => Ring (Int    /n) where fromInteger i = quotient $ fromInteger i
instance KnownNat n => Ring (Integer/n) where fromInteger i = quotient $ fromInteger i

type instance Scalar (Int    /n) = Int
type instance Scalar (Integer/n) = Integer

instance KnownNat n => Module (Int    /n) where x        *. (Mod a) = quotient $ x  *. a
                                                (Mod b) .*. (Mod a) = quotient $ b .*. a
instance KnownNat n => Module (Integer/n) where x        *. (Mod a) = quotient $ x  *. a
                                                (Mod b) .*. (Mod a) = quotient $ b .*. a

-- | Extended Euclid's algorithm is used to calculate inverses in modular arithmetic
--
-- FIXME: need another implementation of
extendedEuclid :: (Eq t, Integral t) => t -> t -> (t,t,t,t,t,t)
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

-- | @Galois p k@ is the type of integers modulo p^k, where p is prime.  All
-- finite fields have this form.
--
-- See wikipedia <https://en.wikipedia.org/wiki/Finite_field> for more details.
--
-- FIXME: Many arithmetic operations over Galois Fields can be implemented more
-- efficiently than the standard operations.  See
-- <http://en.wikipedia.org/wiki/Finite_field_arithmetic>.
newtype Galois (p::Nat) (k::Nat) = Galois (Z (p^k))
    deriving (Read,Show,Eq)

deriving instance KnownNat (p^k) => Semigroup (Galois p k)
deriving instance KnownNat (p^k) => Monoid (Galois p k)
deriving instance KnownNat (p^k) => Abelian (Galois p k)
deriving instance KnownNat (p^k) => Cancellative (Galois p k)
deriving instance KnownNat (p^k) => Group (Galois p k)
deriving instance KnownNat (p^k) => Rg (Galois p k)
deriving instance KnownNat (p^k) => Rig (Galois p k)
deriving instance KnownNat (p^k) => Ring (Galois p k)

type instance Scalar (Galois p k) = Scalar (Z (p^k))

instance KnownNat (p^k) => Module  (Galois p k) where
    i   *. z  = Galois (Mod i) * z
    z1 .*. z2 = z1 * z2

instance (Prime p, KnownNat (p^k)) => Field (Galois p k) where
    reciprocal (Galois (Mod i)) = Galois $ mkZ $ t
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
-- the vedic square

-- | The Vedic Square always forms a monoid, and sometimes forms a group
-- depending on the value of "n".  (The type system isn't powerful enough to
-- encode these special cases.)  See Wikipedia for more details
-- <https://en.wikipedia.org/wiki/Vedic_square>
newtype VedicSquare (n::Nat) = VedicSquare (Z n)
    deriving (Eq)

instance KnownNat n => Semigroup (VedicSquare n) where
    (VedicSquare v1)+(VedicSquare v2) = VedicSquare $ v1*v2

instance KnownNat n => Monoid (VedicSquare n) where
    zero = VedicSquare one

------------------------------------------------------------------------------
-- Minkowski addition

-- | TODO: implement
-- More details available at <https://en.wikipedia.org/wiki/Minkowski_addition wikipedia>.

-------------------------------------------------------------------------------
-- hask algebra

{-
instance Abelian b => Abelian (a -> b)
instance Monoid b => Monoid (a -> b) where
    zero = \a -> zero
    f+g = \a -> f a + g a

instance Group b => Group (a -> b) where
    negate f = negate . f

type instance Scalar (a -> b) = Scalar b

instance Module b => Module (a -> b) where
    r .* f = \a -> r .* f a

instance VectorSpace b => VectorSpace (a -> b) where
    f /. r = \a -> f a /. r
-}
