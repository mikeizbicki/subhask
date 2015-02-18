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
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------
-- non-negative objects

newtype NonNegative t = NonNegative { unNonNegative :: t }

deriveHierarchy ''NonNegative [ ''Enum, ''Boolean, ''Rig, ''MetricSpace ]

-- instance (Ord t, Group t) => Cancellative (NonNegative t) where
--     (NonNegative t1)-(NonNegative t2) = if diff>zero
--         then NonNegative diff
--         else NonNegative zero
--         where
--             diff=t1-t2

-------------------

{-
newtype a +> b = HomHask { unHomHask :: a -> b }
infixr +>

unsafeHomHask2 :: (a -> b -> c) -> (a +> b +> c)
unsafeHomHask2 f = HomHask (\a -> HomHask $ \b -> f a b)

instance Category (+>) where
    type ValidCategory (+>) a = ()
    id = HomHask id
    (HomHask a).(HomHask b) = HomHask $ a.b

instance Sup (+>) (->) (->)
instance Sup (->) (+>) (->)
instance (+>) <: (->) where
    embedType_ = Embed2 unHomHask

instance Monoidal (+>) where
    type Tensor (+>) = (,)
    tensor = unsafeHomHask2 $ \a b -> (a,b)

instance Braided (+>) where
    braid  = HomHask $ \(a,b) -> (b,a)
    unbraid = braid

instance Closed (+>) where
    curry (HomHask f) = HomHask $ \ a -> HomHask $ \b -> f (a,b)
    uncurry (HomHask f) = HomHask $ \ (a,b) -> unHomHask (f a) b

mkSubtype [t|Int|] [t|Integer|] 'toInteger

[subhask|
poop :: (Semigroup' g, Ring g) => g +> g
poop = (+:1)
|]

class Semigroup' a where
    (+:) :: a +> a +> a

instance Semigroup' Int where (+:) = unsafeHomHask2 (+)

instance Semigroup' [a] where (+:) = unsafeHomHask2 (+)

f :: Integer +> Integer
f = HomHask $ \i -> i+1

n1 = NonNegative 5 :: NonNegative Int
n2 = NonNegative 3 :: NonNegative Int
i1 = 5 :: Int
i2 = 3 :: Int
j1 = 5 :: Integer
j2 = 3 :: Integer
-}

-------------------------------------------------------------------------------
-- integers modulo n

-- | Maps members of an equivalence class into the "canonical" element.
class Quotient a (b::k) where
    mkQuotient :: a -> a/b

-- | The type of equivalence classes created by a mod b.
newtype (/) (a :: *) (b :: k) = Mod a

instance (Quotient a b, Arbitrary a) => Arbitrary (a/b) where
    arbitrary = liftM mkQuotient arbitrary

deriveHierarchyFiltered ''(/) [ ''Eq_, ''P.Ord ] [''Arbitrary]

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

instance (Module a, Quotient a b) => Module (a/b) where
    x        *. (Mod a) = mkQuotient $ x  *. a
    (Mod b) .*. (Mod a) = mkQuotient $ b .*. a

-- | The type of integers modulo n
type Z (n::Nat) = Integer/n

instance KnownNat n => Quotient Int n
    where
        mkQuotient i = Mod $ i `P.mod` (fromIntegral $ natVal (Proxy::Proxy n))

instance KnownNat n => Quotient Integer n
    where
        mkQuotient i = Mod $ i `P.mod` (natVal (Proxy::Proxy n))

-- | Extended Euclid's algorithm is used to calculate inverses in modular arithmetic
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

-- | @Galois p k@ is the type of integers modulo p^k, where p is prime.
-- All finite fields have this form.
--
-- See wikipedia <https://en.wikipedia.org/wiki/Finite_field> for more details.
--
-- FIXME: Many arithmetic operations over Galois Fields can be implemented more efficiently than the standard operations.
-- See <http://en.wikipedia.org/wiki/Finite_field_arithmetic>.
newtype Galois (p::Nat) (k::Nat) = Galois (Z (p^k))

deriveHierarchy ''Galois [''Eq_]

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
-- the vedic square

-- | The Vedic Square always forms a monoid,
-- and sometimes forms a group depending on the value of "n".
-- (The type system isn't powerful enough to encode these special cases.)
--
-- See <https://en.wikipedia.org/wiki/Vedic_square wikipedia> for more detail.
newtype VedicSquare (n::Nat) = VedicSquare (Z n)

deriveHierarchy ''VedicSquare [''Eq_]

instance KnownNat n => Semigroup (VedicSquare n) where
    (VedicSquare v1)+(VedicSquare v2) = VedicSquare $ v1*v2

instance KnownNat n => Monoid (VedicSquare n) where
    zero = VedicSquare one

------------------------------------------------------------------------------
-- Minkowski addition

-- | TODO: implement
-- More details available at <https://en.wikipedia.org/wiki/Minkowski_addition wikipedia>.



