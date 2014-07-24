module SubHask.Algebra.Objects
    where

import GHC.Prim
import GHC.TypeLits
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
-- import SubHask.Category.Finite
-- import SubHask.Category.Trans.Bijective

-------------------------------------------------------------------------------
-- integers modulo n

-- | The type of integers modulo n
newtype Z (n::Nat) = Z Integer
    deriving (Read,Show,Eq,Ord)

-- | safe constructor that takes the mod of the input
mkZ :: forall n. KnownNat n => Integer -> Z n
mkZ i = Z $ i `mod` n
    where
        n = natVal (Proxy :: Proxy n)

instance KnownNat n => Monoid (Z n) where
    zero = Z 0
    (Z z1) + (Z z2) = mkZ $ z1 + z2 

instance KnownNat n => Group (Z n) where
    negate (Z i) = mkZ $ negate i 

instance KnownNat n => Abelian (Z n) 

instance KnownNat n => Ring (Z n) where
    one = Z 1
    (Z z1)*(Z z2) = mkZ $ z1 * z2

type instance Scalar (Z n) = Integer

instance KnownNat n => Module (Z n) where
    i .* z = Z i * z

-- | Extended Euclid's algorithm is used to calculate inverses in modular arithmetic
extendedEuclid :: Integer -> Integer -> (Integer,Integer,Integer,Integer,Integer,Integer)
extendedEuclid a b = go 0 1 1 0 b a
    where
        go s1 s0 t1 t0 0  r0 = (s1,s0,t1,t0,0,r0)
        go s1 s0 t1 t0 r1 r0 = go s1' s0' t1' t0' r1' r0'
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
newtype Galois (p::Nat) (k::Nat) = Galois (Z (p^k))
    deriving (Read,Show,Eq)

deriving instance KnownNat (p^k) => Monoid (Galois p k)
deriving instance KnownNat (p^k) => Abelian (Galois p k)
deriving instance KnownNat (p^k) => Group (Galois p k)
deriving instance KnownNat (p^k) => Ring (Galois p k)

type instance Scalar (Galois p k) = Scalar (Z (p^k))

instance KnownNat (p^k) => Module  (Galois p k) where
    i .* z = Galois (Z i) * z

instance (Prime p, KnownNat (p^k)) => Field (Galois p k) where
    reciprocal (Galois (Z i)) = Galois $ mkZ $ t
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

instance KnownNat n => Monoid (VedicSquare n) where
    zero = VedicSquare one
    (VedicSquare v1)+(VedicSquare v2) = VedicSquare $ v1*v2 

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
