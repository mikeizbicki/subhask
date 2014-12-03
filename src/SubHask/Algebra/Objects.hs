{-# LANGUAGE RebindableSyntax,QuasiQuotes #-}

module SubHask.Algebra.Objects
    where

import Control.Monad
import GHC.Prim
import Control.Monad
import GHC.TypeLits
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Quotient
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving
-- import SubHask.Category.Finite
-- import SubHask.Category.Trans.Bijective

--------------------------------------------------------------------------------
-- | A Boolean algebra is a special type of Ring.
-- Their applications (set-like operations) tend to be very different than Rings, so it makes sense for the class hierarchies to be completely unrelated.
-- The "BooleanRing" type, however, provides the correct transformation.
newtype BooleanRing b = BooleanRing b
    deriving (Read,Show,Arbitrary,NFData,Eq)

mkBooleanRing :: Boolean b => b -> BooleanRing b
mkBooleanRing = BooleanRing

instance Boolean b => Semigroup (BooleanRing b) where
--     (BooleanRing b1)+(BooleanRing b2) = BooleanRing $ (b1 && not b2) || (not b1 && b2)
    (BooleanRing b1)+(BooleanRing b2) = BooleanRing $ (b1 || b2) && not (b1 && b2)

instance Boolean b => Abelian (BooleanRing b)

instance Boolean b => Monoid (BooleanRing b) where
    zero = BooleanRing $ false

instance Boolean b => Cancellative (BooleanRing b) where
    (-)=(+)
--     b1-b2 = b1+negate b2

instance Boolean b => Group (BooleanRing b) where
    negate = id
--     negate (BooleanRing b) = BooleanRing $ not b

instance Boolean b => Rg (BooleanRing b) where
    (BooleanRing b1)*(BooleanRing b2) = BooleanRing $ b1 && b2

instance Boolean b => Rig (BooleanRing b) where
    one = BooleanRing $ true

instance Boolean b => Ring (BooleanRing b)

-------------------------------------------------------------------------------
newtype WithPreludeOrd a = WithPreludeOrd a
--     deriving (Read,Show,NFData,Eq,POrd,Ord,InfSemilattice,SupSemilattice,Lattice)

deriveHierarchy ''WithPreludeOrd [ ''Enum, ''Boolean, ''Rig, ''MetricSpace ]

instance Ord a => P.Ord (WithPreludeOrd a) where
    compare (WithPreludeOrd a1) (WithPreludeOrd a2) = compare a1 a2

-------------------------------------------------------------------------------
-- compatibility layer types

newtype IndexedVector k v = IndexedVector { unsafeGetMap :: Map.Map (WithPreludeOrd k) v }
    deriving (Read,Show,NFData)

type instance Scalar (IndexedVector k v) = Scalar v
type instance Elem (IndexedVector k v) = v

-- | This is the L2 norm of the vector.
instance (Floating (Scalar v), IsScalar v, Ord v) => Normed (IndexedVector k v) where
    {-# INLINE abs #-}
    abs (IndexedVector m) =
        {-# SCC abs_IndexedVector #-}
        sqrt $ sum $ map (**2) $ Map.elems m

instance (Floating (Scalar v), IsScalar v, Ord v, Ord k) => MetricSpace (IndexedVector k v) where
    {-# INLINE distance #-}
    distance (IndexedVector m1) (IndexedVector m2) =
        {-# SCC distance_IndexedVector #-}
        sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
        where
            go tot [] [] = tot
            go tot [] ((k,v):xs) = go (tot+v*v) [] xs
            go tot ((k,v):xs) [] = go (tot+v*v) [] xs

            go tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
                EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
                LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
                GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1

    isFartherThanWithDistanceCanError (IndexedVector m1) (IndexedVector m2) dist =
        {-# SCC isFartherThanWithDistanceCanError_IndexedVector #-}
        sqrt $ go 0 (Map.assocs m1) (Map.assocs m2)
        where
            dist2 = dist*dist

            go tot [] [] = tot
            go tot xs ys = if tot > dist2
                then errorVal
                else go' tot xs ys

            go' tot [] ((k,v):xs) = go (tot+v*v) [] xs
            go' tot ((k,v):xs) [] = go (tot+v*v) [] xs

            go' tot ((k1,v1):xs1) ((k2,v2):xs2) = case compare k1 k2 of
                EQ -> go (tot+(v1-v2)*(v1-v2)) xs1 xs2
                LT -> go (tot+v1*v1) xs1 ((k2,v2):xs2)
                GT -> go (tot+v2*v2) ((k1,v1):xs1) xs1

instance (Eq k, Eq v) => Eq (IndexedVector k v) where
    (IndexedVector m1)==(IndexedVector m2) = m1' == m2'
        where
            m1' = removeWithPreludeOrd $ Map.toList m1
            m2' = removeWithPreludeOrd $ Map.toList m2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd ((WithPreludeOrd k,v):xs) = (k,v):removeWithPreludeOrd xs

-- FIXME: this would be faster if we don't repeat all the work of the comparisons
-- FIXME: is this the correct instance?
-- FIXME: this is the ordering for an Array; create a SparseArray type
-- instance (Ord k, Eq v, Semigroup v) => POrd (IndexedVector k v) where
--     pcompare (IndexedVector m1) (IndexedVector m2) = if (IndexedVector m1)==(IndexedVector m2)
--         then PEQ
--         else if Map.isSubmapOfBy (==) m1 m2
--             then PLT
--             else if Map.isSubmapOfBy (==) m2 m1
--                 then PGT
--                 else PNA

instance (Ord k, POrd v, Semigroup v) => POrd (IndexedVector k v) where
    pcompare (IndexedVector m1) (IndexedVector m2) = go (Map.assocs m1) (Map.assocs m2)
        where
            go [] [] = PEQ
            go [] _  = PLT
            go _  [] = PGT
            go ((k1,v1):xs1) ((k2,v2):xs2) = case pcompare k1 k2 of
                PNA -> PNA
                PLT -> PLT
                PGT -> PGT
                PEQ -> case pcompare v1 v2 of
                    PNA -> PNA
                    PLT -> PLT
                    PGT -> PGT
                    PEQ -> go xs1 xs2

instance (Ord k, POrd v, Semigroup v) => Ord (IndexedVector k v) where

instance (Ord k, Semigroup v) => InfSemilattice (IndexedVector k v) where
    inf (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.unionWith (+) m1 m2

instance (Ord k, Semigroup v) => SupSemilattice (IndexedVector k v) where
    sup (IndexedVector m1) (IndexedVector m2) = IndexedVector $ Map.intersectionWith (+) m1 m2

instance (Ord k, Semigroup v) => Lattice (IndexedVector k v) where

instance (Ord k, Semigroup v) => Semigroup (IndexedVector k v) where
    (+) = inf

instance (Ord k, Semigroup v) => Monoid (IndexedVector k v) where
    zero = IndexedVector $ Map.empty

instance (Ord k, Abelian v) => Abelian (IndexedVector k v)

instance (Ord k, Semigroup v, Eq v) => Container (IndexedVector k v) where
    elem x (IndexedVector m) = elem x $ P.map snd $ Map.toList m

type instance Index (IndexedVector k v) = k

instance (Ord k, Semigroup v) => Indexed (IndexedVector k v) where
    (IndexedVector m) !! k = Map.lookup (WithPreludeOrd k) m

instance (Ord k, Semigroup v) => IndexedUnfoldable (IndexedVector k v) where
    singletonAt i e = IndexedVector $ Map.singleton (WithPreludeOrd i) e

    consAt i e (IndexedVector s) = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    snocAt (IndexedVector s) i e = IndexedVector $ Map.insert (WithPreludeOrd i) e s

    fromIndexedList xs = IndexedVector $ Map.fromList $ map (\(i,e) -> (WithPreludeOrd i,e)) xs

instance (Ord k, Semigroup v) => IndexedFoldable (IndexedVector k v) where
    toIndexedList (IndexedVector s) = map (\(WithPreludeOrd i,e) -> (i,e)) $ Map.assocs s

instance (Ord k, Semigroup v) => IndexedDeletable (IndexedVector k v) where
    deleteAt k (IndexedVector s) = IndexedVector $ Map.delete (WithPreludeOrd k) s

---------------------------------------

newtype Set a = Set (Set.Set (WithPreludeOrd a))

type instance Scalar (Set a) = Int
type instance Elem (Set a) = a

instance Normed (Set a) where
    abs (Set s) = Set.size s

instance Eq a => Eq (Set a) where
    (Set s1)==(Set s2) = s1'==s2'
        where
            s1' = removeWithPreludeOrd $ Set.toList s1
            s2' = removeWithPreludeOrd $ Set.toList s2
            removeWithPreludeOrd [] = []
            removeWithPreludeOrd (WithPreludeOrd x:xs) = x:removeWithPreludeOrd xs

-- | FIXME: this would be faster if we don't repeat all the work of the comparisons
instance Ord a => POrd (Set a) where
    pcompare (Set s1) (Set s2) = if (Set s1)==(Set s2)
        then PEQ
        else if Set.isSubsetOf s1 s2
            then PLT
            else if Set.isSubsetOf s2 s1
                then PGT
                else PNA

instance Ord a => InfSemilattice (Set a) where
    inf (Set s1) (Set s2) = Set $ Set.union s1 s2

instance Ord a => SupSemilattice (Set a) where
    sup (Set s1) (Set s2) = Set $ Set.intersection s1 s2

instance Ord a => Lattice (Set a) where

instance Ord a => MinBound (Set a) where
    minBound = Set $ Set.empty

instance Ord a => Semigroup (Set a) where
    (Set s1)+(Set s2) = Set $ Set.union s1 s2

instance Ord a => Monoid (Set a) where
    zero = Set $ Set.empty

instance Ord a => Abelian (Set a)

instance Ord a => Container (Set a) where
    elem a (Set s) = Set.member (WithPreludeOrd a)s

instance Ord a => Unfoldable (Set a) where
    singleton a = Set $ Set.singleton (WithPreludeOrd a)

-- newtype MutRef a s = MutRef (STRef s a)
--
-- instance Mutable a (MutRef a) where
--     unsafeFreeze (MutRef r) = readSTRef r
--     unsafeThaw = liftM MutRef . newSTRef

-------------------------------------------------------------------------------
-- non-negative objects

newtype NonNegative t = NonNegative { unNonNegative :: t }

deriveHierarchy ''NonNegative [ ''Enum, ''Boolean, ''Rig, ''MetricSpace ]

instance (Ord t, Group t) => Cancellative (NonNegative t) where
    (NonNegative t1)-(NonNegative t2) = if diff>zero
        then NonNegative diff
        else NonNegative zero
        where
            diff=t1-t2

-------------------


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

-- xx = $(sub[e| (1::Int) + (2::Integer) |])

-------------------------------------------------------------------------------
-- integers modulo n

-- | The type of integers modulo n
type Z (n::Nat) = Integer/n

instance KnownNat n => Arbitrary (Integer / n) where
    arbitrary = liftM mkZ arbitrary

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
