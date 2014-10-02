{-# LANGUAGE CPP #-}

module SubHask.Algebra
    where

import Control.Monad -- required for deriving clauses
import qualified Prelude as P

import Data.Ratio
import qualified Data.Strict.Maybe as Strict

import SubHask.Internal.Prelude
import SubHask.Category

-------------------------------------------------------------------------------
-- type classes


class Eq a where
    infix 4 ==
    (==) :: a -> a -> Bool

    infix 4 /=
    (/=) :: a -> a -> Bool
    a1 /= a2 = not $ a1 == a2

instance Eq Int         where (==) = (P.==)
instance Eq Integer     where (==) = (P.==)
instance Eq Float       where (==) = (P.==)
instance Eq Double      where (==) = (P.==)
instance Eq Rational    where (==) = (P.==)

instance Eq () where
    ()==() = True

instance (Eq a, Eq b) => Eq (a,b) where
    (a1,b1)==(a2,b2) = a1==a2 && b1==b2

instance (Eq a, Eq b, Eq c) => Eq (a,b,c) where
    (a1,b1,c1)==(a2,b2,c2) = a1==a2 && b1==b2 && c1==c2

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a,b,c,d) where
    (a1,b1,c1,d1)==(a2,b2,c2,d2) = a1==a2 && b1==b2 && c1==c2 && d1==d2

---------------------------------------

class Eq g => Semigroup g where
    infixl 6 +
    (+) :: g -> g -> g

instance Semigroup Int      where (+) = (P.+)
instance Semigroup Integer  where (+) = (P.+)
instance Semigroup Float    where (+) = (P.+)
instance Semigroup Double   where (+) = (P.+)
instance Semigroup Rational where (+) = (P.+)
instance Semigroup Bool     where (+) = (||)

instance Semigroup () where
    ()+() = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
    (a1,b1,c1,d1)+(a2,b2,c2,d2) = (a1+a2,b1+b2,c1+c2,d1+d2)

instance Semigroup   b => Semigroup   (a -> b) where f+g = \a -> f a + g a

---------------------------------------

class Semigroup g => Monoid g where
    zero :: g

---------

data AddUnit' g = Unit' | AddUnit' !g
    deriving (Read,Show)

instance Eq g => Eq (AddUnit' g) where
    Unit'         == Unit'         = True
    (AddUnit' a1) == (AddUnit' a2) = a1==a2
    _             == _             = False

instance Semigroup g => Semigroup (AddUnit' g) where
    Unit' + Unit' = Unit'
    Unit' + (AddUnit' g2) = AddUnit' g2
    (AddUnit' g1) + Unit' = AddUnit' g1
    (AddUnit' g1) + (AddUnit' g2) = AddUnit' $ g1 + g2

instance Semigroup g => Monoid (AddUnit' g) where
    zero = Unit'

---------

instance Monoid Int       where zero = 0
instance Monoid Integer   where zero = 0
instance Monoid Float     where zero = 0
instance Monoid Double    where zero = 0
instance Monoid Rational  where zero = 0
instance Monoid Bool      where zero = False

instance Monoid () where
    zero = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    zero = (zero,zero,zero,zero)

instance Monoid      b => Monoid      (a -> b) where zero = \a -> zero

---------------------------------------

class Monoid g => Group g where
    negate :: g -> g

    {-# INLINE (-) #-}
    infixl 6 -
    (-) :: g -> g -> g
    a - b = a + negate b

instance Group Int        where negate = P.negate
instance Group Integer    where negate = P.negate
instance Group Float      where negate = P.negate
instance Group Double     where negate = P.negate
instance Group Rational   where negate = P.negate
instance Group Bool       where negate = P.not

instance Group () where
    negate () = ()

instance (Group a, Group b) => Group (a,b) where
    negate (a,b) = (negate a,negate b)

instance (Group a, Group b, Group c) => Group (a,b,c) where
    negate (a,b,c) = (negate a,negate b,negate c)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    negate (a,b,c,d) = (negate a,negate b,negate c,negate d)

instance Group       b => Group       (a -> b) where negate f = negate . f

---------------------------------------

class Monoid m => Abelian m

instance Abelian Int
instance Abelian Integer
instance Abelian Float
instance Abelian Double
instance Abelian Rational
instance Abelian Bool

instance Abelian ()
instance (Abelian a, Abelian b) => Abelian (a,b)
instance (Abelian a, Abelian b, Abelian c) => Abelian (a,b,c)
instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a,b,c,d)

instance Abelian     b => Abelian     (a -> b)

---------------------------------------

-- | FIXME: What constraint should be here? Semigroup?
--
-- See <http://ncatlab.org/nlab/show/normed%20group ncatlab>
class
--     ( Group g
    ( Ord (Scalar g)
    , HasScalar g
    ) => Normed g where
    abs :: g -> Scalar g

instance Normed Int       where abs = P.abs
instance Normed Integer   where abs = P.abs
instance Normed Float     where abs = P.abs
instance Normed Double    where abs = P.abs
instance Normed Rational  where abs = P.abs

---------------------------------------

class (Abelian r, Group r) => Rng r where
    infixl 7 *
    (*) :: r -> r -> r

instance Rng Int         where (*) = (P.*)
instance Rng Integer     where (*) = (P.*)
instance Rng Float       where (*) = (P.*)
instance Rng Double      where (*) = (P.*)
instance Rng Rational    where (*) = (P.*)
instance Rng Bool        where (*) = (&&)

instance Rng         b => Rng         (a -> b) where f*g = \a -> f a * f a

---------------------------------------

class Rng r => Ring r where
    one :: r

    -- | NOTE: The default definition is extremely inefficient for most types
    -- and should be specialized to something better.
    fromInteger :: Integer -> r
    fromInteger i = if i>0
        then          foldl' (+) zero $ map (const (one::r)) [1..        i]
        else negate $ foldl' (+) zero $ map (const (one::r)) [1.. negate i]

instance Ring Int         where one = 1; fromInteger = P.fromInteger
instance Ring Integer     where one = 1; fromInteger = P.fromInteger
instance Ring Float       where one = 1; fromInteger = P.fromInteger
instance Ring Double      where one = 1; fromInteger = P.fromInteger
instance Ring Rational    where one = 1; fromInteger = P.fromInteger
instance Ring Bool        where one = True; fromInteger = (/= 0)

instance Ring        b => Ring        (a -> b) where one = \a -> one

---------------------------------------

-- | 'Integral' numbers can be formed from a wideclass of things that behave
-- like integers, but intuitively look nothing like integers.
--
-- FIXME: Add a bunch of examples.  I'm not yet sure if this is the right
-- abstraction for all of these member functions.
--
-- See wikipedia on <https://en.wikipedia.org/wiki/Integral_element integral elements>
-- and the <https://en.wikipedia.org/wiki/Ring_of_integers ring of integers>.
class Ring a => Integral a where
    infixl 7  `quot`, `rem`, `div`, `mod`
    quot                :: a -> a -> a
    rem                 :: a -> a -> a
    div                 :: a -> a -> a
    mod                 :: a -> a -> a
    toInteger           :: a -> Integer

{-# NOINLINE [1] fromIntegral #-}
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

{-# RULES
"fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

instance Integral Int where
    div = P.div
    mod = P.mod
    quot = P.quot
    rem = P.rem
    toInteger = P.toInteger

instance Integral Integer where
    div = P.div
    mod = P.mod
    quot = P.quot
    rem = P.rem
    toInteger = P.toInteger

---------------------------------------

class Ring r => Field r where
    {-# INLINE reciprocal #-}
    reciprocal :: r -> r
    reciprocal r = one/r

    {-# INLINE (/) #-}
    infixl 7 /
    (/) :: r -> r -> r
    n/d = n * reciprocal d

    {-# INLINE fromRational #-}
    fromRational :: Rational -> r
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Field Float      where (/) = (P./)
instance Field Double     where (/) = (P./)
instance Field Rational   where (/) = (P./)

instance Field       b => Field       (a -> b) where reciprocal f = reciprocal . f

---------------------------------------

-- | A 'QuotientField' is a field with an 'IntegralDomain' as a subring.  There
-- may be many such subrings.  This is especially true in Haskell because we
-- have different data types that represent essentially the same ring (e.g.
-- "Int" and "Integer").  Therefore this is a multiparameter type class.  The
-- 'r' parameter represents the quotient field, and the 's' parameter represents
-- the subring.  The main purpose of this class is to provide functions that
-- map elements in 'r' to elements in 's' in various ways.
--
-- FIXME: Need examples.  Is there a better representation?
--
-- See <http://en.wikipedia.org/wiki/Field_of_fractions wikipedia> for more
-- details.
class QuotientField r s where
    truncate    :: r -> s
    round       :: r -> s
    ceiling     :: r -> s
    floor       :: r -> s

#define mkQuotientField(r,s) \
instance QuotientField r s where \
    truncate = P.truncate; \
    round    = P.round; \
    ceiling  = P.ceiling; \
    floor    = P.floor

mkQuotientField(Float,Int)
mkQuotientField(Float,Integer)
mkQuotientField(Double,Int)
mkQuotientField(Double,Integer)
mkQuotientField(Rational,Int)
mkQuotientField(Rational,Integer)

---------------------------------------

-- | TODO: add rest of Floating functions
class Field r => Floating r where
    pi :: r
    exp :: r -> r
    sqrt :: r -> r
    log :: r -> r
    (**) :: r -> r -> r
    isNaN :: r -> Bool
    infixl 8 **

instance Floating Float where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)
    isNaN = P.isNaN

instance Floating Double where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)
    isNaN = P.isNaN

---------------------------------------

type family Scalar m

type IsScalar r = Scalar r ~ r
type HasScalar a = IsScalar (Scalar a)

type instance Scalar Int      = Int
type instance Scalar Integer  = Integer
type instance Scalar Float    = Float
type instance Scalar Double   = Double
type instance Scalar Rational = Rational

type instance Scalar (a,b) = Scalar a
type instance Scalar (a,b,c) = Scalar a
type instance Scalar (a,b,c,d) = Scalar a

type instance Scalar (a -> b) = Scalar b

---------------------------------------

class (Abelian m, Group m, HasScalar m, Ring (Scalar m)) => Module m where
    infixl 7 *.
    (*.) :: Scalar m -> m -> m

    infixl 7 .*.
    (.*.) :: m -> m -> m

{-# INLINE (.*) #-}
infixl 7 .*
(.*) :: Module m => m -> Scalar m -> m
m .* r  = r *. m

instance (Module a, Module b, Scalar a ~ Scalar b) => Module (a,b) where
    r *. (a,b) = (r*.a, r*.b)
    (a1,b1).*.(a2,b2) = (a1.*.a2,b1.*.b2)

instance (Module a, Module b, Module c, Scalar a ~ Scalar b, Scalar a ~ Scalar c) => Module (a,b,c) where
    r *. (a,b,c) = (r*.a, r*.b,r*.c)
    (a1,b1,c1).*.(a2,b2,c2) = (a1.*.a2,b1.*.b2,c1.*.c2)

instance
    ( Module a, Module b, Module c, Module d
    , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a~Scalar d
    ) => Module (a,b,c,d)
        where
    r *. (a,b,c,d) = (r*.a, r*.b,r*.c,r*.d)
    (a1,b1,c1,d1).*.(a2,b2,c2,d2) = (a1.*.a2,b1.*.b2,c1.*.c2,d1.*.d2)

instance Module Int       where (*.) = (*); (.*.) = (*)
instance Module Integer   where (*.) = (*); (.*.) = (*)
instance Module Float     where (*.) = (*); (.*.) = (*)
instance Module Double    where (*.) = (*); (.*.) = (*)
instance Module Rational  where (*.) = (*); (.*.) = (*)

instance Module      b => Module      (a -> b) where
    b  *. f = \a -> b    *. f a
    g .*. f = \a -> g a .*. f a

---------------------------------------

class (Module v, Field (Scalar v)) => VectorSpace v where
    {-# INLINE (./) #-}
    infixl 7 ./
    (./) :: v -> Scalar v -> v
    v ./ r = v .* reciprocal r

    infixl 7 ./.
    (./.) :: v -> v -> v

instance (VectorSpace a,VectorSpace b, Scalar a ~ Scalar b) => VectorSpace (a,b) where
    (a,b) ./ r = (a./r,b./r)
    (a1,b1)./.(a2,b2) = (a1./.a2,b1./.b2)

instance (VectorSpace a, VectorSpace b, VectorSpace c, Scalar a ~ Scalar b, Scalar a ~ Scalar c) => VectorSpace (a,b,c) where
    (a,b,c) ./ r = (a./r,b./r,c./r)
    (a1,b1,c1)./.(a2,b2,c2) = (a1./.a2,b1./.b2,c1./.c2)

instance
    ( VectorSpace a, VectorSpace b, VectorSpace c, VectorSpace d
    , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a~Scalar d
    ) => VectorSpace (a,b,c,d)
        where
    (a,b,c,d)./r = (a./r, b./r,c./r,d./r)
    (a1,b1,c1,d1)./.(a2,b2,c2,d2) = (a1./.a2,b1./.b2,c1./.c2,d1./.d2)

instance VectorSpace Float     where (./) = (/); (./.) = (/)
instance VectorSpace Double    where (./) = (/); (./.) = (/)
instance VectorSpace Rational  where (./) = (/); (./.) = (/)

instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a

---------------------------------------

-- |
--
-- Note: It is not axiomatic that an inner product space's field must be non-finite (and hence normed and ordered).
-- However, it necessarily follows from the axioms.
-- Therefore, we include these class constraints.
-- In practice, this greatly simplifies many type signatures.
-- See this <http://math.stackexchange.com/questions/49348/inner-product-spaces-over-finite-fields stackoverflow question> for a detailed explanation of these constraints.
--
-- Note: Similarly, it is not axiomatic that every 'InnerProductSpace' is a 'MetricSpace'.
-- This is easy to see, however, since the "innerProductNorm" function can be used to define a metric on any inner product space.
-- The implementation will probably not be efficient, however.
--
-- Note: Machine learning papers often talk about Hilbert spaces, which are a minor extension of inner product spaces.
-- Specifically, the metric space must be complete.
-- I know of no useful complete metric spaces that can be represented in finite space on a computer, however, so we use the more general inner product spaces in this library.
class
    ( VectorSpace v
    , MetricSpace v
    , HasScalar v
    , Normed (Scalar v)
    , Floating (Scalar v)
    ) => InnerProductSpace v
        where

    infix 8 <>
    (<>) :: v -> v -> Scalar v

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: InnerProductSpace v => v -> Scalar v
squaredInnerProductNorm v = v<>v

{-# INLINE innerProductNorm #-}
innerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
innerProductNorm = sqrt . squaredInnerProductNorm

{-# INLINE innerProductDistance #-}
innerProductDistance :: (Floating (Scalar v), InnerProductSpace v) => v -> v -> Scalar v
innerProductDistance v1 v2 = innerProductNorm $ v1-v2

---------------------------------------

-- | FIXME: This needs to relate to a Monoidal category
class
    ( VectorSpace v
    , Scalar (Outer v) ~ Scalar v
    , Ring (Outer v)
    ) => OuterProductSpace v
        where
    type Outer v
    infix 8 ><
    (><) :: v -> v -> Outer v

---------------------------------------

class
    ( Field (Scalar v)
    , Normed v
    , Eq v
    ) => MetricSpace v
        where

    distance :: v -> v -> Scalar v

    {-# INLINE isFartherThan #-}
    isFartherThan :: v -> v -> Scalar v -> Bool
    isFartherThan s1 s2 b = case isFartherThanWithDistance s1 s2 b of
        Strict.Nothing -> True
        Strict.Just _ -> False

    {-# INLINE isFartherThanWithDistance #-}
    isFartherThanWithDistance :: v -> v -> Scalar v -> Strict.Maybe (Scalar v)
    isFartherThanWithDistance s1 s2 b = if dist > b
        then Strict.Nothing
        else Strict.Just $ dist
        where
            dist = distance s1 s2

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError :: CanError (Scalar v) => v -> v -> Scalar v -> Scalar v
    isFartherThanWithDistanceCanError s1 s2 b = if dist > b
        then errorVal
        else dist
        where
            dist = distance s1 s2

---------

class CanError a where
    errorVal :: a
    isError :: a -> Bool

instance CanError (Maybe a) where
    {-# INLINE isError #-}
    isError Nothing = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Nothing

instance CanError (Strict.Maybe a) where
    {-# INLINE isError #-}
    isError Strict.Nothing = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Strict.Nothing

instance CanError Float where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
    errorVal = 0/0

instance CanError Double where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
    errorVal = 0/0

---------------------------------------

data POrdering
    = PLT
    | PGT
    | PEQ
    | PNA

class Eq a => POrd a where
    pcompare :: a -> a -> POrdering

    infix 4 <
    (<) :: a -> a -> Bool
    a1 < a2 = case pcompare a1 a2 of
        PLT -> True
        otherwise -> False

    infix 4 <=
    (<=) :: a -> a -> Bool
    a1 <= a2 = case pcompare a1 a2 of
        PLT -> True
        PEQ -> True
        otherwise -> False

    infix 4 >
    (>) :: a -> a -> Bool
    a1 > a2 = case pcompare a1 a2 of
        PGT -> True
        otherwise -> False

    infix 4 >=
    (>=) :: a -> a -> Bool
    a1 >= a2 = case pcompare a1 a2 of
        PGT -> True
        PEQ -> True
        otherwise -> False

preludeOrdering2POrdering :: P.Ordering -> POrdering
preludeOrdering2POrdering P.LT = PLT
preludeOrdering2POrdering P.GT = PGT
preludeOrdering2POrdering P.EQ = PEQ

instance POrd Int       where pcompare a1 a2 = preludeOrdering2POrdering $ P.compare a1 a2
instance POrd Integer   where pcompare a1 a2 = preludeOrdering2POrdering $ P.compare a1 a2
instance POrd Float     where pcompare a1 a2 = preludeOrdering2POrdering $ P.compare a1 a2
instance POrd Double    where pcompare a1 a2 = preludeOrdering2POrdering $ P.compare a1 a2
instance POrd Rational  where pcompare a1 a2 = preludeOrdering2POrdering $ P.compare a1 a2

data Ordering
    = LT
    | GT
    | EQ

class POrd a => Ord a where
    compare :: a -> a -> Ordering
    compare a1 a2 = case pcompare a1 a2 of
        PLT -> LT
        PGT -> GT
        PEQ -> EQ
        PNA -> error "PNA given by pcompare on a totally ordered type"

preludeOrdering2Ordering :: P.Ordering -> Ordering
preludeOrdering2Ordering P.LT = LT
preludeOrdering2Ordering P.GT = GT
preludeOrdering2Ordering P.EQ = EQ

instance Ord Int        where compare a1 a2 = preludeOrdering2Ordering $ P.compare a1 a2
instance Ord Integer    where compare a1 a2 = preludeOrdering2Ordering $ P.compare a1 a2
instance Ord Float      where compare a1 a2 = preludeOrdering2Ordering $ P.compare a1 a2
instance Ord Double     where compare a1 a2 = preludeOrdering2Ordering $ P.compare a1 a2
instance Ord Rational   where compare a1 a2 = preludeOrdering2Ordering $ P.compare a1 a2

-- data Ordering
--     = LT
--     | GT
--     | EQ
--
-- class Ord a where
--     compare :: a -> a -> Pordering

-- | FIXME: add rewrite rules for speed
--
-- See <https://en.wikipedia.org/wiki/Lattice_%28order%29 wikipedia> for more details.
class POrd b => Lattice b where
    -- | "&&" takes the max of its two arguments; that is, a ≤ b implies a && b = b
    infixr 3 &&
    (&&) :: b -> b -> b

    -- | "||" takes the min of its two arguments; that is, a ≥ b implies a || b = b
    infixr 2 ||
    (||) :: b -> b -> b

sup :: Lattice b => [b] -> b
sup = foldl1' (||)

inf :: Lattice b => [b] -> b
inf = foldl1' (&&)

-- | Most Lattice literature only considers 'Bounded' lattices, but here we have both upper and lower bounded lattices.
class Lattice b => UpperBound b where
    -- | "false" is an upper bound because `a && false = false` for all a.
    false :: b

-- | Most Lattice literature only considers 'Bounded' lattices, but here we have both upper and lower bounded lattices.
class Lattice b => LowerBound b where
    true :: b

-- | A Boolean algebra is a special type of Ring.
--
-- See <https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29 wikipedia> for more details.
class (UpperBound b, LowerBound b) => Boolean b where
    not :: b -> b

-------------------

instance Eq [a] where
    (==) [] [] = True
    (==) [] _  = False
    (==) _  [] = False
    (==) (x:xs) (y:ys) = xs==ys


instance POrd [a] where
    pcompare [] [] = PEQ
    pcompare [] _  = PLT
    pcompare _  [] = PGT
    pcompare (x:xs) (y:ys) = pcompare xs ys

instance Eq a => Lattice [a] where
    (&&) = intersectBy (==)
    (||) = unionBy (==)

instance Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False

instance POrd Bool where
    pcompare False False = PEQ
    pcompare True True = PEQ
    pcompare False True = PGT
    pcompare True False = PLT

instance Lattice Bool where (&&) = (P.&&); (||) = (P.||)
instance UpperBound Bool where false = False
instance LowerBound Bool where true = True
instance Boolean Bool where not = P.not

instance Eq b => Eq (a -> b) where
    (==) = error "equality for (a -> b) exists, but is not computable"
instance POrd b => POrd (a -> b) where
    pcompare f g = error "partial ordering for (a -> b) exists, but is not computable"
instance Lattice b => Lattice (a -> b) where
    f && g = \x -> f x && g x
    f || g = \x -> f x || g x
instance UpperBound b => UpperBound (a -> b) where false = \x -> false
instance LowerBound b => LowerBound (a -> b) where true = \x -> true
instance Boolean b => Boolean (a -> b) where not f = \x -> not $ f x

-------------------------------------------------------------------------------
-- playing

class Semigroup s => Set s where
    type Elem s :: *
    elem :: Elem s -> s -> Bool

-- class (Set s, Cat (+>)) => EndoFunctor s (+>) where
--     efmap :: (a +> b) -> s { Elem :: a } +> s { Elem :: b }
--     efmap :: (a +> b) -> s a +> s b
--
-- class (Pointed s, EndoFunctor s (+>)) => Applicative s (+>) where
--     (<*>) :: s { Elem :: (a +> b) } -> s { Elem :: a } +> s { Elem :: b }
--     (<*>) :: s (a +> b) -> s a +> s b
--
-- class (Pointed s, EndoFunctor s (+>)) => Monad s (+>) where
--     join :: ValidCategory (+>) a => s (s a) +> s a

class Set s => Pointed s where
    singleton :: Elem s -> s

    -- | For an Abelian set, cons==snoc
    insert :: Abelian s => Elem s -> s -> s
    insert = cons

    -- | inserts an element on the left
    cons :: Elem s -> s -> s
    cons x xs = singleton x + xs

    -- | inserts an element on the right
    snoc :: Elem s -> s -> s
    snoc x xs = xs + singleton x

class Set s => Indexed s where
    type Index s :: *
    (!) :: s -> Index s -> Elem s

class (Abelian s, Indexed s) => Pindexed s where
    singletonAt :: Index s -> Elem s -> s

    insertAt :: Index s -> Elem s -> s -> s
    insertAt i e s = singletonAt i e + s

class (Boolean s, Set s) => Topology s

class (Normed s, Topology s) => Measurable s where

newtype Borel r = Borel [Ball r]

class Set s => MultiSet s where
    numElem :: Elem s -> s -> Scalar s

-------------------

instance Semigroup [a] where
    xs+ys = xs++ys

instance Monoid [a] where
    zero = []

instance Eq a => Set [a] where
    type Elem [a] = a
    elem _ []       = False
    elem x (y:ys)   = x==y || elem x ys

-------------------

data Ball v = Ball
    { radius :: !(Scalar v)
    , center :: !v
    }

type instance Scalar (Ball v) = Scalar v

instance MetricSpace v => Semigroup (Ball v) where
    b1+b2 = b1' { radius = distance (center b1') (center b2') + radius b2' }
        where
            (b1',b2') = if radius b1 > radius b2
                then (b1,b2)
                else (b2,b1)

instance InnerProductSpace v => Set (Ball v) where
    type Elem (Ball v) = v
    elem x b = not $ isFartherThan x (center b) (radius b)

instance (Eq v, Eq (Scalar v)) => Eq (Ball v) where
    b1 == b2 = radius b1 == radius b2 && center b1 == center b2

instance MetricSpace v => POrd (Ball v) where
    pcompare b1 b2 = if dist == 0 && radius b1 == radius b2
        then PEQ
        else if dist <= radius b1 - radius b2
            then PGT
            else if dist <= radius b2 - radius b1
                then PLT
                else PNA
        where
            dist = distance (center b1) (center b2)

-------------------

newtype VectorBall v = VectorBall (Ball v)

type instance Scalar (VectorBall v) = Scalar v

-- instance InnerProductSpace v => Lattice (VectorBall v) where
--     (VectorBall b1) || (VectorBall b2) = VectorBall $ Ball
--         { radius = (distance (center b1) (center b2) + radius b1 + radius b2) / 2
--         , center = ( (1-radius b2+radius b1)*.center b1
--                    + (1-radius b1+radius b2)*.center b2
--                    ) ./ 2
--         }
--
--     -- FIXME: this is incorrect!
--     (VectorBall b1) && (VectorBall b2) = VectorBall $ Ball
--         { radius = max 0 $ radius b1 + radius b2 - 2*distance (center b1) (center b2)
--         , center = (radius b1 *. center b1 + radius b2 *. center b2) ./ (radius b1 + radius b2)
--         }


-- class InnerProductSpace v => RepresentableVector v where
--     type BasisIndex v :: *
--
--     getBasis :: BasisIndex v -> v
--
--     (!) :: v -> BasisIndex v -> Scalar v
--     (!) v i = getBasis i <> v

