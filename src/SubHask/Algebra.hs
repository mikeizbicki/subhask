{-# LANGUAGE CPP #-}

module SubHask.Algebra
    where

import Debug.Trace

import Data.Ratio
import qualified Data.Strict.Maybe as Strict
import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category

-------------------------------------------------------------------------------
-- type classes

class Semigroup g where
    infixl 6 +
    (+) :: g -> g -> g

class Semigroup g => Monoid g where
    zero :: g

data AddUnit' g = Unit' | AddUnit' !g
    deriving (Read,Show,Eq,Ord)

instance Semigroup g => Semigroup (AddUnit' g) where
    Unit' + Unit' = Unit'
    Unit' + (AddUnit' g2) = AddUnit' g2
    (AddUnit' g1) + Unit' = AddUnit' g1
    (AddUnit' g1) + (AddUnit' g2) = AddUnit' $ g1 + g2

instance Semigroup g => Monoid (AddUnit' g) where
    zero = Unit'

---------------------------------------

class Monoid g => Group g where
    negate :: g -> g

    {-# INLINE (-) #-}
    infixl 6 -
    (-) :: g -> g -> g
    a - b = a + negate b

---------------------------------------

class Monoid m => Abelian m

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

---------------------------------------

class (Abelian r, Group r) => Rng r where
    infixl 7 *
    (*) :: r -> r -> r

---------------------------------------

class Rng r => Ring r where
    one :: r

    -- | NOTE: The default definition is extremely inefficient for most types
    -- and should be specialized to something better.
    fromInteger :: Integer -> r
    fromInteger i = if i>0
        then          foldl' (+) zero $ map (const (one::r)) [1..        i] 
        else negate $ foldl' (+) zero $ map (const (one::r)) [1.. negate i]

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

---------------------------------------

type family Scalar m

type IsScalar r = Scalar r ~ r

type HasScalar a = IsScalar (Scalar a)

---------------------------------------

class (Abelian m, Group m, HasScalar m, Ring (Scalar m)) => Module m where
    infixl 7 *.
    (*.) :: Scalar m -> m -> m

    infixl 7 .*.
    (.*.) :: m -> m -> m

    basis :: m

{-# INLINE (.*) #-}
infixl 7 .*
(.*) :: Module m => m -> Scalar m -> m
m .* r  = r *. m

---------------------------------------

class (Module v, Field (Scalar v)) => VectorSpace v where
    {-# INLINE (./) #-}
    infixl 7 ./
    (./) :: v -> Scalar v -> v
    v ./ r = v .* reciprocal r

    infixl 7 ./.
    (./.) :: v -> v -> v

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

-- FIXME: add Normed constraint
class 
    ( Field (Scalar v)
    , Normed v
--     , Ord (Scalar v)
--     , IsScalar (Scalar v)
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

-- | FIXME: add rewrite rules for speed
--
-- https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29
class Boolean b where
    infixr 3 &&
    infixr 2 ||
    (&&) :: b -> b -> b
    (||) :: b -> b -> b

    true :: b
    false :: b

-------------------------------------------------------------------------------
-- generic structures

instance Semigroup () where
    ()+() = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)
    
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)
    
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
    (a1,b1,c1,d1)+(a2,b2,c2,d2) = (a1+a2,b1+b2,c1+c2,d1+d2)
    
---------------------------------------

instance Monoid () where
    zero = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    zero = (zero,zero,zero,zero)

---------------------------------------

instance Group () where
    negate () = ()

instance (Group a, Group b) => Group (a,b) where
    negate (a,b) = (negate a,negate b)

instance (Group a, Group b, Group c) => Group (a,b,c) where
    negate (a,b,c) = (negate a,negate b,negate c)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    negate (a,b,c,d) = (negate a,negate b,negate c,negate d)

-------------------------------------------------------------------------------
-- standard numbers

instance Semigroup Int      where (+) = (P.+)
instance Semigroup Integer  where (+) = (P.+)
instance Semigroup Float    where (+) = (P.+)
instance Semigroup Double   where (+) = (P.+)
instance Semigroup Rational where (+) = (P.+)
instance Semigroup Bool     where (+) = (||)

instance Monoid Int       where zero = 0 
instance Monoid Integer   where zero = 0
instance Monoid Float     where zero = 0
instance Monoid Double    where zero = 0
instance Monoid Rational  where zero = 0
instance Monoid Bool      where zero = False

instance Group Int        where negate = P.negate
instance Group Integer    where negate = P.negate
instance Group Float      where negate = P.negate
instance Group Double     where negate = P.negate
instance Group Rational   where negate = P.negate
instance Group Bool       where negate = P.not

instance Abelian Int        
instance Abelian Integer    
instance Abelian Float      
instance Abelian Double    
instance Abelian Rational 
instance Abelian Bool

-------------------

instance Rng Int         where (*) = (P.*)
instance Rng Integer     where (*) = (P.*)
instance Rng Float       where (*) = (P.*)
instance Rng Double      where (*) = (P.*)
instance Rng Rational    where (*) = (P.*)
instance Rng Bool        where (*) = (&&)

instance Ring Int         where one = 1; fromInteger = P.fromInteger
instance Ring Integer     where one = 1; fromInteger = P.fromInteger
instance Ring Float       where one = 1; fromInteger = P.fromInteger
instance Ring Double      where one = 1; fromInteger = P.fromInteger
instance Ring Rational    where one = 1; fromInteger = P.fromInteger
instance Ring Bool        where one = True; fromInteger = (/= 0)

instance Field Float      where (/) = (P./)
instance Field Double     where (/) = (P./)
instance Field Rational   where (/) = (P./)

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

-------------------

type instance Scalar Int      = Int
type instance Scalar Integer  = Integer
type instance Scalar Float    = Float
type instance Scalar Double   = Double
type instance Scalar Rational = Rational

instance Module Int       where (*.) = (*); (.*.) = (*)
instance Module Integer   where (*.) = (*); (.*.) = (*)
instance Module Float     where (*.) = (*); (.*.) = (*)
instance Module Double    where (*.) = (*); (.*.) = (*)
instance Module Rational  where (*.) = (*); (.*.) = (*)

instance Normed Int       where abs = P.abs 
instance Normed Integer   where abs = P.abs 
instance Normed Float     where abs = P.abs 
instance Normed Double    where abs = P.abs 
instance Normed Rational  where abs = P.abs 

instance VectorSpace Float     where (./) = (/); (./.) = (/)
instance VectorSpace Double    where (./) = (/); (./.) = (/)
instance VectorSpace Rational  where (./) = (/); (./.) = (/)

---------------------------------------

instance Boolean Bool where 
    (&&) = (P.&&)
    (||) = (P.||)
    true = True
    false = False

instance Boolean b => Boolean (a -> b) where
    f && g = \x -> f x && g x
    f || g = \x -> f x || g x
    true = \x -> true
    false = \x -> false

---------------------------------------

instance Semigroup   b => Semigroup   (a -> b) where f+g = \a -> f a + g a
instance Monoid      b => Monoid      (a -> b) where zero = \a -> zero
instance Group       b => Group       (a -> b) where negate f = negate . f
instance Abelian     b => Abelian     (a -> b)
instance Rng         b => Rng         (a -> b) where f*g = \a -> f a * f a
instance Ring        b => Ring        (a -> b) where one = \a -> one
instance Field       b => Field       (a -> b) where reciprocal f = reciprocal . f
instance Module      b => Module      (a -> b) where b  *. f = \a -> b    *. f a
                                                     g .*. f = \a -> g a .*. f a
instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a
type instance Scalar (a -> b) = Scalar b

