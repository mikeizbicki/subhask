module SubHask.Algebra
    where

import Debug.Trace

import Data.Ratio
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

class (Abelian g, Group g) => Normed g where
    abs :: g -> g

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

-- | TODO: add rest of Floating functions
class Field r => Floating r where
    pi :: r
    exp :: r -> r
    sqrt :: r -> r
    log :: r -> r
    (**) :: r -> r -> r
    infixl 8 **

---------------------------------------

type family Scalar m

type IsScalar r = Scalar r ~ r

type HasScalar a = IsScalar (Scalar a)

---------------------------------------

class (Abelian m, Group m, HasScalar m, Ring (Scalar m)) => Module m where
    {-# INLINE (.*) #-}
    infixl 7 .*
    (.*) :: Scalar m -> m -> m
    r .* m = m *. r 

    {-# INLINE (*.) #-}
    infixl 7 *.
    (*.) :: m -> Scalar m -> m
    m *. r  = r .* m

---------------------------------------

class (Module v, Field (Scalar v)) => VectorSpace v where
    {-# INLINE (/.) #-}
    infixl 7 /.
    (/.) :: v -> Scalar v -> v
    v /. r = v *. reciprocal r

---------------------------------------

class VectorSpace v => InnerProductSpace v where
    infix 8 <>
    (<>) :: v -> v -> Scalar v

{-# INLINE innerProductNorm #-}
innerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
innerProductNorm = sqrt . squaredInnerProductNorm

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
squaredInnerProductNorm v = sqrt $ v<>v 

{-# INLINE innerProductDistance #-}
innerProductDistance :: (Floating (Scalar v), InnerProductSpace v) => v -> v -> Scalar v
innerProductDistance v1 v2 = innerProductNorm $ v1-v2

---------------------------------------

class VectorSpace v => OuterProduct v where
    type Outer v 
    outerProduct :: v -> v -> Outer v
--     (><) :: v -> v -> Outer v

---------------------------------------

class (Field (Scalar v), IsScalar (Scalar v)) => MetricSpace v where
    distance :: v -> v -> Scalar v

---------------------------------------

class (InnerProductSpace v, MetricSpace v) => HilbertSpace v

-------------------------------------------------------------------------------
-- generic structures

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)
    
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)
    
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
    (a1,b1,c1,d1)+(a2,b2,c2,d2) = (a1+a2,b1+b2,c1+c2,d1+d2)
    
---------------------------------------

instance (Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    zero = (zero,zero,zero,zero)

---------------------------------------

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

instance Monoid Int       where  zero = 0 
instance Monoid Integer   where  zero = 0
instance Monoid Float     where  zero = 0
instance Monoid Double    where  zero = 0
instance Monoid Rational  where  zero = 0

instance Group Int        where negate = P.negate
instance Group Integer    where negate = P.negate
instance Group Float      where negate = P.negate
instance Group Double     where negate = P.negate
instance Group Rational   where negate = P.negate

instance Abelian Int        
instance Abelian Integer    
instance Abelian Float      
instance Abelian Double    
instance Abelian Rational 

-------------------

instance Rng Int         where (*) = (P.*)
instance Rng Integer     where (*) = (P.*)
instance Rng Float       where (*) = (P.*)
instance Rng Double      where (*) = (P.*)
instance Rng Rational    where (*) = (P.*)

instance Ring Int         where one = 1; fromInteger = P.fromInteger
instance Ring Integer     where one = 1; fromInteger = P.fromInteger
instance Ring Float       where one = 1; fromInteger = P.fromInteger
instance Ring Double      where one = 1; fromInteger = P.fromInteger
instance Ring Rational    where one = 1; fromInteger = P.fromInteger

instance Field Float      where (/) = (P./)
instance Field Double     where (/) = (P./)
instance Field Rational   where (/) = (P./)

instance Floating Float where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)

instance Floating Double where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)

-------------------

type instance Scalar Int      = Int
type instance Scalar Integer  = Integer
type instance Scalar Float    = Float
type instance Scalar Double   = Double
type instance Scalar Rational = Rational

instance Module Int       where (.*) = (*)
instance Module Integer   where (.*) = (*)
instance Module Float     where (.*) = (*)
instance Module Double    where (.*) = (*)
instance Module Rational  where (.*) = (*)

instance Normed Int       where abs = P.abs 
instance Normed Integer   where abs = P.abs 
instance Normed Float     where abs = P.abs 
instance Normed Double    where abs = P.abs 
instance Normed Rational  where abs = P.abs 

instance VectorSpace Float     where (/.) = (/)
instance VectorSpace Double    where (/.) = (/)
instance VectorSpace Rational  where (/.) = (/)

-- instance Module Int       Int       where (.*) = (*)
-- instance Module Integer   Integer   where (.*) = (*)
-- instance Module Float     Float     where (.*) = (*)
-- instance Module Double    Double    where (.*) = (*)
-- instance Module Rational  Rational  where (.*) = (*)
-- 
-- instance VectorSpace Float     Float     where (/.) = (/)
-- instance VectorSpace Double    Double    where (/.) = (/)
-- instance VectorSpace Rational  Rational  where (/.) = (/)
