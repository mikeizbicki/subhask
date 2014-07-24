module SubHask.Algebra
    where

import Debug.Trace

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category

-------------------------------------------------------------------------------
-- type classes

class Monoid m where
    zero :: m

    infixl 6 +
    (+) :: m -> m -> m

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

class (Abelian r, Group r) => Ring r where
    one :: r

    infixl 7 *
    (*) :: r -> r -> r

---------------------------------------

class Ring r => Field r where
    {-# INLINE reciprocal #-}
    reciprocal :: r -> r
    reciprocal r = one/r

    {-# INLINE (/) #-}
    infixl 7 /
    (/) :: r -> r -> r
    n/d = n * reciprocal d

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

-- type IsScalar r = Scalar r ~ r

class (Scalar r ~ r) => IsScalar r where
    fromInteger :: Integer -> r

---------------------------------------

-- class (Abelian m, Group m, Scalar r~Scalar m) => Module r m where
--     {-# INLINE (.*) #-}
--     infixl 7 .*
--     (.*) :: r -> m -> m
--     r .* m = m *. r 
-- 
--     {-# INLINE (*.) #-}
--     infixl 7 *.
--     (*.) :: m -> r -> m
--     m *. r  = r .* m

class (Abelian m, Group m, IsScalar (Scalar m)) => Module m where
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
    (/.) :: v -> Scalar v -> v
    v /. r = v *. reciprocal r

---------------------------------------

class VectorSpace v => InnerProductSpace v where
    (<>) :: v -> v -> Scalar v

    {-# INLINE innerProductNorm #-}
    innerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
    innerProductNorm v = sqrt $ v<>v 

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

instance (Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    zero = (zero,zero,zero,zero)
    (a1,b1,c1,d1)+(a2,b2,c2,d2) = (a1+a2,b1+b2,c1+c2,d1+d2)

---------------------------------------

instance (Group a, Group b) => Group (a,b) where
    negate (a,b) = (negate a,negate b)

instance (Group a, Group b, Group c) => Group (a,b,c) where
    negate (a,b,c) = (negate a,negate b,negate c)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    negate (a,b,c,d) = (negate a,negate b,negate c,negate d)

-------------------------------------------------------------------------------
-- standard numbers

instance IsScalar Int       where fromInteger = P.fromInteger
instance IsScalar Integer   where fromInteger = P.fromInteger
instance IsScalar Float     where fromInteger = P.fromInteger
instance IsScalar Double    where fromInteger = P.fromInteger
instance IsScalar Rational  where fromInteger = P.fromInteger

-------------------

instance Monoid Int       where  zero = 0; (+) = (+)
instance Monoid Integer   where  zero = 0; (+) = (+)
instance Monoid Float     where  zero = 0; (+) = (+)
instance Monoid Double    where  zero = 0; (+) = (+)
instance Monoid Rational  where  zero = 0; (+) = (+)

instance Group Int        where negate = negate
instance Group Integer    where negate = negate
instance Group Float      where negate = negate
instance Group Double     where negate = negate
instance Group Rational   where negate = negate

instance Abelian Int        
instance Abelian Integer    
instance Abelian Float      
instance Abelian Double    
instance Abelian Rational 

-------------------

instance Ring Int         where one = 1; (*) = (*)
instance Ring Integer     where one = 1; (*) = (*)
instance Ring Float       where one = 1; (*) = (*)
instance Ring Double      where one = 1; (*) = (*)
instance Ring Rational    where one = 1; (*) = (*)

instance Field Float      where (/) = (/)
instance Field Double     where (/) = (/)
instance Field Rational   where (/) = (/)

instance Floating Float where
    pi = pi
    sqrt = sqrt
    log = log
    exp = exp
    (**) = (**)

instance Floating Double where
    pi = pi
    sqrt = sqrt
    log = log
    exp = exp
    (**) = (**)

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

