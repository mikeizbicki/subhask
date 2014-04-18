module SubHask.Algebra
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------
-- type classes

class Monoid m where
    zero :: m
    (+) :: m -> m -> m

newtype Mon a b = Mon (a -> b)

instance Category Mon where
    type ValidCategory Mon a b = (Monoid a, Monoid b)
    id = Mon id
    (Mon f).(Mon g) = Mon (f.g)

instance SubCategory (->) Mon where
    embed (Mon f) = f

embedMon ::
    ( SubCategory Mon subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Mon a b
embedMon = embed

---------------------------------------

class Monoid g => Group g where
    negate :: g -> g
    (-) :: g -> g -> g
    (-) = (+).negate

newtype Grp a b = Grp (a -> b)

instance Category Grp where
    type ValidCategory Grp a b = (Group a, Group b)
    id = Grp id
    (Grp f).(Grp g) = Grp (f.g)

instance SubCategory (->) Grp where
    embed (Grp f) = f

instance SubCategory Mon Grp where
    embed (Grp f) = Mon f

embedGrp ::
    ( SubCategory Grp subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Grp a b
embedGrp = embed

---------------------------------------

class Monoid m => Abelian m

---------------------------------------

class (Abelian r, Group r) => Ring r where
    one :: r
    (*) :: r -> r -> r

---------------------------------------

class Ring r => Field r where
    reciprocal :: r -> r
    reciprocal r = one/r

    (/) :: r -> r -> r
    n/d = n * reciprocal d

---------------------------------------

class Field r => Floating r where
    pi :: r
    exp :: r -> r
    sqrt :: r -> r
    log :: r -> r
    (**) :: r -> r -> r
    -- TODO: add rest of Floating functions

---------------------------------------

type family Scalar m

class (Abelian m, Group m, Scalar r~Scalar m) => Module r m where
    (.*) :: r -> m -> m
    r .* m = m *. r 

    (*.) :: m -> r -> m
    m *. r  = r .* m

---------------------------------------

class (Module r v, Field r) => VectorSpace r v where
    (/.) :: v -> r -> v
    v /. r = v *. reciprocal r

---------------------------------------

class VectorSpace (Scalar v) v => InnerProductSpace v where
    (<>) :: v -> v -> Scalar v

innerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
innerProductNorm v = sqrt $ v<>v 

innerProductDistance :: (Floating (Scalar v), InnerProductSpace v) => v -> v -> Scalar v
innerProductDistance v1 v2 = innerProductNorm $ v1-v2

---------------------------------------

class MetricSpace v where
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

instance Monoid P.Int       where  zero = 0; (+) = (P.+)
instance Monoid P.Integer   where  zero = 0; (+) = (P.+)
instance Monoid P.Float     where  zero = 0; (+) = (P.+)
instance Monoid P.Double    where  zero = 0; (+) = (P.+)
instance Monoid P.Rational  where  zero = 0; (+) = (P.+)

instance Group P.Int        where negate = P.negate
instance Group P.Integer    where negate = P.negate
instance Group P.Float      where negate = P.negate
instance Group P.Double     where negate = P.negate
instance Group P.Rational   where negate = P.negate

instance Abelian P.Int        
instance Abelian P.Integer    
instance Abelian P.Float      
instance Abelian P.Double    
instance Abelian P.Rational 

-------------------

instance Ring P.Int         where one = 1; (*) = (P.*)
instance Ring P.Integer     where one = 1; (*) = (P.*)
instance Ring P.Float       where one = 1; (*) = (P.*)
instance Ring P.Double      where one = 1; (*) = (P.*)
instance Ring P.Rational    where one = 1; (*) = (P.*)

instance Field P.Float      where (/) = (P./)
instance Field P.Double     where (/) = (P./)
instance Field P.Rational   where (/) = (P./)

instance Floating P.Float where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)

instance Floating P.Double where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    (**) = (P.**)

-------------------

type instance Scalar P.Int      = P.Int
type instance Scalar P.Integer  = P.Integer
type instance Scalar P.Float    = P.Float
type instance Scalar P.Double   = P.Double
type instance Scalar P.Rational = P.Rational

instance Module P.Int       P.Int       where (.*) = (P.*)
instance Module P.Integer   P.Integer   where (.*) = (P.*)
instance Module P.Float     P.Float     where (.*) = (P.*)
instance Module P.Double    P.Double    where (.*) = (P.*)
instance Module P.Rational  P.Rational  where (.*) = (P.*)

instance VectorSpace P.Float     P.Float     where (/.) = (P./)
instance VectorSpace P.Double    P.Double    where (/.) = (P./)
instance VectorSpace P.Rational  P.Rational  where (/.) = (P./)

-------------------------------------------------------------------------------
-- example: Z n

newtype Z (n::Nat) = Z P.Integer
    deriving (P.Read,P.Show,P.Eq,P.Ord)

instance KnownNat n => Monoid (Z n) where
    zero = Z 0
    (Z z1) + (Z z2) = Z $ z1 + z2 `P.mod` n
        where
            n = natVal (Proxy :: Proxy n)

instance KnownNat n => Group (Z n) where
    negate (Z i) = Z $ negate i `P.mod` n
        where
            n = natVal (Proxy :: Proxy n)

instance KnownNat n => Abelian (Z n) 

instance KnownNat n => Ring (Z n) where
    one = Z 1
    (Z z1)*(Z z2) = Z $ z1 * z2 `P.mod` n
        where
            n = natVal (Proxy :: Proxy n)

type instance Scalar (Z n) = P.Integer

instance KnownNat n => Module P.Integer (Z n) where
    i .* z = Z i * z

