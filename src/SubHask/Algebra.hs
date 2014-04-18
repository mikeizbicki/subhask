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

-------------------------------------------------------------------------------
-- example: Z n

newtype Z (n::Nat) = Z P.Integer
    deriving (P.Read,P.Show,P.Eq,P.Ord)

instance KnownNat n => Finite (Z n) where
    type Order (Z n) = n
    index i = Index i
    deIndex (Index i) = i
    enumerate = [ Z i | i <- [0..n P.- 1] ]
        where
            n = natVal (Proxy :: Proxy n)

instance KnownNat n => Monoid (Z n) where
    zero = Z 0
    (Z z1) + (Z z2) = Z $ z1 P.+ z2 `P.mod` n
        where
            n = natVal (Proxy :: Proxy n)

instance KnownNat n => Group (Z n) where
    negate (Z i) = Z $ P.negate i `P.mod` n
        where
            n = natVal (Proxy :: Proxy n)

class Finite a where
    type Order a :: Nat
    index :: a -> Index a
    deIndex :: Index a -> a
    enumerate :: [a]
    
newtype Index a = Index (Z (Order a))
    deriving (P.Read,P.Show,P.Eq,P.Ord)

swapIndex :: Order a ~ Order b => Index a -> Index b
swapIndex (Index i) = Index i

