module SubHask.Algebra
    where

import GHC.Prim
import GHC.TypeLits
import Data.Proxy
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

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

