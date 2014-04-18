module SubHask.Category.Constrained
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

newtype Constrained (xs :: [* -> Constraint]) a b = Constrained (a -> b)

type family AppConstraints (f :: [* -> Constraint]) (a :: *) :: Constraint
type instance AppConstraints '[] a = ()
type instance AppConstraints (x ': xs) a = (x a, AppConstraints xs a)

instance Category (Constrained xs) where
    type ValidCategory (Constrained xs) a b = (AppConstraints xs a, AppConstraints xs b)
    id = Constrained P.id
    (Constrained f).(Constrained g) = Constrained (f.g)

instance SubCategory (->) (Constrained xs) where
    embed (Constrained f) = f

embedOrd :: 
    ( SubCategory (Constrained '[P.Ord]) subcat
    , ValidCategory subcat a b
    ) => subcat a b -> Constrained '[P.Ord] a b
embedOrd = embed
