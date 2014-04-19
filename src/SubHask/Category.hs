module SubHask.Category
    where

import GHC.Prim
import qualified Prelude as P

-------------------------------------------------------------------------------

class Category cat where

--     type ValidCategory cat (a :: k1) (b :: k2) :: Constraint
--     type ValidCategory cat (a :: *)  (b :: *) = ()

    type ValidCategory cat a b :: Constraint
    type ValidCategory cat a b = ()

    id :: ValidCategory cat a a => cat a a

    infixr 9 .
    (.) :: 
        ( ValidCategory cat b c
        , ValidCategory cat a b
        , ValidCategory cat a c
        ) => cat b c -> cat a b -> cat a c

-------------------

-- data Cat a b cat1 cat2 = Cat (cat1 a b -> cat2 a b)
-- 
-- instance Category (Cat a b) where
--     type ValidCategory (Cat a b) cat1 cat2 =
--         ( ValidCategory cat1 a b
--         , ValidCategory cat2 a b
--         )
-- 
--     id = Cat id
--     (Cat f).(Cat g) = Cat $ f.g
--     
-- 
-- data Linear r a b = Linear (a r -> b r)
-- 
-- instance Category (Linear r) where
--     type ValidCategory (Linear r) (a :: * -> *) (b :: * -> *) = ()
--     id = Linear id
--     (Linear f1).(Linear f2) = Linear $ f1.f2

-- class NCategory cat where
-- 
--     type ValidNCategory cat (a :: * -> *) (b :: * -> *) :: Constraint
--     type ValidNCategory cat a b = ()
-- 
--     idN :: ValidCategory cat a a => cat a a
-- 
--     infixr 9 ...
--     (...) :: 
--         ( ValidCategory cat b c
--         , ValidCategory cat a b
--         , ValidCategory cat a c
--         ) => cat b c -> cat a b -> cat a c
-- 

---------------------------------------

class SubCategory cat subcat where
    embed :: ValidCategory subcat a b => subcat a b -> cat a b

instance SubCategory a a where
    embed = id

-------------------

class Category cat => Groupoid cat where
    inverse :: ValidCategory cat a b => cat a b -> cat b a

class SubCategory (->) cat => ConcreteCategory cat 

instance SubCategory (->) cat => ConcreteCategory cat


-------------------------------------------------------------------------------

class Category cat => Monoidal cat where
    data Tensor cat a b

    type Unit cat
    type Unit cat = ()

    associator :: proxy cat -> ((a,b),c) -> (a,(b,c))


-------------------------------------------------------------------------------
-- example categories

instance Category (->) where
    id = P.id
    (.) = (P..)

infixr 0 $

($) :: (SubCategory (->) subcat, ValidCategory subcat a b) => subcat a b -> a -> b
($) = embed
