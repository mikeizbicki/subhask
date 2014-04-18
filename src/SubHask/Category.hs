module SubHask.Category
    where

import GHC.Prim
import qualified Prelude as P

-------------------------------------------------------------------------------

class Category cat where

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
-- example categories

instance Category (->) where
    id = P.id
    (.) = (P..)

infixr 0 $

($) :: (SubCategory (->) subcat, ValidCategory subcat a b) => subcat a b -> a -> b
($) = embed

