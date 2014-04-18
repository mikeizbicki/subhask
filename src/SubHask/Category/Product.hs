module SubHask.Category.Product
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

data (><) cat1 cat2 a b = Product (cat1 a b, cat2 a b)

instance (Category cat1, Category cat2) => Category (cat1 >< cat2) where
    type ValidCategory (cat1><cat2) a b = (ValidCategory cat1 a b, ValidCategory cat2 a b)
    id = Product (id,id)
    (Product (c1,d1)).(Product (c2,d2)) = Product (c1.c2,d1.d2)


