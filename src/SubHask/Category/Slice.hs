module SubHask.Category.Slice
    where

import GHC.Prim
import qualified Prelude as P

import SubHask.Category

-------------------------------------------------------------------------------

data (cat / (obj :: *)) a b = Slice (cat a b) 

instance Category cat => Category (cat/obj) where
    type ValidCategory (cat/obj) a b = 
        ( ValidCategory cat a obj
        , ValidCategory cat b obj
        , ValidCategory cat a b
        , Category cat
        )

    id = Slice id
    (Slice f).(Slice g) = Slice $ f.g

runSlice :: ValidCategory (cat/obj) a b => (cat/obj) a b -> (cat b obj) -> (cat a obj)
runSlice (Slice cat1) cat2 = cat2.cat1

