module SubHask.Algebra.Matrix
    where

import SubHask.Category
import SubHask.Algebra
import SubHask.Algebra.Vector
import SubHask.Internal.Prelude

--------------------------------------------------------------------------------
--

data family Sparse_ (a :: * -> *) (n :: Nat) t

newtype instance Sparse_ a 1 t = SparseVector (a (Int,t))

newtype instance Sparse_ a i t = Sparse_ (Array (Sparse_ a (i-1) t))


---------

-- newtype (+>) a b =
