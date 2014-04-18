module SubHask.Algebra.Objects
    where

import GHC.Prim
import GHC.TypeLits
import qualified Prelude as P

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.SparseFunction
import SubHask.Category.Trans.Bijective

-------------------------------------------------------------------------------
-- the symmetric group

newtype Sym (n::Nat) = Sym (BijectiveT SparseFunction (Z n) (Z n))

instance KnownNat n => Monoid (Sym n) where
    zero = Sym id
    (Sym s1)+(Sym s2) = Sym $ s1.s2

instance KnownNat n => Group (Sym n) where
    negate (Sym s) = Sym $ inverse s
