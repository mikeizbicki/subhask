module SubHask.Algebra.Trans.CompensatedSum
    where

import Control.Monad

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude

-------------------------------------------------------------------------------

newtype Uncompensated s = Uncompensated s
    deriving
        ( Read,Show
        , Eq,POrd,Ord,InfSemilattice,SupSemilattice,Lattice,MaxBound,MinBound,Heyting,Boolean
        , Semigroup,Monoid,Abelian,Cancellative,Group,Rg,Rig,Ring
        )

deriving instance Cone s => Cone (Uncompensated s)
deriving instance Module s => Module (Uncompensated s)
deriving instance VectorSpace s => VectorSpace (Uncompensated s)

deriving instance Normed s => Normed (Uncompensated s)
deriving instance MetricSpace s => MetricSpace (Uncompensated s)

type instance Scalar (Uncompensated s) = Scalar s

instance Container s => Container (Uncompensated s) where
    type Elem (Uncompensated s) = Elem s
--     type ElemConstraint (Uncompensated s) = ElemConstraint s
    elem x (Uncompensated s) = elem x s

deriving instance Unfoldable s => Unfoldable (Uncompensated s)

instance Foldable s => Foldable (Uncompensated s) where
    unCons (Uncompensated s) = case unCons s of
        Nothing -> Nothing
        Just (x,xs) -> Just (x, Uncompensated xs)

    unSnoc (Uncompensated s) = case unSnoc s of
        Nothing -> Nothing
        Just (xs,x) -> Just (Uncompensated xs,x)

    foldMap f   (Uncompensated s) = foldMap f   s
    foldr   f a (Uncompensated s) = foldr   f a s
    foldr'  f a (Uncompensated s) = foldr'  f a s
    foldr1  f   (Uncompensated s) = foldr1  f   s
    foldr1' f   (Uncompensated s) = foldr1' f   s
    foldl   f a (Uncompensated s) = foldl   f a s
    foldl'  f a (Uncompensated s) = foldl'  f a s
    foldl1  f   (Uncompensated s) = foldl1  f   s
    foldl1' f   (Uncompensated s) = foldl1' f   s

    sum = foldl' (+) zero


