module SubHask.Algebra.Trans.CompensatedSum
    where

import SubHask.Category
import SubHask.Algebra
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

import Control.Monad

-------------------------------------------------------------------------------

newtype Uncompensated s = Uncompensated s

deriveHierarchy ''Uncompensated
    [ ''Ord
    , ''Boolean
    , ''InnerProductSpace
    , ''Ring
    , ''Unfoldable
    ]

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


