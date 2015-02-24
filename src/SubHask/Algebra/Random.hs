module SubHask.Algebra.Random
    where

import SubHask.Algebra
import SubHask.Category
import SubHask.Monad

class Random t where
    sample :: Monad cat m => t -> m (Elem t)

class Monad Hask m => RandGen m where


data family Normal :: a -> *
