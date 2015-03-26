{-# LANGUAGE NoRebindableSyntax #-}

-- | This file contains a LOT of instance declarations for making Base code compatible with SubHask type classes.
-- There's very little code in here though.
-- Most instances are generated using the functions in "SubHask.TemplateHaskell.Base".
module SubHask.Compatibility.Base
    where

import Data.Typeable
import qualified Prelude as P
import qualified Control.Applicative as A
import qualified Control.Monad as M
import Language.Haskell.TH

import Control.Arrow
import Control.Monad.ST
import GHC.Conc.Sync
import GHC.GHCi
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

import SubHask.Algebra
import SubHask.Category
import SubHask.Monad
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Base
import SubHask.TemplateHaskell.Deriving


--------------------------------------------------------------------------------
-- automatic instances

instance Functor Hask NoIO where fmap = M.liftM -- required for GHCI Monad instance

-- deriveAll

-- forAllInScope ''P.Eq             mkPreludeEq
forAllInScope ''P.Functor        mkPreludeFunctor
-- forAllInScope ''A.Applicative    mkPreludeApplicative
forAllInScope ''M.Monad          mkPreludeMonad

--------------------------------------------------------------------------------

type instance Logic TypeRep = Bool

instance Eq_ TypeRep where
    (==) = (P.==)

instance POrd_ TypeRep where
    inf x y = case P.compare x y of
        LT -> x
        _  -> y
instance Lattice_ TypeRep where
    sup x y = case P.compare x y of
        GT -> x
        _  -> y
instance Ord_ TypeRep where compare = P.compare

---------

instance (Semigroup b) => Semigroup (Either a b) where
    (Left a) + _ = Left a
    _ + (Left a) = Left a
    (Right b1)+(Right b2) = Right $ b1+b2

instance (Monoid b) => Monoid (Either a b) where
    zero = Right zero

---------

instance P.Monad Maybe' where
    return = Just'
    Nothing' >>= f = Nothing'
    (Just' a) >>= f = f a

instance Functor Hask Maybe' where
    fmap f Nothing' = Nothing'
    fmap f (Just' a) = Just' $ f a

instance Then Maybe' where
    Nothing' >> _ = Nothing'
    _        >> a = a

instance Monad Hask Maybe' where
    return_ = Just'
    join Nothing' = Nothing'
    join (Just' Nothing') = Nothing'
    join (Just' (Just' a)) = Just' a
