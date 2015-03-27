{-# LANGUAGE NoRebindableSyntax #-}

-- | This file contains a LOT of instance declarations for making Base code compatible with SubHask type classes.
-- There's very little code in here though.
-- Most instances are generated using the functions in "SubHask.TemplateHaskell.Base".
module SubHask.Compatibility.Base
    ()
    where

import Data.Typeable
import qualified Prelude             as Base
import qualified Control.Applicative as Base
import qualified Control.Monad       as Base
import Language.Haskell.TH

import Control.Arrow
import Control.Monad.Identity (Identity(..))
import Control.Monad.State.Strict (State,StateT)
import Control.Monad.Trans
import Control.Monad.ST (ST)
import GHC.Conc.Sync
import GHC.GHCi
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

import Control.Monad.Random
import Pipes

import SubHask.Algebra
import SubHask.Category
import SubHask.Monad
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Base
import SubHask.TemplateHaskell.Deriving


--------------------------------------------------------------------------------
-- bug fixes

-- required for GHCI to work because NoIO does not have a Base.Functor instance
instance Functor Hask NoIO where fmap = Base.liftM

-- these definitions are required for the corresponding types to be in scope in the TH code below;
-- pretty sure this is a GHC bug
dummy1 = undefined :: Identity a
dummy2 = undefined :: StateT s m a
dummy3 = undefined :: Pipes.Proxy a b c d e f

--------------------------------------------------------------------------------
-- derive instances

-- forAllInScope ''Base.Eq             mkPreludeEq
forAllInScope ''Base.Functor        mkPreludeFunctor
-- forAllInScope ''Base.Applicative    mkPreludeApplicative
forAllInScope ''Base.Monad          mkPreludeMonad

--------------------------------------------------------------------------------

type instance Logic TypeRep = Bool

instance Eq_ TypeRep where
    (==) = (Base.==)

instance POrd_ TypeRep where
    inf x y = case Base.compare x y of
        LT -> x
        _  -> y
instance Lattice_ TypeRep where
    sup x y = case Base.compare x y of
        GT -> x
        _  -> y
instance Ord_ TypeRep where compare = Base.compare

---------

instance (Semigroup b) => Semigroup (Either a b) where
    (Left a) + _ = Left a
    _ + (Left a) = Left a
    (Right b1)+(Right b2) = Right $ b1+b2

instance (Monoid b) => Monoid (Either a b) where
    zero = Right zero

---------

instance Base.Monad Maybe' where
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
