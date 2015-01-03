{-# LANGUAGE NoRebindableSyntax #-}

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
import Text.ParserCombinators.ReadP

import SubHask.Algebra
import SubHask.Category
import SubHask.Monad
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Monad
import SubHask.TemplateHaskell.Deriving


--------------------------------------------------------------------------------
-- monad instances

deriveAllInScope ''P.Functor        mkPreludeFunctor
-- deriveAllInScope ''A.Applicative    mkPreludeApplicative
deriveAllInScope ''M.Monad          mkPreludeMonad

--------------------------------------------------------------------------------

fromPreludeEq [t|TypeRep|]


type instance Logic (Maybe a) = Logic a

instance (Bounded (Logic a), Eq_ a) => Eq_ (Maybe a) where
    (Just a1) == (Just a2) = a1==a2
    Nothing   == Nothing   = true
    _         == _         = false


instance Semigroup b => Semigroup (Either a b) where
    (Left a) + _ = Left a
    _ + (Left a) = Left a
    (Right b1)+(Right b2) = Right $ b1+b2

instance Monoid b => Monoid (Either a b) where
    zero = Right zero

