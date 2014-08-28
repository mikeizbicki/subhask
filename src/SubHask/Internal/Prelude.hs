module SubHask.Internal.Prelude
    (
    Eq (..)
    , Ord (..)
    , Read (..)
    , Show (..)

    , Monad (..)

    , String
--     , Char

--     , Int
    , Integer
    , fromIntegral
    , mod
    , div

--     , Float
--     , Double
    , Rational

    , Bool (..)
    , (||), (&&)

    , ifThenElse
    , undefined
    , error
    , const

--     , module Data.Foldable
    , module Data.List
    , module Data.Maybe
    , module Data.Proxy
    , module GHC.TypeLits
    , module GHC.Exts
    )
    where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import GHC.Exts
import Prelude

{-# INLINE ifThenElse #-}
-- ifThenElse a b c = if a then b else c
ifThenElse a b c = case a of
    True -> b
    False -> c
