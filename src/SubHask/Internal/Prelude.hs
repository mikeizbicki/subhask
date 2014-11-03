module SubHask.Internal.Prelude
    (
--     Eq (..)
--     , Ord (compare)
--     Enum (..)
    Read (..)
    , read
    , Show (..)

    , build
    , (++)
--     , Monad (..)

    , String
    , Char

    , Prelude.all
    , map

    , Int
    , Integer

    , Float
    , Double
    , Rational

    , Bool (..)
--     , (||), (&&)

    , Maybe (..)
    , Either (..)

    , asTypeOf
    , ifThenElse
    , undefined
    , otherwise
    , error
--     , const
    , flip
    , seq
    , NFData (..)

    , Constraint
--     , module Data.Foldable
--     , module Data.List
    , module Data.Proxy
    , module GHC.TypeLits
--     , module Test.QuickCheck.Arbitrary

    -- * Non-base types
    , Arbitrary (..)
    )
    where

import Control.DeepSeq
import Data.Foldable
import Data.List (foldl, foldl', foldr, foldl1, foldl1', foldr1, map, (++), intersectBy, unionBy )
import Data.Maybe
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import GHC.Exts
import Prelude
import Test.QuickCheck.Arbitrary

{-# INLINE ifThenElse #-}
-- ifThenElse a b c = if a then b else c
ifThenElse a b c = case a of
    True -> b
    False -> c
