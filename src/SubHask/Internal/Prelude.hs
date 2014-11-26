module SubHask.Internal.Prelude
    (
    -- * classes
    Show (..)
    , Read (..)
    , read

    -- * data types
    , String
    , Char
    , Int
    , Integer
    , Float
    , Double
    , Rational
    , Bool (..)

    , IO
    , Maybe (..)
    , Either (..)

    -- * functions
    , build
    , (++)

    , Prelude.all
    , map

    , asTypeOf
    , ifThenElse
    , undefined
    , otherwise
    , error
    , flip
    , seq

    -- * Modules
    , module Data.Proxy
    , module GHC.TypeLits

    -- * Non-base types
    , Arbitrary (..)
    , NFData (..)
    , Constraint
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
