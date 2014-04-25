module SubHask.Internal.Prelude
    (
    Eq (..)
    , Ord (..)
    , Read (..)
    , Show (..)

    , String
    , Char

    , Int
    , Integer
    , fromIntegral

    , Float
    , Double
    , Rational

    , Bool (..)

    , undefined
    , error

--     , module Data.Foldable
    , module Data.List
    , module Data.Maybe
    )
    where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Traversable
