{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubHask.Compatibility.Cassava
    ( decode_
    , decode

    -- * Types
    , FromRecord
    , ToRecord
    , FromField
    , ToField
    , HasHeader (..)
    )
    where

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Parallel
import SubHask.Compatibility.ByteString

import qualified Prelude as P
import qualified Data.Csv as C
import Data.Csv (FromRecord, ToRecord, FromField, ToField, HasHeader)

--------------------------------------------------------------------------------
-- instances

instance FromField a => FromRecord (BArray a) where
    parseRecord = P.fmap fromList . C.parseRecord

instance (Constructible (UArray a), Monoid (UArray a), FromField a) => FromRecord (UArray a) where
    parseRecord = P.fmap fromList . C.parseRecord

--------------------------------------------------------------------------------
-- replacement functions

-- | This is a monoid homomorphism, which means it can be parallelized
decode_ ::
    ( FromRecord a
    ) => HasHeader
      -> PartitionOnNewline (ByteString Char)
      -> Either String (BArray a)
decode_ h (PartitionOnNewline (BSLC bs)) = case C.decode h bs of
    Right r -> Right $ BArray r
    Left s -> Left s

-- | Like the "decode" function in Data.Csv, but works in parallel
decode ::
    ( NFData a
    , FromRecord a
    ) => HasHeader
      -> ByteString Char
      -> Either String (BArray a)
decode h = parallel (decode_ h) . PartitionOnNewline
