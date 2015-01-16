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
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Vector

import qualified Data.Csv as C
import Data.Csv (FromRecord, ToRecord, FromField, ToField, HasHeader)

-- | This is a monoid homomorphism, which means it can be parallelized
decode_ ::
    ( FromRecord a
    ) => HasHeader
      -> PartitionOnNewline (ByteString Lazy Char)
      -> Either String (Array a)
decode_ h (PartitionOnNewline (BSLC bs)) = case C.decode h bs of
    Right r -> Right $ ArrayT r
    Left s -> Left s

-- | Like the "decode" function in Data.Csv, but works in parallel
decode ::
    ( NFData a
    , FromRecord a
    , ValidEq a
    ) => HasHeader
      -> ByteString Lazy Char
      -> Either String (Array a)
decode h = parallel (decode_ h) . PartitionOnNewline
