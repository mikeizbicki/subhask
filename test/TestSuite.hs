import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.Algebra.Group
import SubHask.Algebra.Container
import SubHask.Algebra.Logic
import SubHask.Algebra.Metric
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Vector
import SubHask.Compatibility.Containers

import SubHask.TemplateHaskell.Deriving
import SubHask.TemplateHaskell.Test
-- import Language.Haskell.TH hiding (Match)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (NonNegative)
import Test.QuickCheck.Arbitrary

main = defaultMain
    [ testGroup "simple"
        [ testGroup "numeric"
            [ $( mkSpecializedClassTests [t| Int      |] [''Enum,''Ring, ''Bounded, ''MetricSpace] )
            , $( mkSpecializedClassTests [t| Integer  |] [''Enum,''Ring, ''Lattice, ''MetricSpace] )
            , $( mkSpecializedClassTests [t| Rational |] [''Ord,''Ring, ''Lattice, ''MetricSpace] )
            , $( mkSpecializedClassTests [t| Float    |] [''Bounded] )
            , $( mkSpecializedClassTests [t| Double   |] [''Bounded] )
            , $( mkSpecializedClassTests [t| Uncompensated Int |] [ ''Ring ] )
            , testGroup "transformers"
                [ $( mkSpecializedClassTests [t| NonNegative Int  |] [''Enum,''Rig, ''Bounded, ''MetricSpace] )
                , $( mkSpecializedClassTests [t| Z 57             |] [''Ring] )
                , $( mkSpecializedClassTests [t| NonNegative (Z 57) |] [''Rig] )
                ]
            ]
        , testGroup "non-numeric"
            [ $( mkSpecializedClassTests [t| Bool      |] [''Enum,''Boolean] )
            , $( mkSpecializedClassTests [t| Char      |] [''Enum,''Bounded] )
            , $( mkSpecializedClassTests [t| Goedel    |] [''Heyting] )
            , $( mkSpecializedClassTests [t| H3        |] [''Heyting] )
            , $( mkSpecializedClassTests [t| K3        |] [''Bounded] )
            , testGroup "transformers"
                [ $( mkSpecializedClassTests [t| Boolean2Ring Bool   |] [''Ring] )
                ]
            ]
        ]
        -- FIXME: vector Arbitrary broken due to different sizes
        -- FIXME: vector identity is different than x-x, so spurious failures
--     , testGroup "vectors"
--         [ $( mkSpecializedClassTests [t| Vector Int |] [ ''Group, ''Ord, ''Lattice ] )
--         [ testGroup "metrics"
--             [ $( mkSpecializedClassTests [t| Vector Double |] [''MetricSpace ] )
--             , $( mkSpecializedClassTests [t| Polynomial 2 (Vector Double) |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| RBF 2 (Vector Double) |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| Sigmoid 2 (Vector Double) |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| Match                   Vector Double  |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| Xi2                     Vector Double  |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| HistogramIntersection   Vector Double  |] [''MetricSpace] )
--             , $( mkSpecializedClassTests [t| JensenShannonDivergence Vector Double  |] [''MetricSpace] )
--             ]
--         ]
    , testGroup "containers"
        [ $( mkSpecializedClassTests [t| []            Char |] [ ''FreeMonoid,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Array         Char |] [ ''FreeMonoid,''Partitionable ] )
        , $( mkSpecializedClassTests [t| UnboxedArray  Char |] [ ''FreeMonoid,''Partitionable ] )
        , $( mkSpecializedClassTests [t| StorableArray Char |] [ ''FreeMonoid,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Set           Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| Seq           Char |] [ ''FreeMonoid,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Map  Char Int |] [ ''POrd, ''Indexed ] )
        , $( mkSpecializedClassTests [t| Map' Char Int |] [ ''POrd, ''Indexed ] )
        , $( mkSpecializedClassTests [t| ByteString Lazy Char |] [ ''FreeMonoid, ''Partitionable ] )
        , testGroup "transformers"
            [ $( mkSpecializedClassTests [t| Lexical        [Char] |] [''Ord,''MinBound] )
            , $( mkSpecializedClassTests [t| ComponentWise  [Char] |] [''Lattice,''MinBound] )
            , $( mkSpecializedClassTests [t| Hamming        [Char] |] [''MetricSpace] )
            , $( mkSpecializedClassTests [t| Levenshtein    [Char] |] [''MetricSpace] )
            ]
            -- FIXME: loops on Eq tests for some reason
--         , testGroup "metric"
--             [ $( mkSpecializedClassTests [t| Box Int              |] [''Lattice] )
--             , $( mkSpecializedClassTests [t| Box (Hamming [Char]) |] [''Lattice,''Container] )
--             ]
        ]
    ]
