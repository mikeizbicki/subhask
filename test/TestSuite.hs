import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude
import SubHask.Algebra.Group
import SubHask.Algebra.Container
import SubHask.Algebra.Logic
import SubHask.Algebra.Metric
import SubHask.Algebra.Parallel
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
            [ $( mkSpecializedClassTests [t| Int      |] [''Enum,''Ring, ''Bounded, ''Metric] )
            , $( mkSpecializedClassTests [t| Integer  |] [''Enum,''Ring, ''Lattice, ''Metric] )
            , $( mkSpecializedClassTests [t| Rational |] [''Ord,''Ring, ''Lattice, ''Metric] )
            , $( mkSpecializedClassTests [t| Float    |] [''Bounded] )
            , $( mkSpecializedClassTests [t| Double   |] [''Bounded] )
            , $( mkSpecializedClassTests [t| Uncompensated Int |] [ ''Ring ] )
            , testGroup "transformers"
                [ $( mkSpecializedClassTests [t| NonNegative Int  |] [''Enum,''Rig, ''Bounded, ''Metric] )
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
--             , testGroup "transformers"
--                 [ $( mkSpecializedClassTests [t| Boolean2Ring Bool   |] [''Ring] )
--                 ]
            ]
        ]
        -- FIXME: vector Arbitrary broken due to different sizes
        -- FIXME: vector identity is different than x-x, so spurious failures
--     , testGroup "vectors"
--         [ $( mkSpecializedClassTests [t| Vector Int |] [ ''Group, ''Ord, ''Lattice ] )
--         [ testGroup "metrics"
--             [ $( mkSpecializedClassTests [t| Vector Double |] [''Metric ] )
--             , $( mkSpecializedClassTests [t| Polynomial 2 (Vector Double) |] [''Metric] )
--             , $( mkSpecializedClassTests [t| RBF 2 (Vector Double) |] [''Metric] )
--             , $( mkSpecializedClassTests [t| Sigmoid 2 (Vector Double) |] [''Metric] )
--             , $( mkSpecializedClassTests [t| Match                   Vector Double  |] [''Metric] )
--             , $( mkSpecializedClassTests [t| Xi2                     Vector Double  |] [''Metric] )
--             , $( mkSpecializedClassTests [t| HistogramIntersection   Vector Double  |] [''Metric] )
--             , $( mkSpecializedClassTests [t| JensenShannonDivergence Vector Double  |] [''Metric] )
--             ]
--         ]
    , testGroup "containers"
        [ $( mkSpecializedClassTests [t| []            Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Array         Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| UnboxedArray  Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| StorableArray Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Set           Char |] [ ''Foldable,''MinBound ] )
        , $( mkSpecializedClassTests [t| Seq           Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Map  Int Int |] [ ''MinBound, ''IxConstructible ] )
        , $( mkSpecializedClassTests [t| Map' Int Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| IntMap  Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| IntMap' Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| ByteString Lazy Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , testGroup "transformers"
            [ $( mkSpecializedClassTests [t| Lexical        [Char] |] [''Ord,''MinBound] )
            , $( mkSpecializedClassTests [t| ComponentWise  [Char] |] [''Lattice,''MinBound] )
            , $( mkSpecializedClassTests [t| Hamming        [Char] |] [''Metric] )
            , $( mkSpecializedClassTests [t| Levenshtein    [Char] |] [''Metric] )
            ]
        , testGroup "metric"
            [ $( mkSpecializedClassTests [t| Box Int                    |] [''Eq,''Container] )
            , $( mkSpecializedClassTests [t| Box (ComponentWise [Char]) |] [''Eq,''Container] )
            ]
        ]
    ]
