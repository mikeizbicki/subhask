{-# LANGUAGE CPP #-}
import SubHask
import SubHask.Algebra.Objects
import SubHask.Algebra.Trans.StringMetrics

import SubHask.Test
import Language.Haskell.TH

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

main = defaultMain
    [ testGroup "simple"
        [ testGroup "numeric"
            [ $( mkSpecializedClassTests [t| Int      |] [''Ord,''Ring, ''Bounded] )
            , $( mkSpecializedClassTests [t| Integer  |] [''Ord,''Ring, ''Lattice] )
            , $( mkSpecializedClassTests [t| Rational |] [''Ord,''Ring, ''Lattice] )
--             , $( mkSpecializedClassTests [t| Float    |] [''Ord,''Field, ''Bounded] )
--             , $( mkSpecializedClassTests [t| Double   |] [''Ord,''Field, ''Bounded] )
            ]
        , testGroup "non-numeric"
            [ $( mkSpecializedClassTests [t| Bool      |] [''Ord,''Boolean] )
            , $( mkSpecializedClassTests [t| Char      |] [''Ord,''Bounded] )
            , $( mkSpecializedClassTests [t| Ordering  |] [''Monoid])
            , $( mkSpecializedClassTests [t| POrdering |] [''Monoid])
            , testGroup "transformers"
                [ $( mkSpecializedClassTests [t| BooleanRing Bool |] [''Ring] )
                , $( mkSpecializedClassTests [t| Z 2              |] [''Ring] )
                ]
            ]
        ]
--     | FIXME: vector Arbitrary broken due to different sizes
--     , testGroup "vectors"
--         [ $( mkSpecializedClassTests [t| Vector Int |] [ ''Group, ''Ord, ''Lattice ] )
--         ]
    , testGroup "containers"
        [ $( mkSpecializedClassTests [t| []            Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| Array         Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| UnboxedArray  Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| StorableArray Char |] [ ''FreeMonoid ] )
        , testGroup "transformers"
            [ $( mkSpecializedClassTests [t| Lexical [Char] |] [''Ord,''MinBound] )
            , $( mkSpecializedClassTests [t| Hamming [Char] |] [''MetricSpace] )
            ]
        ]
    ]
