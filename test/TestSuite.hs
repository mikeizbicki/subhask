{-# LANGUAGE CPP #-}
import SubHask
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
            [ $( mkSpecializedClassTests [t| Integer  |] [''Ord,''Ring, ''Lattice] )
            , $( mkSpecializedClassTests [t| Int      |] [''Ord,''Ring, ''Bounded] )
            , $( mkSpecializedClassTests [t| Rational |] [''Ord,''Ring, ''Lattice] )
--             , $( mkSpecializedClassTests [t| Float    |] [''Ord,''Field, ''Bounded] )
--             , $( mkSpecializedClassTests [t| Double   |] [''Ord,''Field, ''Bounded] )
            ]
        , testGroup "other"
            [ $( mkSpecializedClassTests [t| Bool |] [''Ord,''Boolean] )
            , $( mkSpecializedClassTests [t| Char |] [''Ord,''Bounded] )
            ]
        ]
    , testGroup "containers"
        [ $( mkSpecializedClassTests [t| []            Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| Array         Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| StorableArray Char |] [ ''FreeMonoid ] )
        , $( mkSpecializedClassTests [t| UnboxedArray  Char |] [ ''FreeMonoid ] )
        , testGroup "transformers"
            [ $( mkSpecializedClassTests [t| Lexical [Char] |] [''POrd,''MinBound] )
            , $( mkSpecializedClassTests [t| Hamming [Char] |] [''MetricSpace] )
            ]
        ]
    ]
