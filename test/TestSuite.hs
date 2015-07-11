{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Main
    where

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Group
import SubHask.Algebra.Container
import SubHask.Algebra.Logic
import SubHask.Algebra.Metric
import SubHask.Algebra.Parallel
import SubHask.Algebra.Vector
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Containers

import SubHask.TemplateHaskell.Deriving
import SubHask.TemplateHaskell.Test

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Runners.Console
import Test.Framework.Runners.Options

--------------------------------------------------------------------------------

main = defaultMainWithOpts
    [ testGroup "simple"
        [ testGroup "numeric"
            [ $( mkSpecializedClassTests [t| Int      |] [''Enum,''Ring, ''Bounded, ''Metric] )
            , $( mkSpecializedClassTests [t| Integer  |] [''Enum,''Ring, ''Lattice, ''Metric] )
            , $( mkSpecializedClassTests [t| Rational |] [''Ord,''Ring, ''Lattice, ''Metric] )
            , $( mkSpecializedClassTests [t| Float    |] [''Bounded] )
            , $( mkSpecializedClassTests [t| Double   |] [''Bounded] )
            , testGroup "transformers"
                [ $( mkSpecializedClassTests [t| NonNegative Int  |] [''Enum,''Rig, ''Bounded, ''Metric] )
                , $( mkSpecializedClassTests [t| Z 57             |] [''Ring] )
                , $( mkSpecializedClassTests [t| NonNegative (Z 57) |] [''Rig] )
                ]
            ]
        , testGroup "vector"
            [ $( mkSpecializedClassTests [t| SVector 0     Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| SVector 1     Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| SVector 2     Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| SVector 19    Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| SVector 1001  Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| SVector "dyn" Int |] [ ''Module ] )
            , $( mkSpecializedClassTests [t| UVector "dyn" Int |] [ ''Module ] )
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
    , testGroup "objects"
        [ $( mkSpecializedClassTests [t| Labeled' Int Int |] [ ''Action,''Ord,''Metric ] )
        ]
    , testGroup "arrays"
        [ $( mkSpecializedClassTests [t| BArray        Char |] [ ''Foldable,''MinBound,''IxContainer ] )
        , $( mkSpecializedClassTests [t| UArray        Char |] [ ''Foldable,''MinBound,''IxContainer ] )
        , $( mkSpecializedClassTests [t| UArray (UVector "dyn" Float) |] [ ''Foldable,''IxContainer ] )
        , $( mkSpecializedClassTests [t| UArray (Labeled' (UVector "dyn" Float) Int) |] [ ''Foldable,''IxContainer ] )
        ]
    , testGroup "containers"
        [ $( mkSpecializedClassTests [t| []            Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Set           Char |] [ ''Foldable,''MinBound ] )
        , $( mkSpecializedClassTests [t| Seq           Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , $( mkSpecializedClassTests [t| Map  Int Int |] [ ''MinBound, ''IxConstructible ] )
        , $( mkSpecializedClassTests [t| Map' Int Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| IntMap  Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| IntMap' Int |] [ ''MinBound, ''IxContainer ] )
        , $( mkSpecializedClassTests [t| ByteString Char |] [ ''Foldable,''MinBound,''Partitionable ] )
        , testGroup "transformers"
            [ $( mkSpecializedClassTests [t| Lexical        [Char] |] [''Ord,''MinBound] )
            , $( mkSpecializedClassTests [t| ComponentWise  [Char] |] [''Lattice,''MinBound] )
            , $( mkSpecializedClassTests [t| Hamming        [Char] |] [''Metric] )
            , $( mkSpecializedClassTests [t| Levenshtein    [Char] |] [''Metric] )
            ]
        , testGroup "metric"
--             [ $( mkSpecializedClassTests [t| Ball Int                    |] [''Eq,''Container] )
--             , $( mkSpecializedClassTests [t| Ball (Hamming [Char])       |] [''Eq,''Container] )
            [ $( mkSpecializedClassTests [t| Box Int                     |] [''Eq,''Container] )
            , $( mkSpecializedClassTests [t| Box (ComponentWise [Char])  |] [''Eq,''Container] )
            ]
        ]
    ]
    $ RunnerOptions
        { ropt_threads          = Nothing
        , ropt_test_options     = Nothing
        , ropt_test_patterns    = Nothing
        , ropt_xml_output       = Nothing
        , ropt_xml_nested       = Nothing
        , ropt_color_mode       = Just ColorAlways
        , ropt_hide_successes   = Just True
        , ropt_list_only        = Just True
        }

--------------------------------------------------------------------------------
-- orphan instances needed for compilation

instance (Show a, Show b) => Show (a -> b) where
    show _ = "function"
