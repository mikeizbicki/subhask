{-# LANGUAGE DataKinds,KindSignatures #-}

import qualified Prelude as P
import Control.Monad.Random
import Criterion.Main
import Criterion.Types
import System.IO

import SubHask
import SubHask.Algebra.Vector
import SubHask.Algebra.Vector.FFI
import SubHask.Monad

--------------------------------------------------------------------------------

main = do

    -----------------------------------
    putStrLn "initializing variables"

    let veclen = 1000
    xs1 <- P.fmap (P.take veclen) getRandoms
    xs2 <- P.fmap (P.take veclen) getRandoms
    xs3 <- P.fmap (P.take veclen) getRandoms

    let s1 = unsafeToModule (xs1+xs2) :: SVector 2000 Float
        s2 = unsafeToModule (xs1+xs3) `asTypeOf` s1

        d1 = unsafeToModule (xs1+xs2) :: SVector "dynamic" Float
        d2 = unsafeToModule (xs1+xs3) `asTypeOf` d1

        u1 = unsafeToModule (xs1+xs2) :: UVector "dynamic" Float
        u2 = unsafeToModule (xs1+xs3) `asTypeOf` u1

    let ub14 = distance s1 s2 * 1/4
        ub34 = distance s1 s2 * 3/4

    deepseq s1 $ deepseq s2 $ return ()

    putStrLn $ "distance s1 s2 = " + show (distance s1 s2)
    putStrLn $ "distance d1 d2 = " + show (distance d1 d2)
    putStrLn $ "distance u1 u2 = " + show (distance u1 u2)
    putStrLn ""
    putStrLn $ "distanceUB s1 s2 1 = " + show (distanceUB s1 s2 1)
    putStrLn $ "distanceUB d1 d2 1 = " + show (distanceUB d1 d2 1)
    putStrLn $ "distanceUB u1 u2 1 = " + show (distanceUB u1 u2 1)
    putStrLn ""

    -----------------------------------
    putStrLn "launching criterion"

--     defaultMainWith
--         ( defaultConfig
--             { verbosity = Normal
--             -- when run using `cabal bench`, this will put our results in the right location
--             , csvFile = Just "bench/Vector.csv"
--             }
--         )
    defaultMain
--         [ bgroup "+"
--             [ bench "static"  $ nf (s1+) s2
--             , bench "dynamic" $ nf (d1+) d2
--             , bench "unboxed" $ nf (u1+) u2
--             ]
        [ bgroup "distance"
            [ bench "static"  $ nf (distance s1) s2
            , bench "dynamic" $ nf (distance d1) d2
            , bench "unboxed" $ nf (distance u1) u2
            ]
        , bgroup "distanceUB - bound (1/4)"
            [ bench "static"  $ nf (distanceUB s1 s2) ub14
            , bench "dynamic" $ nf (distanceUB d1 d2) ub14
            , bench "unboxed" $ nf (distanceUB u1 u2) ub14
            ]
        , bgroup "distanceUB - bound (3/4)"
            [ bench "static"  $ nf (distanceUB s1 s2) ub34
            , bench "dynamic" $ nf (distanceUB d1 d2) ub34
            , bench "unboxed" $ nf (distanceUB u1 u2) ub34
            ]
        , bgroup "distanceUB - bound infinity"
            [ bench "static"  $ nf (distanceUB s1 s2) infinity
            , bench "dynamic" $ nf (distanceUB d1 d2) infinity
            , bench "unboxed" $ nf (distanceUB u1 u2) infinity
            ]
--         [ bgroup "size"
--             [ bench "static"  $ nf size s1
--             , bench "dynamic" $ nf size d2
--             ]
        ]
--             , bench "-" $ nf ((-) s1) s2
--             , bench ".*." $ nf ((.*.) s1) s2
--             , bench "./." $ nf ((./.) s1) s2
--             , bench "negate" $ nf negate s2
--             , bench ".*" $ nf (.*5) s2
--             , bench "./" $ nf (./5) s2
--             [ bench "distance"                  $ nf (distance s1) s2
--                 , bench "distance_Vector4_Float"    $ nf (distance_Vector4_Float s1) s2

