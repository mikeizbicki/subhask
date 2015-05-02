{-# LANGUAGE DataKinds #-}

import qualified Prelude as P
import Control.Monad.Random
import Criterion.Main
import Criterion.Types
import System.IO

import Data.Reflection

import SubHask
import SubHask.Monad
import SubHask.Compatibility.StaticVector

--------------------------------------------------------------------------------

main = do

    -----------------------------------
    putStrLn "initializing variables"

    let veclen = 100
    xs1 <- P.fmap (P.take veclen) getRandoms
    xs2 <- P.fmap (P.take veclen) getRandoms
    xs3 <- P.fmap (P.take veclen) getRandoms

    let s1 = unsafeToModule (xs1+xs2) :: Vector 200 Float
        s2 = unsafeToModule (xs1+xs3) `asTypeOf` s1

        d1 = unsafeToModule (xs1+xs2) :: DynVector n Float
        d2 = unsafeToModule (xs1+xs3) `asTypeOf` d1

    deepseq s1 $ deepseq s2 $ return ()

    -----------------------------------
    putStrLn "launching criterion"

    defaultMainWith
        ( defaultConfig
            { verbosity = Normal
            -- when run using `cabal bench`, this will put our results in the right location
            , csvFile = Just "bench/Vector.csv"
            }
        )
--         [ bgroup "+"
--             [ bench "static"  $ nf (s1+) s2
--             , bench "dynamic" $ nf (d1+) d2
--             ]
        [ bgroup "distance"
            [ bench "static"  $ nf (distance s1) s2
            , bench "dynamic" $ nf (distance d1) d2
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
