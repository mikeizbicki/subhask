import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Prelude as P
import Control.Monad.Random
import Criterion.Main
import System.IO

import SubHask
import SubHask.Monad
import SubHask.Compatibility.Vector.Lebesgue

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

--------------------------------------------------------------------------------

main = do

    putStrLn "initializing variables"

    let veclen = 10
    xs1 <- P.fmap (P.take veclen) getRandoms
    xs2 <- P.fmap (P.take veclen) getRandoms
    xs3 <- P.fmap (P.take veclen) getRandoms

    let v1 = VG.fromList (xs1+xs2) :: L2 UnboxedVector Float
        v2 = VG.fromList (xs1+xs3) `asTypeOf` v1
        v3 = VG.fromList (xs1+xs2) `asTypeOf` v1
        v4 = VG.fromList (xs3+xs2) `asTypeOf` v1

    deepseq v1 $ deepseq v2 $ deepseq v2 $ return ()

    putStrLn $ "dist v1 v2="++ show (distance v1 v2)
    putStrLn $ "dist v1 v2="++ show (distance_l2_hask v1 v2)
    putStrLn $ "dist v1 v2="++ show (distance_l2_m128_unboxed v1 v2)

    putStrLn $ "dist v1 v2="++ show (isFartherThanWithDistanceCanError v1 v2 1000)
    putStrLn $ "dist v1 v2="++ show (isFartherThan_l2_hask v1 v2 1000)
    putStrLn $ "dist v1 v2="++ show (isFartherThan_l2_m128_unboxed v1 v2 1000)

    -----------------------------------
    putStrLn "launching criterion"

    defaultMain
        [ bgroup "distance"
            [ bgroup "L2"
                [ bench "isFartherThan" $ nf (isFartherThanWithDistanceCanError v1 v2) 1000
                , bench "isFartherThan_l2_hask" $ nf (isFartherThan_l2_hask v1 v2) 1000
                , bench "isFartherThan_l2_m128" $ nf (isFartherThan_l2_m128_unboxed v1 v2) 1000
                , bench "distance" $ nf (distance v1) v2
                , bench "distance_l2_hask" $ nf (distance_l2_hask v1) v2
                , bench "distance_l2_m128" $ nf (distance_l2_m128_unboxed v1) v2
                ]
            ]

--         , bgroup "fold"
--             [ bench "foldr" $ nf (VS.foldr' (+) VG.empty) (v1)
--             ]
--         , bgroup "eq"
--             [ bgroup "subhask"
--                 [ bench "v1v2" $ nf (v1==) v2
--                 , bench "v1v3" $ nf (v1==) v3
--                 , bench "v1v4" $ nf (v1==) v4
--                 ]
--             , bgroup "prelude"
--                 [ bench "v1v2" $ nf (v1 P.==) v2
--                 , bench "v1v3" $ nf (v1 P.==) v3
--                 , bench "v1v4" $ nf (v1 P.==) v4
--                 ]
--             ]
        ]
