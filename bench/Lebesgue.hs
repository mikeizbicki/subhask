{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The purpose of this file is to test different implementations of the L2 distance to determine which is faster.
--

import Control.Monad.Random
import qualified Data.Vector.Generic as VG
import System.IO

import Criterion.Main

import SubHask
import SubHask.Compatibility.Vector.Lebesgue

-------------------------------------------------------------------------------
-- main

main = do

    let veclen = 200

    -----------------------------------
    -- initialize single vectors

    putStrLn "constructing single vectors"

    let v1f :: Vector Float = evalRand (VG.replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        v2f :: Vector Float = evalRand (VG.replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)
    deepseq v1f $ deepseq v2f $ return ()

    let v1d :: Vector Float = evalRand (VG.replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        v2d :: Vector Float = evalRand (VG.replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)
    deepseq v1d $ deepseq v2d $ return ()

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    defaultMain
        [ bgroup "L2"
            [ bench "distance" $ nf (distance (L2 v1f)) (L2 v2f)
            ]
        ]

