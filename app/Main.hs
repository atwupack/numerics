module Main where

import Numerics.Integration
import Criterion.Main

main :: IO ()
main = do
    print $ trapezoid ( \x -> x**2 ) (-1.0, 1.0) 0.000001
    print $ trapezoid' ( \x -> x**2 ) (-1.0, 1.0) 0.000001
    print $ trapezoid'' ( \x -> x**2 ) (-1.0, 1.0) 0.000001
    defaultMain  [ bgroup "integration"  [ bench "trapezoid1" $ whnf (trapezoid ( \x -> x**2 ) (-1.0, 1.0)) 0.0000000001
                                            , bench "trapezoid2" $ whnf (trapezoid ( \x -> x**2 ) (-1.0, 1.0)) 0.000001
                                            , bench "trapezoid'1" $ whnf (trapezoid' ( \x -> x**2 ) (-1.0, 1.0)) 0.0000000001
                                            , bench "trapezoid'2" $ whnf (trapezoid' ( \x -> x**2 ) (-1.0, 1.0)) 0.000001
                                            , bench "trapezoid''1" $ whnf (trapezoid'' ( \x -> x**2 ) (-1.0, 1.0)) 0.0000000001
                                            , bench "trapezoid''2" $ whnf (trapezoid'' ( \x -> x**2 ) (-1.0, 1.0)) 0.000001
                                            ]
                    ]
