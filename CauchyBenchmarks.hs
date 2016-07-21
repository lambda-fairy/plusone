-- Cauchy sequence benchmarks
--
-- Run them like so:
-- $ cabal install criterion
-- $ ghc CauchyBenchmarks.hs
-- $ ./CauchyBenchmarks -o cauchy-benchmarks.html


{-# LANGUAGE RankNTypes #-}


import Criterion.Main
import Data.List
import Data.Proxy
import Numeric.Natural

import Cauchy


class (Fractional a, Index a) => CauchyImpl a where
    euler :: a

instance CauchyImpl Cauchy where
    euler = expC 1

instance CauchyImpl Cauchy' where
    euler = expC' 1


main :: IO ()
main = defaultMain [
    benchmark "simple" $ (euler * euler + euler) / euler,
    benchmark "complicated" $ sum [fromInteger n * euler | n <- [1 .. 50]]
    ]
  where
    benchmark :: String -> (forall a. CauchyImpl a => a) -> Benchmark
    benchmark label expr = bgroup label [
        bench "single-function" (nf (# 50) (expr :: Cauchy)),
        bench "double-function" (nf (# 50) (expr :: Cauchy'))
        ]
