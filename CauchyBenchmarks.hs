-- Cauchy sequence benchmarks
--
-- Run them like so:
-- $ cabal install criterion
-- $ ghc CauchyBenchmarks.hs
-- $ ./CauchyBenchmarks -o cauchy-benchmarks.html


{-# LANGUAGE ScopedTypeVariables #-}


import Criterion.Main
import Data.List
import Data.Proxy
import Numeric.Natural

import Cauchy


main = defaultMain [
    bgroup "cauchy" [
        bench "single-function" (nf (test (Proxy :: Proxy Cauchy)) 50),
        bench "double-function" (nf (test (Proxy :: Proxy Cauchy')) 50)
        ]
    ]


-- test n = nth approximation of (1! + 2! + ... + 100!)
test :: (Num a, Index a) => Proxy a -> Natural -> Rational
test p n = (x `asProxyTypeOf` p) # n
  where
    x = sum . map product . inits $ map fromInteger [1 .. 100]
