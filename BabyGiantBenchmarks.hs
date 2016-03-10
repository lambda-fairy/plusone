{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies #-}

-- Baby-step giant-step algorithm benchmarks
--
-- Run them like so:
-- $ cabal install criterion
-- $ ghc BabyGiantBenchmarks.hs
-- $ ./BabyGiantBenchmarks --csv babygiant.csv


import Criterion.Main
import Data.Proxy

import NumberTheory


-- 11 is a generator of (Z/499)*
sample :: Integer -> [Maybe Integer]
sample q = [babyGiant' 11 h q | h :: Mod 499 <- [1 ..]]


main = defaultMain [
    bgroup "babyGiant"
        [bench (show q) (nf sample q) | q <- [2, 4 .. 40]]
    ]
