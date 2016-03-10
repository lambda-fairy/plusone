module Cauchy where


import Data.List
import Numeric.Natural


newtype Cauchy = Cauchy (Natural -> Rational)


instance Num Cauchy where
    Cauchy f + Cauchy g = Cauchy (\n -> f (1+n) + g (1+n))
    Cauchy f - Cauchy g = Cauchy (\n -> f (1+n) - g (1+n))
    Cauchy f * Cauchy g = Cauchy (\n -> f (1+kg+n) * g (1+kf+n))
      where
        kf = log2 (abs (f 0) + 1)
        kg = log2 (abs (g 0) + 1)
    negate (Cauchy f) = Cauchy (\n -> negate (f n))
    abs (Cauchy f) = Cauchy (\n -> abs (f n))
    signum = error "signum is undecidable"
    fromInteger n = Cauchy (\_ -> fromInteger n)


-- | Cauchy sequence with a modulus of convergence.
--
-- The second argument is a /modulus function/, @mu :: N -> N@, which
-- for any @k@ gives the least index where the difference is less than
-- @2^(-mu k)@.
data Cauchy' = Cauchy' Cauchy (Natural -> Natural)


instance Num Cauchy' where
    Cauchy' f u + Cauchy' g v = Cauchy' (f + g) (\k -> max (u (1+k)) (v (1+k)))
    Cauchy' f u - Cauchy' g v = Cauchy' (f - g) (\k -> max (u (1+k)) (v (1+k)))
    (*) = error "(*) not implemented"
    negate (Cauchy' f u) = Cauchy' (negate f) u
    abs (Cauchy' f u) = Cauchy' (abs f) u
    signum = error "signum is undecidable"
    fromInteger n = Cauchy' (fromInteger n) (\_ -> 0)


-- | Returns @max 0 (ceiling (logBase 2 x))@, where x is a rational number.
log2 :: Rational -> Natural
log2 x
    | x <= 0 = error "log2: negative argument"
    | otherwise = go x 0
  where
    go y acc
        | y <= 1 = acc
        | otherwise = acc `seq` go (y / 2) (1 + acc)


exp' :: Rational -> Cauchy
exp' = Cauchy (\n -> sum (genericTake (1+n) (scanl (/) 1 [1 ..])))
