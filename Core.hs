module Core where


import Numeric.Natural


newtype Cauchy = Cauchy (Natural -> Rational)


instance Num Cauchy where
    Cauchy f + Cauchy g = Cauchy (\n -> f (1+n) + g (1+n))
    Cauchy f - Cauchy g = Cauchy (\n -> f (1+n) - g (1+n))
    (*) = error "(*) not implemented"
    negate (Cauchy f) = Cauchy (\n -> negate (f n))
    abs (Cauchy f) = Cauchy (\n -> abs (f n))
    signum (Cauchy f) = Cauchy (\n -> signum (f n))
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
    signum (Cauchy' f u) = Cauchy' (signum f) u
    fromInteger n = Cauchy' (fromInteger n) (\_ -> 0)
