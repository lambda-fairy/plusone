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
