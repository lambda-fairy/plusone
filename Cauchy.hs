module Cauchy where


import Control.Applicative
import Data.List
import Numeric.Natural


newtype Cauchy = Cauchy (Natural -> Rational)


instance Num Cauchy where
    Cauchy x + Cauchy y = Cauchy (\n -> x (1+n) + y (1+n))
    Cauchy x - Cauchy y = Cauchy (\n -> x (1+n) - y (1+n))
    Cauchy x * Cauchy y = Cauchy (\n -> x (1+ky+n) * y (1+kx+n))
      where
        kx = log2 (abs (x 0) + 1)
        ky = log2 (abs (y 0) + 1)
    negate (Cauchy x) = Cauchy (negate . x)
    abs (Cauchy x) = Cauchy (abs . x)
    signum = error "signum is undecidable"
    fromInteger n = Cauchy (\_ -> fromInteger n)


-- | Cauchy sequence with a modulus of convergence.
--
-- The second argument is a /modulus function/, @mu :: N -> N@, which
-- for any @k@ gives the least index where the difference is less than
-- @2^(-mu k)@.
data Cauchy' = Cauchy' (Natural -> Rational) (Natural -> Natural)


instance Num Cauchy' where
    Cauchy' x u + Cauchy' y v = Cauchy'
        (liftA2 (+) x y)
        (\r -> max (u (1+r)) (v (1+r)))
    Cauchy' x u - Cauchy' y v = Cauchy'
        (liftA2 (-) x y)
        (\r -> max (u (1+r)) (v (1+r)))
    Cauchy' x u * Cauchy' y v = Cauchy'
        (liftA2 (*) x y)
        (\r -> max (1+ky+r) (1+kx+r))
      where
        kx = log2 (abs (x 0) + 1)
        ky = log2 (abs (y 0) + 1)
    negate (Cauchy' x u) = Cauchy' (negate . x) u
    abs (Cauchy' x u) = Cauchy' (abs . x) u
    signum = error "signum is undecidable"
    fromInteger n = Cauchy' (\_ -> fromInteger n) (\_ -> 0)


-- | Returns @max 0 (ceiling (logBase 2 x))@, where x is a rational number.
log2 :: Rational -> Natural
log2 x
    | x <= 0 = error "log2: negative argument"
    | otherwise = go x 0
  where
    go y acc
        | y <= 1 = acc
        | otherwise = acc `seq` go (y / 2) (1 + acc)


euler :: Cauchy
euler = Cauchy (\n -> sum (genericTake (1+n) (scanl (/) 1 [1 ..])))


class Index a where
    (#) :: a -> Natural -> Rational
infixl 9 #

instance Index Cauchy where
    Cauchy x # n = x n

instance Index Cauchy' where
    Cauchy' x u # n = x (u n)


toDouble :: Real a => a -> Double
toDouble = realToFrac
