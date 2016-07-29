{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cauchy where


import Control.Applicative
import Data.Ratio
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


instance Fractional Cauchy where
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
    recip x'@(Cauchy x) = Cauchy (\n -> recip $ x (n + 2*c))
      where
        c = findNonZero x'


instance Show Cauchy where
    showsPrec p x = showsPrec p (toDouble $ x # 64) . ("..." ++)


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
        (\r -> max (u (1+ky+r)) (v (1+kx+r)))
      where
        kx = log2 (abs (x 0) + 1)
        ky = log2 (abs (y 0) + 1)
    negate (Cauchy' x u) = Cauchy' (negate . x) u
    abs (Cauchy' x u) = Cauchy' (abs . x) u
    signum = error "signum is undecidable"
    fromInteger n = Cauchy' (\_ -> fromInteger n) (\_ -> 0)


instance Fractional Cauchy' where
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
    recip x'@(Cauchy' x u) = Cauchy' (recip . x) (\r -> u (r + 2*c))
      where
        c = findNonZero x'


-- | Returns @max 0 (ceiling (logBase 2 x))@, where x is a rational number.
log2 :: Rational -> Natural
log2 x
    | x <= 0 = error "log2: negative argument"
    | otherwise = go x 0
  where
    go y acc
        | y <= 1 = acc
        | otherwise = acc `seq` go (y / 2) (1 + acc)


-- | Given an @x@ which does not converge to zero, find the lowest index
-- @c@ where @abs (x # (c + k)) > 1 / 2^c@ for every @k@.
findNonZero :: Index a => a -> Natural
findNonZero x = go 0
  where
    go n
        | e <= 0 = go $! n + 1
        | otherwise = log2 (recip e)
      where
        e = abs (x # n) - (1 % 2^n)


-- | Find the exponential of a rational number.
expC :: Rational -> Cauchy
expC x
    | -1 <= x && x <= 1 = exp01 x
    | otherwise = let y = expC (x / 2) in y * y
  where
    exp01 x = Cauchy $ \n -> sum [ x^k / realToFrac (factorial k) | k <- [0 .. n+1] ]


-- | Like @exp@, but returns a @Cauchy'@ instead.
expC' :: Rational -> Cauchy'
expC' x
    | -1 <= x && x <= 1 = exp01 x
    | otherwise = let y = expC' (x / 2) in y * y
  where
    exp01 x = Cauchy' (\n -> sum [ x^k / realToFrac (factorial k) | k <- [0 .. n] ]) (\r -> r+1)


-- | Find the sine of a rational number.
sinC :: Rational -> Cauchy
sinC x
    | -1 <= x && x <= 1 = sin01 x
    | otherwise = 2 * sinC (x / 2) * cosC (x / 2)
  where
    sin01 x = Cauchy $ \n -> sum [
        (-1)^(k `quot` 2) * x^k / realToFrac (factorial k) |
        k <- [1, 3 .. n+1] ]


-- | Find the cosine of a rational number.
cosC :: Rational -> Cauchy
cosC x
    | -1 <= x && x <= 1 = cos01 x
    | otherwise = let c = cosC (x / 2) in 2 * c * c - 1
  where
    cos01 x = Cauchy $ \n -> sum [
        (-1)^(k `quot` 2) * x^k / realToFrac (factorial k) |
        k <- [0, 2 .. n+1] ]


-- | Find the tangent of a rational number.
tanC :: Rational -> Cauchy
tanC x = sinC x / cosC x


-- | Super simple factorial function.
factorial :: Natural -> Natural
factorial n = product [2..n]


-- | Given a sequence of numbers, find a number that is apart from all
-- of them.
cantor :: (Natural -> Cauchy) -> Cauchy
cantor xs = Cauchy $ fst . step
  where
    step :: Natural -> (Rational, Rational)
    step 0 = (0, 1)
    step n
        -- Since we're zooming in by a factor of 1/4 each time,
        -- we only have to refine the approximation every two steps
        | even n = (a, b)
        -- If the number is in the right half, move to the left
        | xn >= (a + b) / 2 = (a, a + offset)
        -- If the number is in the left half, move to the right
        | otherwise = (b - offset, b)
      where
        (a, b) = step (n - 1)
        offset = (b - a) / 4
        xn = xs (n `quot` 2) # (n + 1)


class Index a where
    (#) :: a -> Natural -> Rational
infixl 1 #

instance Index Cauchy where
    Cauchy x # n = x n

instance Index Cauchy' where
    Cauchy' x u # n = x (u n)


toDouble :: Real a => a -> Double
toDouble = realToFrac
