module CalkinWilf (
    calkinWilf,
    calkinWilf',
    ) where


-- https://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf


calkinWilf :: [Rational]
calkinWilf = iterate next 1


next :: Rational -> Rational
next x = recip (fromInteger n + 1 - y)
  where
    (n, y) = properFraction x


calkinWilf' :: [Rational]
calkinWilf' = 0 : concatMap (\x -> [-x, x]) calkinWilf
