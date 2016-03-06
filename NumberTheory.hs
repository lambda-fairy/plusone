{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module NumberTheory where


import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Ratio
import GHC.TypeLits


-- | A modulus is a natural number not less than 2.
type Modulus n = (KnownNat n, 2 <= n)


-- | Integers modulo N.
newtype Mod (n :: Nat) = Mod Integer
    deriving (Eq, Ord)

instance forall n. Modulus n => Read (Mod n) where
    readsPrec p s = [(Mod a, s') | (a, s') <- readsPrec p s, 0 <= a && a < n]
      where
        n = natVal (Proxy :: Proxy n)

instance forall n. Show (Mod n) where
    showsPrec p (Mod n) = showsPrec p n

instance forall n. Modulus n => Num (Mod n) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    negate = lift1 negate
    abs = id
    signum = const 1
    fromInteger a = Mod $ mod a n
      where
        n = natVal (Proxy :: Proxy n)

instance forall n. Modulus n => Real (Mod n) where
    toRational = toRational . toInteger

instance forall n. Modulus n => Integral (Mod n) where
    quotRem (Mod a) (Mod b) = (fromInteger q, fromInteger r)
      where
        (q, r) = quotRem a b
    toInteger (Mod a) = a

instance forall n. Modulus n => Fractional (Mod n) where
    recip (Mod a)
        | g /= 1 = error $ "recip: " ++ show a ++ " has no inverse mod " ++ show n
        | otherwise = fromInteger x
      where
        (g, x, _y) = egcd a n
        n = natVal (Proxy :: Proxy n)
    fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance forall n. Modulus n => Bounded (Mod n) where
    minBound = Mod 0
    maxBound = Mod $ natVal (Proxy :: Proxy n) - 1

instance forall n. Modulus n => Enum (Mod n) where
    toEnum a
        | 0 <= a && a' < n = Mod $ toInteger a'
        | otherwise = error "toEnum: out of range"
      where
        a' = toInteger a
        n = natVal (Proxy :: Proxy n)
    fromEnum = fromEnum . toInteger
    enumFrom x = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y bound
      where
        bound
            | y >= x = maxBound
            | otherwise = minBound

lift1 :: Modulus n => (Integer -> Integer) -> Mod n -> Mod n
lift1 f (Mod a) = fromInteger $ f a

lift2 :: Modulus n => (Integer -> Integer -> Integer) -> Mod n -> Mod n -> Mod n
lift2 f (Mod a) (Mod b) = fromInteger $ f a b


-- | Extended Euclidean algorithm.
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = (g, t - div b a * s, s)
  where
    (g, s, t) = egcd (mod b a) a


-- | Baby-step giant-step algorithm on integers mod N.
--
-- @babyGiant g h@ finds the integer @m@ where @g ^ m == h@.
babyGiant :: forall n. Modulus n => Mod n -> Mod n -> Maybe Integer
babyGiant g h = babyGiant' g h (intSqrt n)
  where
    n = natVal (Proxy :: Proxy n)


-- | Like @babyGiant@, but with an explicit number of baby steps.
babyGiant' :: forall n. Modulus n => Mod n -> Mod n -> Integer -> Maybe Integer
babyGiant' g h k = headMay [i * k + j |
    i <- [1 .. ceiling (n % k)],
    j <- toList $ Map.lookup (h / g^(i * k)) baby]
  where
    baby = Map.fromList [(g^j, j) | j <- [0 .. k]]
    n = natVal (Proxy :: Proxy n)


-- | Find the order of @a@ in the multiplicative group mod N.
--
-- Returns @Nothing@ when @a@ is not in the multiplicative group.
order :: forall n. Modulus n => Mod n -> Maybe Integer
order a = headMay [i | i <- [1 .. n], a ^ i == 1]
  where
    n = natVal (Proxy :: Proxy n)


-- | Determine if @a@ is a generator, i.e. its order is @N - 1@.
isGenerator :: forall n. Modulus n => Mod n -> Bool
isGenerator a = order a == Just (n - 1)
  where
    n = natVal (Proxy :: Proxy n)


-- | Integer square root.
intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . realToFrac


-- | Find the left-most element of a container.
headMay :: Foldable f => f a -> Maybe a
headMay = foldr (\a _ -> Just a) Nothing
