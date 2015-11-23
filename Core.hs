{-# LANGUAGE DeriveFunctor #-}


module Core (

    -- * Types
    Natural,
    Sequence,

    -- * Memoization
    memo,
    memoFix,

    ) where


import Numeric.Natural
import Prelude hiding (lookup)


-- | A sequence is a function indexed by a natural number.
type Sequence = (->) Natural


-- | A lazy, infinite binary tree which allows for @O(log n)@ indexing
-- at all points.
data Tree a = Branch a (Tree a) (Tree a)
    deriving Functor


-- | Look up a value in the tree.
lookup :: Natural -> Tree a -> a
lookup n (Branch value left right)
    | n == 0 = value
    | r == 0 = lookup q left
    | otherwise = lookup q right
  where
    (q, r) = pred n `quotRem` 2


-- | A tree containing all the natural numbers such that the value at
-- @n@ is precisely @n@.
naturals :: Tree Natural
naturals = Branch 0
    (succ . (2 *) <$> naturals)
    (succ . succ . (2 *) <$> naturals)


-- | Memoize a sequence function.
memo :: Sequence a -> Sequence a
memo f = \n -> lookup n tree
  where
    tree = f <$> naturals


-- | Memoize a recursive sequence function.
memoFix :: (Sequence a -> Sequence a) -> Sequence a
memoFix ff = f
  where
    f = memo (ff f)
