module Core (

    -- * Types
    Natural,
    Sequence,

    ) where


import Numeric.Natural


-- | A sequence is a function indexed by a natural number.
type Sequence = (->) Natural
