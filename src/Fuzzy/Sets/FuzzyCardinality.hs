-- | Fuzzy cardinality models that return fuzzy sets over possible counts.
module Fuzzy.Sets.FuzzyCardinality(
    fgCount,
    feCount,
    flCount,
    ralescuF,
    bracket,
    generalizedFGCount,
    generalizedFLCount,
    generalizedFECount
) where

import FuzzySet
import Lattices.ResiduatedLattice
import Data.List (sortBy)

-- | Return the greatest alpha level whose alpha-cut contains at least @k@ elements.
--
-- This helper is used by the fuzzy counting operators to turn a crisp count
-- condition back into a truth degree.
bracket :: (FuzzySet set a l) => Int -> set -> l
bracket k set
    | null alphas = 0
    | otherwise   = maximum alphas
    where
        alphas = [alpha | alpha <- truthDegrees set, length (alphaCut alpha set) >= k]

-- | Fuzzy "greater-or-equal" cardinality.
--
-- The resulting fuzzy set assigns to each count @k@ the degree to which the
-- original set can be said to contain at least @k@ elements.
fgCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
fgCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = if k == 0 then 1 else bracket k set


generalizedFGCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
generalizedFGCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = if k == 0 then 1 else foldr (tnorm . (`bracket` set)) top [1..k]


-- | Fuzzy "less-or-equal" cardinality.
--
-- Each count @k@ receives the degree to which the original set contains at
-- most @k@ elements.
flCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
flCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = negation $ bracket (k + 1) set


-- | Generalized fuzzy "less-or-equal" cardinality.
--
-- This dual form accumulates negated higher-count brackets using the lattice
-- t-norm.
generalizedFLCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
generalizedFLCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = foldr (tnorm . negation . (`bracket` set)) top [1 .. universeCardinality set]


-- | Fuzzy "exactly" cardinality.
--
-- This is the intersection of 'flCount' and 'fgCount', so it captures the
-- degree to which the set has exactly @k@ elements.
feCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
feCount set  = intersection (flCount set) (fgCount set)


-- | Generalized fuzzy "exactly" cardinality.
--
-- This is the intersection of 'generalizedFLCount' and
-- 'generalizedFGCount'.
generalizedFECount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
generalizedFECount set = intersection (generalizedFLCount set) (generalizedFGCount set)


-- | Ralescu's fuzzy cardinality distribution.
--
-- The membership of each count is computed from consecutive truth degrees
-- sorted in descending order.
ralescuF :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
ralescuF set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k  = (degs !! k) /\ negation (degs !! (k + 1))
        degs = 1 : sortBy (flip compare) (truthDegrees set) ++ [0]
