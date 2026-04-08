module Fuzzy.Sets.FuzzyCardinality(
    fgCount,
    feCount,
    flCount,
    ralescuF,
    bracket
) where

import FuzzySet
import Lattices.ResiduatedLattice
import Fuzzy.Sets.LSet (toList)
import Data.List (sort, sortBy)


bracket :: (FuzzySet set a l) => Int -> set -> l
bracket k set
    | null alphas = 0
    | otherwise   = maximum alphas
    where
        alphas = [alpha | alpha <- truthDegrees set, length (alphaCut alpha set) >= k]


fgCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
fgCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = if k == 0 then 1 else bracket k set


flCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
flCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k            = negation $ bracket (k + 1) set


feCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
feCount set  = intersection (flCount set) (fgCount set)

ralescuF :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
ralescuF set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k  = (degs !! k) /\ negation (degs !! (k + 1))
        degs = 1 : sortBy (flip compare) (truthDegrees set) ++ [0]