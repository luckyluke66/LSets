module Fuzzy.Sets.FuzzyCardinality where

import FuzzySet
import Lattices.ResiduatedLattice


bracket :: (FuzzySet set a l) => Int -> set -> l
bracket k set 
    | null alphas = 0 
    | otherwise = maximum alphas  
    where 
        alphas = [alpha | alpha <- truthDegrees set, length (alphaCut alpha set) >= k ]


fgCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
fgCount set = mkFuzzySet f universeCounts 
    where
        universeCounts = [0 .. universeCardinality set]
        f k = bracket k set


flCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
flCount set = mkFuzzySet f universeCounts
    where
        universeCounts = [0 .. universeCardinality set]
        f k = negation $ bracket (k + 1) set

feCount :: (FuzzySet set a l, FuzzySet countSet Int l) => set -> countSet
feCount set  = intersection (flCount set) (fgCount set)