module Fuzzy.Control.Defuzzification where

import Fuzzy.Sets.LSet (LSet, toList)
import Fuzzy.Sets.Cardinality
import Lattices.ResiduatedLattice (ResiduatedLattice)
import FuzzySet
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Defuzzify a fuzzy set using the center of gravity (centroid) method.
-- 
-- Returns 0 for an empty universe.
centerOfGravity ::  ResiduatedLattice l => LSet Double l -> Double
centerOfGravity  = centerOfGravityMod id

-- | Defuzzify with a membership modifier function 'c'.
--
-- The modifier is applied to each membership value before computing the weighted mean.
-- Returns 0 when the modified sigma count is 0.
centerOfGravityMod :: ResiduatedLattice l => (l -> l) -> LSet Double l -> Double
centerOfGravityMod c set = if card == 0 then 0 else numer / card
    where 
        pairs = toList set
        card = sigmaCountMod c set
        numer = sum [x * realToFrac mem | (x, mem) <- pairs]

-- | Defuzzify by taking the midpoint of the maximum interval.
--
-- For a non-empty set returns (max + min) / 2 based on the universe support.
centerOfMaxima :: ResiduatedLattice l => LSet Double l -> Double
centerOfMaxima set = (sup + inf) / 2 
    where 
        sup = maximum $ universe set
        inf = minimum $ universe set

-- | Mean of maxima with a modifier function.
--
-- Uses 'sigmaCountMod' for total modified membership and normalizes by universe size.
-- Returns 0 for empty universes.
meanOfMaximaMod :: ResiduatedLattice l => (l -> l) -> LSet Double l -> Double
meanOfMaximaMod c set = if sizeUniverse == 0 then 0 else card / sizeUniverse
    where 
        card = sigmaCountMod c set
        sizeUniverse = fromIntegral $ universeCardinality set

-- | Return the universe element with maximal membership degree.
--
-- If multiple elements have the same maximal degree, the first encountered is returned.
-- For an empty set, returns 0.
maxMembership :: ResiduatedLattice l => LSet Double l -> Double
maxMembership set
    | null pairs = 0
    | otherwise  = fst $ maximumBy (comparing snd) pairs
    where
        pairs = toList set