-- | Cardinality measures and modifier functions for fuzzy sets.
module Fuzzy.Sets.Cardinality(
    sigmaCount,
    thresholdSigmaCount,
    normalizedSigmaCount,
    sigmaCountMod,
    -- * Modifier functions
    modifierFunction,
    sigmoidModifier,
    identityModifier,
    subDiagonalModifier,
    alphaCutModifier,
    ralescuS
) where

import Lattices.ResiduatedLattice
import Fuzzy.Sets.LSet
import FuzzySet
import Data.List


{- | Most commonly used way to tell the size of a fuzzy set.
The sigma count is the sum of membership values of all elements in the universe set.
For fuzzy set A, |A| = sum A(u) for all u in U.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.5)] :: LSet Int UILukasiewicz
>>> sigmaCount set
1.4

>>> let emptySet = mkEmptySet :: LSet Int UILukasiewicz
>>> sigmaCount emptySet
0.0
-}
sigmaCount :: (FuzzySet set a l) => set -> Double
sigmaCount set = sum [realToFrac (f x) | x <- universe set]
    where f = member set


{- | Similar to 'sigmaCount', but applies a modifier function @c@ to each membership value before summing.
For a fuzzy set A, |A| = sum c(A(u)) for all u in U.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.5)] :: LSet Int UILukasiewicz
>>> let modifier = sigmoidModifier 2.0 0.5
>>> sigmaCountMod modifier set
1.4
-}
sigmaCountMod :: (FuzzySet set a l) => (l -> l) -> set -> Double
sigmaCountMod c set = sum [realToFrac $ (c . f) x | x <- universe set]
    where f = member set


{- | Sigma count with a threshold applied.
Only membership values greater than or equal to the threshold are summed.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.5)] :: LSet Int UILukasiewicz
>>> thresholdSigmaCount 0.5 set
1.2

>>> thresholdSigmaCount 0.8 set
0.0
-}
thresholdSigmaCount :: (FuzzySet set a l) => l -> set -> Double
thresholdSigmaCount threshold set =
    sum [realToFrac (f x) | x <- universe set, f x >= threshold]
    where f = member set


{- | Normalized sigma count is like the standard sigma count, but the value is normalized to be in the interval @[0,1]@.

==== __Examples__

>>> let set = fromList [(1, 0.2), (2, 0.7), (3, 0.5)] :: LSet Int UILukasiewicz
>>> normalizedSigmaCount set
0.4666666666666667

>>> let emptySet = mkEmptySet :: LSet Int UILukasiewicz
>>> normalizedSigmaCount emptySet
0.0
-}
normalizedSigmaCount :: (FuzzySet set a l) => set -> l
normalizedSigmaCount set = mkLattice $ sigmaCount set / fromIntegral (length $ universe set)


{- | A general modifier function that can be used to create specific modifier functions.
The parameters @p@, @r@, and @threshold@ control the behaviour of the modifier.

==== __Examples__

>>> let modifier = modifierFunction 2 2 0.5 :: UILukasiewicz -> UILukasiewicz
>>> modifier 0.3
0.18

>>> modifier 0.7
0.82
-}
modifierFunction :: (ResiduatedLattice l) => Double -> Double -> Double -> (l -> l)
modifierFunction p r threshold a
    | realToFrac a < threshold  = mkLattice $ threshold ** (1 - p) * (realToFrac a ** p)
    | realToFrac a >= threshold = mkLattice $ 1 - (1 - threshold) ** (1 - r) * (1 - realToFrac a) ** r


{- | A sigmoid modifier function, a specific case of 'modifierFunction' where @p = r@.

==== __Examples__

>>> let modifier = sigmoidModifier 2 0.5 :: UILukasiewicz -> UILukasiewicz
>>> modifier 0.3
0.36

>>> modifier 0.7
0.84
-}
sigmoidModifier :: (ResiduatedLattice l) => Double -> Double -> (l -> l)
sigmoidModifier p = modifierFunction p p


{- | Identity modifier function, which leaves the membership values unchanged.

==== __Examples__

>>> let modifier = identityModifier :: UILukasiewicz -> UILukasiewicz
>>> modifier 0.3
0.3

>>> modifier 0.7
0.7
-}
identityModifier :: (ResiduatedLattice l) => (l -> l)
identityModifier = modifierFunction 1 1 1


{- | Sub-diagonal modifier function, a specific case of 'modifierFunction' where @r = 1@.

==== __Examples__

>>> let modifier = subDiagonalModifier 2 :: UILukasiewicz -> UILukasiewicz
>>> modifier 0.3
9.0e-2

>>> modifier 0.7
0.489999999999
-}
subDiagonalModifier :: (ResiduatedLattice l) => Double -> (l -> l)
subDiagonalModifier p = modifierFunction p 1 1


{- | Alpha-cut modifier function, which sets membership values below the
threshold to 'bot' and values above the threshold to 'top'.

==== __Examples__

>>> let modifier = alphaCutModifier 0.5 :: UILukasiewicz -> UILukasiewicz
>>> modifier 0.3
0.0

>>> modifier 0.7
1.0
-}
alphaCutModifier :: (ResiduatedLattice l) => Double -> (l -> l)
alphaCutModifier threshold a
    | realToFrac a <= threshold = bot
    | realToFrac a > threshold  = top

{- $modifier functions
Modifier functions give us a way to shift sigma count in cases where needed.
A common use is reducing the effect of many small accumulated membership values.
-}

-- | Ralescu's crisp cardinality estimate derived from sorted truth degrees.
--
-- The result is the number of membership degrees strictly above @0.5@, with
-- the midpoint counted in the conventional way used by Ralescu's measure.
ralescuS :: (FuzzySet set a l) => set -> Int
ralescuS set
    | 0.5 `elem` degs = length [deg | deg <- degs, deg > 0.5] + 1
    | otherwise       = length [deg | deg <- degs, deg > 0.5]
    where
        degs = 1 : sort (truthDegrees set) ++ [0]
